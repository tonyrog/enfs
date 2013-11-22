%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2013, Tony Rogvall
%%% @doc
%%%    erlang file backend to nfs server 
%%% @end
%%% Created : 11 Nov 2013 by Tony Rogvall <tony@rogvall.se>

-module(nfs_filefs).

-export([command/1, start/1]).

-behaviour(nfs_server).

-export([init/1,
	 terminate/1,
	 getattr/2,
	 setattr/3,
	 lookup/3, 
	 readlink/2,
	 read/5, 
	 write/6, 
	 create/4,
	 remove/3,
	 rename/5,
	 link/4,
	 symlink/5,
	 mkdir/4,
	 rmdir/3,
	 readdir/3,
	 statfs/2]).

-include_lib("kernel/include/file.hrl").

-record(filefs_state,
	{
	  options = [],
	  %% simulate slow read/write (like 4,8 ms for a spinning disk)
	  read_delay = 0,    %% ms to sleep in read operation
	  write_delay = 0,   %% ms to sleep in write operation
	  root
	}).

-define(UNDEF32, 16#ffffffff).

command([RootArg]) ->
    Root = to_list(RootArg),
    start([{root,Root}]);
command([RootArg,WriteDelay,ReadDelay]) ->
    Root = to_list(RootArg),
    WrDelay = to_integer(WriteDelay),
    RdDelay = to_integer(ReadDelay),
    start([{root,Root},{write_delay,WrDelay},
	   {read_delay,RdDelay}]).

start(Options) ->
    application:start(enfs),
    ok = nfs_server:add_mountpoint("/filefs", ?MODULE, Options).

to_list(X) when is_atom(X) -> atom_to_list(X);
to_list(X) when is_binary(X) -> binary_to_list(X);
to_list(X) when is_list(X) -> X.

to_integer(X) when is_atom(X) -> list_to_integer(atom_to_list(X));
to_integer(X) when is_list(X) -> list_to_integer(X);
to_integer(X) when is_binary(X) -> binary_to_integer(X).

init(Options) ->
    WriteDelay = proplists:get_value(write_delay, Options, 0),
    ReadDelay = proplists:get_value(read_delay, Options, 0),
    Root = proplists:get_value(root, Options, "/tmp"),
    {Root, #filefs_state { options=Options, root=Root,
			   write_delay = WriteDelay,
			   read_delay  = ReadDelay }}.

terminate(#filefs_state {}) ->   
    ok.

%% return (partial) attribute list
getattr(File, _St) ->
    case file:read_link_info(File) of
	{ok, FI} ->
	    {ok, 
	     [{size,FI#file_info.size},
	      {type,FI#file_info.type},
	      %% {access,case FI#file.access}
	      {atime, datetime_to_nfstime(FI#file_info.atime)},
	      {mtime, datetime_to_nfstime(FI#file_info.mtime)},
	      {ctime, datetime_to_nfstime(FI#file_info.ctime)},
	      {mode,FI#file_info.mode},
	      {nlink,FI#file_info.links},
	      {uid, FI#file_info.uid},
	      {gid, FI#file_info.gid}]};
	Error ->
	    Error
    end.

setattr(Filename, SAttrs, St) ->
    setattr_(undefined, Filename, SAttrs, St).

setattr_(Fd, Filename, _SAttrs={Mode,Uid,Gid,Size,Atime,Mtime}, St) ->
    Size1 = aval(Size),
    set_file_size(Fd, Filename, Size1, St),
    FI = #file_info {
	    mode = aval(Mode),
	    uid = aval(Uid),
	    gid = aval(Gid),
	    size = Size1,
	    atime = tval(Atime),
	    mtime = tval(Mtime)
	   },
    file:write_file_info(Filename, FI).

set_file_size(_Fd, _Filename, undefined, _St) ->
    ok;
set_file_size(undefined, Filename, Size, St) ->
    case file:open(Filename, [read,write]) of
	{ok,Fd} ->
	    Res = set_file_size(Fd, Filename, Size, St),
	    file:close(Fd),
	    Res;
	Error ->
	    Error
    end;
set_file_size(Fd, _Filename, Size, _St) ->
    %% St may be used later to access file cache
    case file:position(Fd, Size) of
	{ok,_Pos} ->
	    file:truncate(Fd);
	Error -> Error
    end.


tval({?UNDEF32,?UNDEF32}) -> undefined;
tval({Sec,USec}) -> 
    Now = {Sec div 1000000,Sec rem 1000000,USec},
    calendar:now_to_datetime(Now).

aval(?UNDEF32) -> undefined;
aval(Value) -> Value.
    

readdir(DirName, _Count, _St) ->
    file:list_dir(DirName).

lookup(Dir, File, _St) ->
    FileName = filename:join(Dir, File),
    case file:read_link_info(FileName) of
	{ok,_} -> {ok, FileName};
	Error -> Error
    end.
	
readlink(LinkName, _St) ->
    file:read_link(LinkName).

%% fixme: cache open files for a while?
read(FileName, Offset, Count, _TotalCount, St) ->
    case file:open(FileName, [read,binary]) of
	{ok,Fd} ->
	    Res = file:pread(Fd, Offset, Count),
	    file:close(Fd),
	    timer:sleep(St#filefs_state.read_delay),
	    Res;
	Error ->
	    Error
    end.
    
%% fixme: cache open files for a while?
write(FileName,_BeginOffset,Offset,_TotalCount,Data, St) ->
    case file:open(FileName, [read,write,binary]) of
	{ok,Fd} ->
	    Res = file:pwrite(Fd, Offset, Data),
	    file:close(Fd),
	    timer:sleep(St#filefs_state.write_delay),
	    Res;
	Error ->
	    Error
    end.

create(Dir, File, SAttr, St) ->
    Filename = filename:join(Dir,File),
    case file:read_file_info(Filename) of
	{error, enoent} ->
	    case file:open(Filename, [write,binary]) of
		{ok,Fd} ->
		    R = setattr_(Fd, Filename, SAttr, St),
		    file:close(Fd),
		    case R of
			ok -> {ok, Filename};
			Error -> Error
		    end;
		Error ->
		    Error
	    end;
	{ok,_Fi} ->
	    {error,exist}
    end.

remove(Dir, File, _St) ->    
    FileName = filename:join(Dir,File),
    file:delete(FileName).


rename(DirFrom, NameFrom, DirTo, NameTo, _St) ->
    file:rename(filename:join(DirFrom,NameFrom),
		filename:join(DirTo, NameTo)).

link(FromFileName, ToDir, ToName, _St) ->
    file:make_link(FromFileName,filename:join(ToDir,ToName)).

symlink(FromDir, FromName, Existing, SAttr, St) ->
    New = filename:join(FromDir,FromName),
    case file:make_symlink(Existing, New) of
	ok -> setattr_(undefined, New, SAttr, St);
	Error -> Error
    end.

mkdir(ParentName, Name, SAttr, St) ->
    DirName = filename:join(ParentName, Name),
    case file:make_dir(DirName) of
	ok -> 
	    case setattr_(undefined, DirName, SAttr, St) of
		ok -> {ok, DirName};
		Error -> Error
	    end;
	Error -> Error
    end.
	    
rmdir(DirName, Name, _St) ->
    file:del_dir(filename:join(DirName, Name)).

%% FIXME:
statfs(_ID,_St) ->
    {ok, {1024, 1024, 1024, 0, 0}}.

datetime_to_unix_seconds(DateTime) ->
    Secs = calendar:datetime_to_gregorian_seconds(DateTime),
    Secs - 62167219200.

datetime_to_nfstime(undefined) -> {0,0};
datetime_to_nfstime(DateTime) ->
    case datetime_to_unix_seconds(DateTime) of
	?UNDEF32 -> {?UNDEF32, ?UNDEF32};
	USec -> {USec, 0}
    end.
