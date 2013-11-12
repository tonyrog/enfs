%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2013, Tony Rogvall
%%% @doc
%%%    erlang file backend to nfs server 
%%% @end
%%% Created : 11 Nov 2013 by Tony Rogvall <tony@rogvall.se>

-module(nfs_filefs).


-export([start_link/0, start_link/1]).

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
	  root = "/tmp"
	}).

-define(UNDEF32, 16#ffffffff).

start_link() ->
    start_link([filename:join(os:getenv("HOME"), "tmp")]).

start_link([RootArg]) ->
    Pid = case nfs_server:start_link() of
	      {ok,Pid0} -> Pid0;
	      {error,{already_started,Pid0}} -> Pid0
	  end,
    Root = if is_atom(RootArg) -> atom_to_list(RootArg);
	      is_list(RootArg) -> RootArg;
	      is_binary(RootArg) -> binary_to_list(RootArg)
	   end,
    ok = nfs_server:add_mountpoint("/filefs", ?MODULE, [{root,Root}]),
    {ok, Pid}.

init(Options) ->
    Root = proplists:get_value(root, Options, "/tmp"),
    {Root, #filefs_state { options=Options, root=Root }}.

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

setattr(Filename, _SAttrs={Mode,Uid,Gid,Size,Atime,Mtime}, _St) ->
    FI = #file_info {
	    mode = aval(Mode),
	    uid = aval(Uid),
	    gid = aval(Gid),
	    size = aval(Size),  %% truncate/fill?
	    atime = tval(Atime),
	    mtime = tval(Mtime)
	   },
    file:write_file_info(Filename, FI).

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
read(FileName, Offset, Count, _TotalCount, _St) ->
    case file:open(FileName, [read,binary]) of
	{ok,Fd} ->
	    Res = file:pread(Fd, Offset, Count),
	    file:close(Fd),
	    Res;
	Error ->
	    Error
    end.
    
%% fixme: cache open files for a while?
write(FileName,_BeginOffset,Offset,_TotalCount,Data, _St) ->
    case file:open(FileName, [read,write,binary]) of
	{ok,Fd} ->
	    Res = file:pwrite(Fd, Offset, Data),
	    file:close(Fd),
	    Res;
	Error ->
	    Error
    end.

create(Dir, File, SAttr, _St) ->
    Filename = filename:join(Dir,File),
    case file:open(Filename, [read,write,binary]) of
	{ok,Fd} ->
	    file:close(Fd),
	    case setattr(Filename, SAttr,_St) of
		ok -> {ok, Filename};
		Error -> Error
	    end;
	Error ->
	    Error
    end.

remove(Dir, File, _St) ->    
    FileName = filename:join(Dir,File),
    file:delete(FileName).


rename(DirFrom, NameFrom, DirTo, NameTo, _St) ->
    file:rename(filename:join(DirFrom,NameFrom),
		filename:join(DirTo, NameTo)).

link(FromFileName, ToDir, ToName, _St) ->
    file:make_link(FromFileName,filename:join(ToDir,ToName)).

symlink(FromDir, FromName, Existing, SAttr, _St) ->
    New = filename:join(FromDir,FromName),
    case file:make_symlink(Existing, New) of
	ok -> setattr(New, SAttr,_St);
	Error -> Error
    end.

mkdir(ParentName, Name, SAttr, _St) ->
    DirName = filename:join(ParentName, Name),
    case file:make_dir(DirName) of
	ok -> 
	    case setattr(DirName, SAttr,_St) of
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
