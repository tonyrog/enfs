%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2013, Tony Rogvall
%%% @doc
%%%    erlang ssh_sftp backend to nfs server 
%%% @end
%%% Created : 11 Nov 2013 by Tony Rogvall <tony@rogvall.se>

-module(nfs_sshfs).

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

-record(sshfs_state,
	{
	  cm,             %% connection manager pid
	  chan,           %% sftp channel pid
	  options = [],   %% backend options
	  root = "/tmp"   %% remote root 
	}).

-define(UNDEF32, 16#ffffffff).

%% from command line
command([HostArg,RootArg]) ->
    Root = to_list(RootArg),
    Host = to_list(HostArg),    
    start([{root,Root},{host,Host}]).

start(Options) ->
    application:start(crypto),
    application:start(asn1),
    application:start(public_key),
    application:start(ssh),
    application:start(enfs),
    ok = nfs_server:add_mountpoint("/sshfs", ?MODULE, Options).


to_list(X) when is_atom(X) -> atom_to_list(X);
to_list(X) when is_binary(X) -> binary_to_list(X);
to_list(X) when is_list(X) -> X.

init(Options) ->
    Host = proplists:get_value(host, Options),
    Port = proplists:get_value(port, Options, 22),
    Timeout = proplists:get_value(timeout, Options, 4000),
    Root = proplists:get_value(root, Options, "/tmp"),
    %% fixme: how can we run with ipv6 enabled?
    SSHOptions = proplists:get_value(ssh_options, Options, []),
    case ssh:connect(Host,Port,[{ipv6_disabled,true}|SSHOptions],Timeout) of
	{ok, Cm} ->
	    case ssh_sftp:start_channel(Cm, SSHOptions) of
		{ok, Chan} ->
		    {Root, #sshfs_state { options=Options, 
					  cm=Cm,
					  chan=Chan,
					  root=Root }};
		Error -> 
		    Error
	    end;
	Error ->
	    Error
    end.

terminate(#sshfs_state { cm=Cm, chan=Chan }) ->   
    ssh_sftp:stop_channel(Chan),
    ssh:close(Cm),
    ok.

%% return (partial) attribute list
getattr(File, St) ->
    case ssh_sftp:read_link_info(St#sshfs_state.chan, File) of
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

setattr(Filename, _SAttrs={Mode,Uid,Gid,Size,Atime,Mtime},St) ->
    FI = #file_info {
	    mode = aval(Mode),
	    uid = aval(Uid),
	    gid = aval(Gid),
	    size = aval(Size),  %% truncate/fill?
	    atime = tval(Atime),
	    mtime = tval(Mtime)
	   },
    ssh_sftp:write_file_info(St#sshfs_state.chan,Filename, FI).

tval({?UNDEF32,?UNDEF32}) -> undefined;
tval({Sec,USec}) -> 
    Now = {Sec div 1000000,Sec rem 1000000,USec},
    calendar:now_to_datetime(Now).

aval(?UNDEF32) -> undefined;
aval(Value) -> Value.
    
readdir(DirName, _Count, St) ->
    ssh_sftp:list_dir(St#sshfs_state.chan,DirName).

%% simple mac os x test
lookup(Dir, File, St) ->
    FileName = filename:join(Dir, File),
    case ssh_sftp:read_link_info(St#sshfs_state.chan,FileName) of
	{ok,_} -> {ok, FileName};
	Error -> Error
    end.
	
readlink(LinkName, St) ->
    ssh_sftp:read_link(St#sshfs_state.chan,LinkName).

%% fixme: cache open files for a while?
read(FileName, Offset, Count, _TotalCount,St) ->
    case ssh_sftp:open(St#sshfs_state.chan,FileName,[read,binary]) of
	{ok,Handle} ->
	    Res = ssh_sftp:pread(St#sshfs_state.chan,Handle,Offset,Count),
	    ssh_sftp:close(St#sshfs_state.chan,Handle),
	    Res;
	Error ->
	    Error
    end.
    
%% fixme: cache open files for a while?
write(FileName,_BeginOffset,Offset,_TotalCount,Data,St) ->
    case ssh_sftp:open(St#sshfs_state.chan,FileName, [write,binary]) of
	{ok,Handle} ->
	    Res = ssh_sftp:pwrite(St#sshfs_state.chan,Handle,Offset,Data),
	    ssh_sftp:close(St#sshfs_state.chan,Handle),
	    Res;
	Error ->
	    Error
    end.

create(Dir,File, SAttr,St) ->
    Filename = filename:join(Dir,File),
    case ssh_sftp:read_file_info(St#sshfs_state.chan,Filename) of
	{error,no_such_file} ->
	    case ssh_sftp:open(St#sshfs_state.chan,Filename, [write]) of
		{ok,Handle} ->
		    ssh_sftp:close(St#sshfs_state.chan,Handle),
		    case setattr(Filename,SAttr,St) of
			ok -> {ok, Filename};
			Error -> Error
		    end;
		Error ->
		    Error
	    end;
	{ok,_FI} -> {error,exist};
	Error -> Error
    end.

remove(Dir,File,St) ->    
    FileName = filename:join(Dir,File),
    ssh_sftp:delete(St#sshfs_state.chan,FileName).


rename(DirFrom,NameFrom,DirTo,NameTo,St) ->
    ssh_sftp:rename(St#sshfs_state.chan,
		    filename:join(DirFrom,NameFrom),
		    filename:join(DirTo, NameTo)).

link(FromFileName, ToDir, ToName,St) ->
    ssh_sftp:make_link(St#sshfs_state.chan,
		       FromFileName,filename:join(ToDir,ToName)).

symlink(FromDir, FromName, Existing, SAttr,St) ->
    New = filename:join(FromDir,FromName),
    case ssh_sftp:make_symlink(St#sshfs_state.chan,Existing, New) of
	ok -> setattr(New,SAttr,St);
	Error -> Error
    end.

mkdir(ParentName, Name, SAttr,St) ->
    DirName = filename:join(ParentName, Name),
    case ssh_sftp:make_dir(St#sshfs_state.chan,DirName) of
	ok -> 
	    case setattr(DirName, SAttr,St) of
		ok -> {ok, DirName};
		Error -> Error
	    end;
	Error -> Error
    end.
	    
rmdir(DirName, Name,St) ->
    ssh_sftp:del_dir(St#sshfs_state.chan,filename:join(DirName, Name)).

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
