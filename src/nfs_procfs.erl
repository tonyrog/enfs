%%%----------------------------------------------------------------------
%%% File    : nfs_procfs.erl
%%% Author  : Luke Gorrie <luke@bluetail.com>
%%% Purpose : 
%%% Created : 22 Jun 2001 by Luke Gorrie <luke@bluetail.com>
%%%----------------------------------------------------------------------

-module(nfs_procfs).
-author('luke@bluetail.com').

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

-record(procfs_state,
	{
	  options = [],
	  root = root
	}).

command([]) ->
    start([]).

start(Options) ->
    application:start(enfs),
    ok = nfs_server:add_mountpoint("/procfs", ?MODULE, Options).

%% Returns: ID of root directory, any erlang term.
init(Options) ->
    {root, #procfs_state{ options=Options } }.

terminate(#procfs_state{}) ->
    ok.

%% return (partial) attribute list
getattr(root, _St) ->
    {ok, [{type,directory},{mode,{[r,x],[r,x],[r,x]}}]};
getattr({node, _N}, _St) ->
    {ok, [{type,directory},{mode,{[r,x],[r,x],[r,x]}}]};
getattr({pid, _P}, _St) ->
    {ok, [{type,directory},{mode,{[r,x],[r,x],[r,x]}}]};
getattr({registered, _N, _P}, _St) ->
    {ok,[{type,directory},{mode,{[r,x],[r,x],[r,x]}}]};
getattr(ID, _St) ->
    Time = timestamp(),
    Size = filesize(ID,_St),
    {ok, [{type,regular},{mode,{[r],[r],[r]}},
	  {size, Size},
	  {ctime, Time}, {mtime, Time}, {atime, Time}]}.

setattr(_ID, _Attrs, _St) ->
    {error, eacces}.

readdir(root, _Count, _St) ->
    {ok, [atom_to_list(Node) || Node <- [node()] ++ nodes()]};
readdir({node, Node}, _Count, _St) ->
    case rpc:call(Node, erlang, processes, []) of
	{badrpc, _} ->
	    {error, eio};
	Procs ->
	    case rpc:call(Node, erlang, registered, []) of
		{badrpc, _} ->
		    {error, eio};
		Regs ->
		    ProcNms = [erlang:pid_to_list(P) -- "<>" || P <- Procs],
		    Registered = [atom_to_list(R) || R <- Regs],
		    {ok, ProcNms ++ Registered}
	    end
    end;
readdir({registered, Node, R}, _Count, _St) ->
    case rpc:call(Node, erlang, whereis, [R]) of
	undefined ->
	    {error, enoent};
	{badrpc, _} ->
	    {error, eio};
	Pid when is_pid(Pid) ->
	    readdir({pid, Pid}, 8192, _St)
    end;
readdir({pid, P}, _Count, _St) ->
    case rpc:call(node(P), erlang, process_info, [P]) of
	{badrpc, _} ->
	    {error, eio};
	Info ->
	    {ok, [atom_to_list(A) || {A, _} <- Info]}
    end.

%% simple mac os x test
lookup(root, ".metadata_never_index", _St) ->
    {ok, {property, metadata_never_index, value}};
lookup(root, Child, _St) ->
    %% Child is a node name
    NodeName = atom_to_list(node()),
    if Child == NodeName ->
	    {ok, {node, node()}};
       true ->
	    Node = list_to_atom(Child),
	    case net_adm:ping(Node) of
		pang ->
		    {error, enoent};
		pong ->
		    {ok, {node,Node}}
	    end
    end;
lookup({node, Node}, Child, _St) ->
    %% Child could be either a pid or a registered name
    case catch list_to_pid("<"++Child++">") of
	{'EXIT', _} ->
	    %% Not a pid - registered name
	    {ok, {registered, Node, list_to_atom(Child)}};
	Pid ->
	    {ok, {pid, Pid}}
    end;
lookup({registered, Node, R}, Child, _St) ->
    {ok, {property, {registered, Node, R}, list_to_atom(Child)}};
lookup({pid, P}, Child, _St) ->
    {ok, {property, {pid, P}, list_to_atom(Child)}}.

readlink(_Name, _St) ->
    {error, eacces}.

read({property, metadata_never_index, value},_Offset,_Count,_TotalCount, _St) ->
    {ok, "1"};
read({property, P, Name},_Offset,_Count,_TotalCount, _St) ->
    case get_process_info(P, Name) of
	undefined ->
	    {error, enoent};
	{_, Value} ->
	    {ok, io_lib:format("~p~n", [Value])}
    end.

write(_ID,_BeginOffset,_Offset,_TotalCount,_Data, _St) ->
    {error, eacces}.

create(_DirID, _Name, _Attr, _St) ->
    {error, eacces}.

remove(_DirID, _NAme, _St) ->
    {error, eacces}.

rename(_DirFrom, _NameFrom, _DirTo, _NameTo, _St) ->    
    {error, eacces}.

link(_FromID, _ToID, _ToName, _St) ->
    {error, eacces}.

symlink(_FromID, _FromName, _ToName, _SAttr, _St) ->
    {error, eacces}.

mkdir(_DirID, _Name, _Attr, _St) ->
    {error, eacces}.

rmdir(_DirID, _Name, _St) ->
    {error, eacces}.    


get_process_info({pid, P}, Name) ->
    case rpc:call(node(P), erlang, process_info, [P, Name]) of
	{badrpc, _} ->
	    undefined;
	Info ->
	    Info
    end;
get_process_info({registered, Node, R}, Name) ->
    case rpc:call(Node, erlang, whereis, [R]) of
	undefined ->
	    undefined;
	{badrpc, _} ->
	    undefined;
	Pid ->
	    get_process_info({pid, Pid}, Name)
    end.

filesize(ID = {property, _P, _Name},_St) ->
    case read(ID,0,8192,8192,_St) of
	{ok, IOList} ->
	    erlang:iolist_size(IOList);
	_ ->
	    0
    end.

timestamp() ->
    {Mega, Sec, Micro} = os:timestamp(),
    {((Mega * 1000000) + Sec) band 16#ffffffff, Micro}.

%% Callback: statfs(ID) -> {ok, {Tsize, Bsize, Blocks, Bfree, Bavail}} |
%%                         {error, Reason}
%% Return values:
%%       Tsize   The optimum transfer size of the server in bytes.  This is
%%               the number of bytes the server would like to have in the
%%               data part of READ and WRITE requests.
%%       Bsize   The block size in bytes of the filesystem.
%%       Blocks  The total number of "bsize" blocks on the filesystem.
%%       Bfree   The number of free "bsize" blocks on the filesystem.
%%       Bavail  The number of "bsize" blocks available to non-privileged
%%               users.

statfs(_ID, _St) ->
    %% 65535? is a bita strange
    {ok, {1024, 1024, 1024, 0, 0}}.	% pulled out of the air

