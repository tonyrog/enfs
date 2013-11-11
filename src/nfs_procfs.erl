%%%----------------------------------------------------------------------
%%% File    : nfs_procfs.erl
%%% Author  : Luke Gorrie <luke@bluetail.com>
%%% Purpose : 
%%% Created : 22 Jun 2001 by Luke Gorrie <luke@bluetail.com>
%%%----------------------------------------------------------------------

-module(nfs_procfs).
-author('luke@bluetail.com').

-export([start_link/0]).

-behaviour(nfs_server).

-export([init/1,
	 terminate/1,
	 getattr/1,
	 setattr/2,
	 lookup/2, 
	 readlink/1,
	 read/4, 
	 write/5, 
	 create/3,
	 remove/2,
	 rename/4,
	 link/3,
	 symlink/4,
	 mkdir/3,
	 rmdir/2,
	 readdir/2,
	 statfs/1]).

-record(procfs_state,
	{
	  options = [],
	  root = root
	}).

start_link() ->
    Pid = case nfs_server:start_link() of
	      {ok,Pid0} -> Pid0;
	      {error,{already_started,Pid0}} -> Pid0
	  end,
    ok = nfs_server:add_mountpoint("/procfs", ?MODULE, []),
    {ok, Pid}.

%% Returns: ID of root directory, any erlang term.
init(Options) ->
    {root, #procfs_state{ options=Options}}.

terminate(#procfs_state{}) ->
    ok.

%% return (partial) attribute list
getattr(root) ->
    {ok, [{type,directory},{mode,{rx,rx,rx}}]};
getattr({node, _N}) ->
    {ok, [{type,directory},{mode,{rx,rx,rx}}]};
getattr({pid, _P}) ->
    {ok, [{type,directory},{mode,{rx,rx,rx}}]};
getattr({registered, _N, _P}) ->
    {ok,[{type,directory},{mode,{rx,rx,rx}}]};
getattr(ID) ->
    Time = timestamp(),
    Size = filesize(ID),
    {ok, [{type,regular},{mode,{r,r,r}},
	  {size, Size},
	  {ctime, Time}, {mtime, Time}, {atime, Time}]}.

setattr(_ID, _Attrs) ->
    {error, eacces}.

readdir(root, _Count) ->
    {ok, [atom_to_list(Node) || Node <- [node()] ++ nodes()]};
readdir({node, Node}, _Count) ->
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
readdir({registered, Node, R}, _Count) ->
    case rpc:call(Node, erlang, whereis, [R]) of
	undefined ->
	    {error, enoent};
	{badrpc, _} ->
	    {error, eio};
	Pid when is_pid(Pid) ->
	    readdir({pid, Pid}, 8192)
    end;
readdir({pid, P}, _Count) ->
    case rpc:call(node(P), erlang, process_info, [P]) of
	{badrpc, _} ->
	    {error, eio};
	Info ->
	    {ok, [atom_to_list(A) || {A, _} <- Info]}
    end.

%% simple mac os x test
lookup(root, ".metadata_never_index") ->
    {ok, {property, metadata_never_index, value}};
lookup(root, Child) ->
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
lookup({node, Node}, Child) ->
    %% Child could be either a pid or a registered name
    case catch list_to_pid("<"++Child++">") of
	{'EXIT', _} ->
	    %% Not a pid - registered name
	    {ok, {registered, Node, list_to_atom(Child)}};
	Pid ->
	    {ok, {pid, Pid}}
    end;
lookup({registered, Node, R}, Child) ->
    {ok, {property, {registered, Node, R}, list_to_atom(Child)}};
lookup({pid, P}, Child) ->
    {ok, {property, {pid, P}, list_to_atom(Child)}}.

readlink(_Name) ->
    {error, eacces}.

read({property, metadata_never_index, value},_Offset,_Count,_TotalCount) ->
    {ok, "1"};
read({property, P, Name},_Offset,_Count,_TotalCount) ->
    case get_process_info(P, Name) of
	undefined ->
	    {error, enoent};
	{_, Value} ->
	    {ok, io_lib:format("~p~n", [Value])}
    end.

write(_ID,_BeginOffset,_Offset,_TotalCount,_Data) ->
    {error, eacces}.

create(_DirID, _Name, _Attr) ->
    {error, eacces}.

remove(_DirID, _NAme) ->    
    {error, eacces}.

rename(_DirFrom, _NameFrom, _DirTo, _NameTo) ->    
    {error, eacces}.

link(_FromID, _ToID, _ToName) ->
    {error, eacces}.

symlink(_FromID, _FromName, _ToName, _SAttr) ->
    {error, eacces}.

mkdir(_DirID, _Name, _Attr) ->
    {error, eacces}.

rmdir(_DirID, _Name) ->
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

filesize(ID = {property, _P, _Name}) ->
    case read(ID,0,8192,8192) of
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

statfs(_ID) ->
    %% 65535? is a bita strange
    {ok, {1024, 1024, 1024, 0, 0}}.	% pulled out of the air

