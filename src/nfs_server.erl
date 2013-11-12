%%%----------------------------------------------------------------------
%%% File    : nfs_server.erl
%%% Author  : Luke Gorrie <luke@bluetail.com>
%%% Purpose : Extensible NFS v2 (RFC 1094) server core
%%% Created : 22 Jun 2001 by Luke Gorrie <luke@bluetail.com>
%%%----------------------------------------------------------------------

-module(nfs_server).
-author('luke@bluetail.com').

-behaviour(gen_server).

-include("nfs.hrl").

%% External exports
-export([start_link/0, add_mountpoint/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
	 terminate/2, code_change/3]).

-export([behaviour_info/1]).

-define(dbg(F,A), io:format("~s:~w "++(F)++"\n",[?MODULE,?LINE|(A)])).
%% -define(dbg(F,A), ok).

-define(MOUNTD_PORT, 22050).		% arbitrary
-define(NFS_PORT, 22049).		% normal port + 20000

%% NFS identifies files by a "file handle", which is a fixed-length
%% opaque binary. This program's file handles look like this:
%%
%%   <<FileID:32, FilesystemID:32, _Junk/binary>>
%%
%% We have bi-directional mappings to identify which erlang module
%% implements each file system, and which term represents each file
%% id.

%% These tables are mappings. fh_id_tab maps file handles onto
%% identifying terms, etc.
-define(fh_id_tab, nfs_fh_id).         %% FileID   => FH
-define(id_fh_tab, nfs_id_fh).         %% FH       => FileID
-define(fsid_mod_tab, nfs_fsid_mod).   %% FSID     => Module
-define(misc_tab,     nfs_misc).       %% counters next_fsid, {next_fileid,FSID}

%% fattr modes
-define(MODE_DIR,     8#0040000).
-define(MODE_CHAR,    8#0020000).
-define(MODE_BLOCK,   8#0060000).
-define(MODE_REGULAR, 8#0100000).
-define(MODE_SYMLINK, 8#0120000).
-define(MODE_SOCKET,  8#0140000).
-define(MODE_SETUID,  8#0004000).
-define(MODE_SETGID,  8#0002000).
-define(MODE_SV_SWAP, 8#0001000).	% "Save swapped text even after use."
-define(MODE_UR,      8#0000400).
-define(MODE_UW,      8#0000200).
-define(MODE_UX,      8#0000100).
-define(MODE_GR,      8#0000040).
-define(MODE_GW,      8#0000020).
-define(MODE_GX,      8#0000010).
-define(MODE_OR,      8#0000004).
-define(MODE_OW,      8#0000002).
-define(MODE_OX,      8#0000001).

-record(mount_ent,
	{
	  path,   %% mount path
	  mod,    %% backend mod
	  opts,   %% backend options
	  root,   %% root file handle
	  fsid    %% current fsid
	}).

-record(state, {
	  fh_suffix,        %% filehandle suffix in use
	  mountpoints = [] :: [#mount_ent{}],
	  locals :: dict()  %% dict: fsid -> localstate()
	 }).

-spec behaviour_info(Arg::callbacks) -> 
			    list({FunctionName::atom(), Arity::integer()}).
behaviour_info(callbacks) ->
    [{init, 1},        %% {rootid,state0}
     {terminate,1},    %% void
     {getattr, 2},     %% fhandle
     {setattr, 3},     %% sattrargs
     {lookup,  3},     %% diropargs
     {readlink, 2},    %% fhandle
     {read,    5},     %% readargs
     {write,   6},     %% write
     {create,4},       %% createargs
     {remove,3},       %% diropargs
     {rename,5},       %% renameargs
     {link,4},         %% linkargs
     {symlink,5},      %% symlinkargs
     {mkdir,4},        %% createargs
     {rmdir,3},        %% diropargs
     {readdir, 3},     %% readdirargs
     {statfs, 2}       %% fhandle
    ];
behaviour_info(_) ->
    undefined.

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, nfs_server}, nfs_server, [], []).

add_mountpoint(Path, Module, Opts) ->
    gen_server:call(?MODULE, {add_mountpoint, Path, Module, Opts}).


start_mountd() ->
    {ok, _Pid} = rpc_server:start_link({local, nfs_mountd},
				      [{udp, any, ?MOUNTD_PORT, false, []}],
				      ?MOUNTPROG,
				      mountprog,
				      ?MOUNTVERS,
				      ?MOUNTVERS,
				      nfs_svc,
				      do_init).

start_nfsd() ->
    {ok, _Pid} = rpc_server:start_link({local, nfs_rpc_nfsd},
				      [{udp, any, ?NFS_PORT, false, []}],
				      ?NFS_PROGRAM,
				      nfs_program,
				      ?NFS_VERSION,
				      ?NFS_VERSION,
				      nfs_svc,
				      []).


%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------

init([]) ->
    ?dbg("starting", []),
    start_mountd(),
    start_nfsd(),
    init_tabs(),
    ?dbg("init done", []),
    {ok, #state{ fh_suffix = make_suffix(),
		 mountpoints=[],
		 locals=dict:new() }}.

handle_call(Req, From, State) ->
    ?dbg("call: ~p", [Req]),
    Res = handle_call_(Req, From, State),
    case Res of
	{reply,_Value,_State1} ->
	    ?dbg("call_result: ~p\n", [_Value]);
	_Other ->
	    ?dbg("call_result: other=~p\n", [_Other])
    end,
    Res.

%% ----------------------------------------------------------------------
%% MOUNTPROC
%% ----------------------------------------------------------------------

handle_call_({mountproc_null_2, _Client}, _From, State) ->
    {reply, void, State};

handle_call_({mountproc_mnt_1, PathBin, _Client}, _From, State) ->
    Path = binary_to_list(PathBin),
    Es0 = State#state.mountpoints,
    case lists:keytake(Path, #mount_ent.path, Es0) of
	false ->
	    {reply, {1, void}, State};
	{value,Ent = #mount_ent { opts=Opts, mod=Mod, root=undefined },Es1} ->
	    case callback(Mod, init, [Opts]) of
		{error,_} ->
		    {reply, {1, void}, State};
		{Root,Loc} ->
		    FSID = new_fsid(Mod),
		    {ok, RootFH} = id2fh(Root, FSID, State),
		    Ent1 = Ent#mount_ent { root=RootFH, fsid=FSID },
		    Locals1 = dict:store(FSID,Loc,State#state.locals),
		    State1 = State#state { mountpoints = [Ent1 | Es1],
					   locals = Locals1 },
		    {reply, {0, RootFH}, State1}
	    end;
	{value,#mount_ent { root=Fh },_} ->
	    {reply, {0, Fh}, State}
    end;

handle_call_({mountproc_umnt_1, PathBin, _Client}, _From, State) ->
    ?dbg("unmount directory ~p", [PathBin]),
    Path = binary_to_list(PathBin),
    Es0 = State#state.mountpoints,
    case lists:keytake(Path, #mount_ent.path, Es0) of
	false ->
	    {reply, void, State};
	{value,Ent = #mount_ent { mod=Mod,root=RootFh,fsid=FSID }, Es1} ->
	    if RootFh =/= undefined ->
		    MState = dict:fetch(FSID, State#state.locals),
		    callback(Mod, terminate, [MState]),
		    Ent1 = Ent#mount_ent { root=undefined, fsid=undefined },
		    Locals1 = dict:erase(FSID, State#state.locals),
		    State1 = State#state { mountpoints = [Ent1 | Es1],
					   locals = Locals1 },
		    {reply, void, State1};
	       true ->
		    {reply, void, State}
	    end
    end;

handle_call_({mountproc_umntall_1, _Client}, _From, State) ->
    S1 = 
	lists:foldl(
	  fun(E, Si) ->
		  FSID = E#mount_ent.fsid,
		  if FSID =/= undefined ->
			  MState = dict:fetch(FSID, State#state.locals),
			  Mod = E#mount_ent.mod,
			  callback(Mod, terminate, [MState]),
			  Locals1 = dict:erase(FSID, Si#state.locals),
			  Si#state { locals = Locals1 };
		     true ->
			  Si
		  end
	  end, State, State#state.mountpoints),
    Ms = lists:map(
	   fun(E) ->
		   E#mount_ent { root=undefined, fsid=undefined }
	   end, State#state.mountpoints),
    {reply, void, S1#state { mountpoints = Ms }};

handle_call_({mountproc_export_1, _Client}, _From, State) ->
    ?dbg("export/all", []),
    Ent = export_entries(State#state.mountpoints),
    {reply, Ent, State};

%% ----------------------------------------------------------------------
%% NFSPROC
%% ----------------------------------------------------------------------

handle_call_({nfsproc_null_2, _Client}, _From, State) ->
    {reply, {nfs_stat(ok), void}, State};

handle_call_({nfsproc_getattr_2, FH, _Client}, _From, State) ->
    R = case fh2id(FH) of
	    {ok, _ID} ->
		Mod = fh2mod(FH),
		case fattr(FH, Mod, State) of
		    {ok, FA} ->
			{nfs_stat(ok), FA};
		    {error, Error} ->
			{nfs_stat(Error), void}
		end;
	    error ->
		{nfs_stat(estale), void}
	end,
    {reply, R, State};

handle_call_({nfsproc_setattr_2, {FH, Attrs}, _Client}, _From, State) ->
    R = case fh2id(FH) of
	    {ok, ID} ->
		Mod = fh2mod(FH),
		Loc  = fh2local(FH,State),
		case callback(Mod, setattr, [ID,Attrs,Loc]) of
		    ok ->
			case fattr(FH, Mod, State) of
			    {ok, FA} ->
				{nfs_stat(ok), FA};
			    {error, Error} ->
				{nfs_stat(Error), void}
			end;
		    {error, Reason} ->
			{nfs_stat(Reason), void}
		end;
	    error ->
		{nfs_stat(estale), void}
	end,
    {reply, R, State};    

%% Obsolte (rfc1094)
handle_call_({nfsproc_root_2, _Client}, _From, State) ->
    {reply, void, State};

handle_call_({nfsproc_lookup_2, {DirFH, NameBin}, _C}, _From, State) ->
    R = case fh2id(DirFH) of
	    {ok, DirID} ->
		Name = binary_to_list(NameBin),
		Mod = fh2mod(DirFH),
		Loc  = fh2local(DirFH,State),
		case callback(Mod, lookup, [DirID,Name,Loc]) of
		    {error, Error} ->
			{nfs_stat(Error), void};
		    {ok, ChildID} ->
			{ok, ChildFH} = id2fh(ChildID,fh2fsid(DirFH),State),
			case fattr(ChildFH, Mod, State) of
			    {ok, FA} ->
				{nfs_stat(ok), {ChildFH, FA}};
			    {error, Error} ->
				{nfs_stat(Error), void}
			end
		end;
	    error ->
		{nfs_stat(estale), void}
	end,
    {reply, R, State};


handle_call_({nfsproc_readlink_2, FH,_C},
	     _From,State) ->
    R = case fh2id(FH) of
	    {ok, ID} ->
		Mod = fh2mod(FH),
		Loc  = fh2local(FH,State),
		case callback(Mod, readlink, [ID,Loc]) of
		    {ok, Path} ->
			{nfs_stat(ok),erlang:iolist_to_binary(Path)};
		    {error, Reason} ->
			{nfs_stat(Reason), void}
		end;
	    error ->
		{nfs_stat(estale), void}
	end,
    {reply, R, State};

handle_call_({nfsproc_read_2, {FH, Offset, Count, TotalCount},_C},
	     _From,State) ->
    R = case fh2id(FH) of
	    {ok, ID} ->
		Mod = fh2mod(FH),
		Loc  = fh2local(FH,State),
		case callback(Mod, read, [ID,Offset,Count,TotalCount,Loc]) of
		    {ok, IOList} ->
			case fattr(FH, Mod, State) of
			    {ok, FA} ->
				BIOList = erlang:iolist_to_binary(IOList),
				{nfs_stat(ok), {FA,BIOList}};
			    {error, Error} ->
				{nfs_stat(Error), void}
			end;
		    {error, Reason} ->
			{nfs_stat(Reason), void}

		end;
	    error ->
		{nfs_stat(estale), void}
	end,
    {reply, R, State};

%% just decribed to be implemented in the future ?
handle_call_({nfsproc_writecache_2, _Client}, _From, State) ->
    {reply, void, State};

handle_call_({nfsproc_write_2, {FH, BeginOffset, Offset, TotalCount, Data},_C},
	     _From,State) ->
    R = case fh2id(FH) of
	    {ok, ID} ->
		Mod = fh2mod(FH),
		Loc  = fh2local(FH,State),
		case callback(Mod, write, [ID,BeginOffset,Offset,TotalCount,
					   Data,Loc]) of
		    ok ->
			case fattr(FH, Mod, State) of
			    {ok, FA} ->
				{nfs_stat(ok), FA};
			    {error, Error} ->
				{nfs_stat(Error), void}
			end;
		    {error, Reason} ->
			{nfs_stat(Reason), void}
		end;
	    error ->
		{nfs_stat(estale), void}
	end,
    {reply, R, State};

handle_call_({nfsproc_create_2, {{DirFH, NameBin},Attr}, _Client},
	     _From, State) ->
    R = case fh2id(DirFH) of
	    {ok, DirID} ->
		Name = binary_to_list(NameBin),
		Mod = fh2mod(DirFH),
		Loc  = fh2local(DirFH,State),
		case callback(Mod, create, [DirID, Name, Attr, Loc]) of
		    {ok, ChildID} ->
			{ok, ChildFH} = id2fh(ChildID,fh2fsid(DirFH),State),
			case fattr(ChildFH, Mod, State) of
			    {ok, FA} ->
				{nfs_stat(ok), {ChildFH, FA}};
			    {error, Error} ->
				{nfs_stat(Error), void}
			end;
		    {error, Error} ->
			{nfs_stat(Error), void}
		end;
	    error ->
		{nfs_stat(estale), void}
	end,
    {reply, R, State};

handle_call_({nfsproc_remove_2, {DirFH, NameBin}, _Client},_From, State) ->
    R = case fh2id(DirFH) of
	    {ok, DirID} ->
		Name = binary_to_list(NameBin),
		Mod = fh2mod(DirFH),
		Loc  = fh2local(DirFH,State),
		case callback(Mod, remove, [DirID, Name, Loc]) of
		    {error, Error} ->
			nfs_stat(Error);
		    ok ->
			nfs_stat(ok)
		end;
	    error ->
		nfs_stat(estale)
	end,
    {reply, R, State};


handle_call_({nfsproc_rename_2,{DirFromFH,FromBin},{DirToFH,ToBin},_Client},
	     _From, State) ->
    R = case {fh2id(DirFromFH),fh2id(DirToFH)} of
	    {{ok, DirFromID},{ok,DirToID}} ->
		From = binary_to_list(FromBin),
		To = binary_to_list(ToBin),
		ModFrom = fh2mod(DirFromFH),
		ModTo = fh2mod(DirToFH),
		if ModFrom =:= ModTo ->
			Loc  = fh2local(DirFromFH,State),
			case callback(ModFrom, rename, [DirFromID,From,
							DirToID,To,Loc]) of
			    {error, Error} ->
				nfs_stat(Error);
			    ok ->
				nfs_stat(ok)
			end;
		   true -> %% must be in same space (FIXME)
			nfs_stat(nxio)
		end;
	    {_, error} -> nfs_stat(estale);
	    {error,_}  -> nfs_stat(estale)
	end,
    {reply, R, State};

handle_call_({nfsproc_link_2,{FromFH,ToFH,ToNameBin},_Client},
	     _From, State) ->
    R = case {fh2id(FromFH),fh2id(ToFH)} of
	    {{ok, FromID},{ok,ToID}} ->
		ModFrom = fh2mod(FromFH),
		ModTo = fh2mod(ToFH),
		ToName = binary_to_list(ToNameBin),
		if ModFrom =:= ModTo ->
			Loc  = fh2local(FromFH,State),
			case callback(ModFrom, link, [FromID,ToID,ToName,
						      Loc]) of
			    {error, Error} ->
				nfs_stat(Error);
			    ok ->
				nfs_stat(ok)
			end;
		   true -> %% must be in same space (FIXME)
			nfs_stat(nxio)
		end;
	    {_, error} -> nfs_stat(estale);
	    {error,_}  -> nfs_stat(estale)
	end,
    {reply, R, State};

handle_call_({nfsproc_symlink_2,{FromFH,FromNameBin,ToPathBin,SAttr},_Client},
	     _From, State) ->
    R = case fh2id(FromFH) of
	    {ok, FromID} ->
		Mod = fh2mod(FromFH),
		FromName = binary_to_list(FromNameBin),
		ToPath = binary_to_list(ToPathBin),
		Loc  = fh2local(FromFH,State),
		case callback(Mod, symlink, [FromID,FromName,ToPath,SAttr,
					     Loc]) of
		    {error, Error} ->
			nfs_stat(Error);
		    ok ->
			nfs_stat(ok)
		end;
	    error  -> nfs_stat(estale)
	end,
    {reply, R, State};

handle_call_({nfsproc_mkdir_2, {{DirFH, NameBin},Attr}, _Client},
	     _From, State) ->
    R = case fh2id(DirFH) of
	    {ok, DirID} ->
		Name = binary_to_list(NameBin),
		Mod = fh2mod(DirFH),
		Loc  = fh2local(DirFH,State),
		case callback(Mod, mkdir, [DirID, Name, Attr, Loc]) of
		    {error, Error} ->
			{nfs_stat(Error), void};
		    {ok, ChildID} ->
			{ok, ChildFH} = id2fh(ChildID,fh2fsid(DirFH),State),
			case fattr(ChildFH, Mod, State) of
			    {ok, FA} ->
				{nfs_stat(ok), {ChildFH, FA}};
			    {error, Error} ->
				{nfs_stat(Error), void}
			end
		end;
	    error ->
		{nfs_stat(estale), void}
	end,
    {reply, R, State};

handle_call_({nfsproc_rmdir_2, {DirFH, NameBin}, _Client},_From, State) ->
    R = case fh2id(DirFH) of
	    {ok, DirID} ->
		Name = binary_to_list(NameBin),
		Mod = fh2mod(DirFH),
		Loc  = fh2local(DirFH,State),
		case callback(Mod, rmdir, [DirID,Name,Loc]) of
		    {error, Error} ->
			nfs_stat(Error);
		    ok ->
			nfs_stat(ok)
		end;
	    error ->
		nfs_stat(estale)
	end,
    {reply, R, State};

handle_call_({nfsproc_readdir_2, {FH, <<Cookie:32/integer>>, Count}, _Client},
	    _From,
	    State) ->
    R = case fh2id(FH) of
	    {ok, ID} ->
		Mod = fh2mod(FH),
		%% FIXME: handle big count + continuation
		Loc = fh2local(FH,State),
		case callback(Mod, readdir, [ID,Count,Loc]) of
		    {error, ErrCode} ->
			{nfs_stat(ErrCode), void};
		    {ok, Names} ->
			Entries = entries(Mod,fh2fsid(FH),ID,Names,
					  Loc,State,Cookie),
			{nfs_stat(ok), {Entries, true}}
		end;
	    error ->
		{nfs_stat(estale), void}
	end,
    {reply, R, State};

handle_call_({nfsproc_statfs_2, FH, _C}, _From, State) ->
    R = case fh2id(FH) of
	    {ok, ID} ->
		Mod = fh2mod(FH),
		Loc = fh2local(FH,State),
		case callback(Mod, statfs, [ID,Loc]) of
		    {ok, Res = {_Tsize, _Bsize, _Blocks, _Bfree, _Bavail}} ->
			{nfs_stat(ok), Res};
		    {error, Reason} ->
			{nfs_stat(Reason), void}
		end;
	    error ->
		{nfs_stat(estale), void}
	end,
    {reply, R, State};

%% Local calls
handle_call_({add_mountpoint, Path, Mod, Opts}, _From, State) ->
    ?dbg("adding mount point=~p,mod=~p,opts=~p", [Path,Mod,Opts]),
    Ent = #mount_ent { path=Path, mod=Mod, opts=Opts },
    Ms = [Ent|State#state.mountpoints],
    {reply, ok, State#state{ mountpoints=Ms }};

handle_call_(Request, _From, State) ->
    io:format("Undefined callback: ~p~n", [Request]),
    Reply = {error, nocallback},
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    ?dbg("handle_cast got ~p\n", [_Msg]),
    {noreply, State}.

handle_info(_Info, State) ->
    ?dbg("handle_info got ~p\n", [_Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

init_tabs() ->
    ets:new(?fh_id_tab,    [named_table, public, set]),
    ets:new(?id_fh_tab,    [named_table, public, set]),
    ets:new(?fsid_mod_tab, [named_table, public, set]),
    ets:new(?misc_tab,     [named_table, public, set]),
    ets:insert(?misc_tab,  {next_fsid, 0}),
    ok.

make_suffix() ->
    SufBits = (32 - 8) * 8,
    {A,B,C} = now(),
    S0 = A,
    S1 = (S0 * 1000000) + B,
    S2 = (S1 * 1000000) + C,
    <<S2:SufBits/integer>>.


callback(Mod, Func, Args) ->
    ?dbg("callback ~s:~s ~p\n", [Mod,Func,Args]),
    Res = (catch apply(Mod,Func,Args)),
    case Res of
	{'EXIT',Rsn} ->
	    io:format("Error in ~s: ~p~n", [Func,Rsn]),
	    {error, eio};
	_ ->
	    ?dbg("result = ~p\n", [Res]),
	    Res
    end.

fattr(FH, Mod, State) ->
    {ok,ID} = fh2id(FH),
    Loc = fh2local(FH, State),
    case callback(Mod, getattr, [ID,Loc]) of
	{ok, As} -> {ok,make_fattr(FH,As)};
	Error -> Error
    end.

entries(_Mod,_FSID,_DirID,[],_MState,_State,_N) ->
    void;
entries(Mod,FSID,DirID,[H|T],MState,State,N) ->
    Next = N+1,
    case callback(Mod,lookup,[DirID,H,MState]) of
	{ok, ID} ->
	    {id2fileid(ID,FSID,State),	% fileid
	     H,				% name
	     <<Next:32/integer>>,		% cookie
	     entries(Mod,FSID,DirID,T,MState,State,Next)	% nextentry
	    };
	{error, _Error} ->
	    %% just skip this one
	    entries(Mod,FSID,DirID,T,MState,State,Next)
    end.

export_entries([]) ->
    void;
export_entries([#mount_ent{path=Path}|Es]) ->
    Groups = void,  %% fixme
    {erlang:iolist_to_binary(Path),
     Groups,
     export_entries(Es)}.

id2fileid(ID,FSID,State) ->
    {ok, FH} = id2fh(ID,FSID,State),
    fh2fileid(FH).

id2fh(ID,FSID,State) ->
    case ets:lookup(?id_fh_tab, ID) of
	[{_, FH}] -> {ok, FH};
	[] -> {ok, new_fh(ID,FSID,State)}
    end.

new_fh(ID,FSID,State) ->
    Suf  = State#state.fh_suffix,
    N    = ets:update_counter(?misc_tab,{next_fileid,FSID},1),
    FH = <<N:32/integer, FSID:32/integer, Suf/binary>>,
    ets:insert(?id_fh_tab, {ID, FH}),
    ets:insert(?fh_id_tab, {FH, ID}),
    FH.

%% fetch local backend state
fh2local(<<_FileID:32/integer, FSID:32/integer, _Pad/binary>>,State) ->
    dict:fetch(FSID, State#state.locals).

fh2mod(<<_FileID:32/integer, FSID:32/integer, _Pad/binary>>) ->
    ets:lookup_element(?fsid_mod_tab, FSID, 2).

fh2fileid(<<FileID:32/integer, _FSID:32/integer, _Pad/binary>>) ->
    FileID.

fh2fsid(<<_:32/integer, FSID:32/integer, _/binary>>) ->
    FSID.

fh2id(FH) ->
    case ets:lookup(?fh_id_tab, FH) of
	[{_, ID}] ->
	    {ok, ID};
	[] ->
	    error
    end.

new_fsid(Mod) when is_atom(Mod) ->
    FSID = ets:update_counter(?misc_tab, next_fsid, 1),
    ets:insert(?misc_tab,     {{next_fileid,FSID}, 0}),
    ets:insert(?fsid_mod_tab, {FSID, Mod}),
    FSID.


%% ----------------------------------------------------------------------
%% File attributes
%% ----------------------------------------------------------------------

%% List of file attributes, some which have defaults.
-record(fattr,{
	  type  = 'NFNON',
	  mode  = 0,
	  nlink = 1,
	  uid   = 0,
	  gid   = 0,
	  size  = 0,
	  blocksize = 1024,
	  rdev = 0,
	  blocks = 1,
	  fsid,
	  fileid,
	  atime = {0,0},
	  mtime = {0,0}, 
	  ctime = {0,0}
	 }).

%% Make an fattr (file attributes) struct. Opts is a dictionary of
%% values we're interested in setting (see fattr_spec/0 below for
%% available options).
make_fattr(FH,Opts) ->
    F0 = #fattr { fsid = fh2fsid(FH), fileid = fh2fileid(FH) },
    F = make_fattr_list(Opts,F0),
    list_to_tuple(tl(tuple_to_list(F))).

make_fattr_list([], F) -> F;
make_fattr_list([{Opt,Value}|Opts], F) ->
    F1 = set_fattr(Opt,Value,F),
    make_fattr_list(Opts, F1).

set_fattr(type,Value,F) ->
    F#fattr { type = fattr_type(Value),
	      mode = fattr_mode(Value) bor F#fattr.mode
	    };
set_fattr(mode,Value,F) ->
    F#fattr { mode = fattr_mode(Value) bor F#fattr.mode };
set_fattr(nlink,Value,F) -> F#fattr { nlink = Value };
set_fattr(uid,Value,F)   -> F#fattr { uid = Value };
set_fattr(gid,Value,F)   -> F#fattr { gid = Value };
set_fattr(size,Value,F)  -> F#fattr { size = Value };
set_fattr(blocksize,Value,F) -> F#fattr { blocksize = Value };
set_fattr(rdev,Value,F)   -> F#fattr { rdev = Value };
set_fattr(blocks,Value,F) -> F#fattr { blocks = Value };
set_fattr(fsid,Value,F)   -> F#fattr { fsid = Value };
set_fattr(fileid,Value,F) -> F#fattr { fileid = Value };
set_fattr(atime,Value,F)  -> F#fattr { atime = Value };
set_fattr(mtime,Value,F)  -> F#fattr { mtime = Value }; 
set_fattr(ctime,Value,F)  -> F#fattr { ctime = Value }.



fattr_type(none)      -> 'NFNON';
fattr_type(regular)   -> 'NFREG';
fattr_type(directory) -> 'NFDIR';
fattr_type(device)    -> 'NFCHR';
fattr_type(block)     -> 'NFBLK';
fattr_type(symlink)   -> 'NFLNK';
fattr_type(socket)    -> 'NFSOCK';
fattr_type(fifo)      -> 'NFFIFO';
fattr_type(_)         -> 'NFBAD'.


fattr_mode(regular)   -> ?MODE_REGULAR;
fattr_mode(directory) -> ?MODE_DIR;
fattr_mode(device)    -> ?MODE_CHAR;
fattr_mode(block)     -> ?MODE_BLOCK;
fattr_mode(symlink)   -> ?MODE_SYMLINK;
fattr_mode(socket)    -> ?MODE_SOCKET;
fattr_mode(setuid)    -> ?MODE_SETUID;  %% FIX
fattr_mode(setgid)    -> ?MODE_SETGID;  %% FIX
fattr_mode({U,G,O})   ->
    ((access(U) bsl 6) bor (access(G) bsl 3) bor access(O));
fattr_mode(Mode) when is_integer(Mode) ->
    Mode.

access([x|A]) -> ?MODE_OX bor access(A);
access([w|A]) -> ?MODE_OW bor access(A);
access([r|A]) -> ?MODE_OR bor access(A);
access([]) -> 0.


nfs_stat(ok)   -> 'NFS_OK';	                %% no error
nfs_stat(eperm) -> 'NFSERR_PERM';		%% Not owner
nfs_stat(enoent) -> 'NFSERR_NOENT';		%% No such file or directory
nfs_stat(eio) -> 'NFSERR_IO';		        %% I/O error
nfs_stat(enxio) -> 'NFSERR_NXIO';		%% No such device or address
nfs_stat(eacces) -> 'NFSERR_ACCES';             %% Permission denied
nfs_stat(eexist) -> 'NFSERR_EXIST';	        %% File exists
nfs_stat(enodev) -> 'NFSERR_NODEV';	        %% No such device
nfs_stat(enotdir) -> 'NFSERR_NOTDIR';	        %% Not a directory
nfs_stat(eisdir) -> 'NFSERR_ISDIR';	        %% Is a directory
nfs_stat(efbig)	-> 'NFSERR_FBIG';		%% File too large
nfs_stat(enospc) -> 'NFSERR_NOSPC';	        %% No space left on device
nfs_stat(erofs)	-> 'NFSERR_ROFS';		%% Read-only file system
nfs_stat(enametoolong)-> 'NFSERR_NAMETOOLONG';	%% File name too long
nfs_stat(enotempty) -> 'NFSERR_NOTEMPTY';	%% Directory not empty
nfs_stat(edquot) -> 'NFSERR_DQUOT';	        %% Disc quota exceeded
nfs_stat(estale) -> 'NFSERR_STALE';	        %% Stale NFS file handle
nfs_stat(wflush) -> 'NFSERR_WFLUSH';	        %% write cache flushed
nfs_stat(timeout) -> 'NFSERR_IO';	        %% timeout as I/O error

%% ssh_xfer errors 
nfs_stat(no_such_file)           -> nfs_stat(enoent);
nfs_stat(permission_denied)	 -> nfs_stat(eperm);
nfs_stat(failure)                -> nfs_stat(eio);
nfs_stat(bad_message)            -> nfs_stat(eio);
nfs_stat(no_connection)          -> nfs_stat(eio);
nfs_stat(connection_lost)        -> nfs_stat(eio);
nfs_stat(op_unsupported)         -> nfs_stat(enxio);
nfs_stat(invalid_handle)         -> nfs_stat(estae);
nfs_stat(no_such_path)           -> nfs_stat(enoent);
nfs_stat(file_already_exists)    -> nfs_stat(eexist);
nfs_stat(write_protect)          -> nfs_stat(eacces);
nfs_stat(no_media)               -> nfs_stat(enxio);
nfs_stat(no_space_on_filesystem) -> nfs_stat(enospc);
nfs_stat(quota_exceeded)         -> nfs_stat(edquot);
nfs_stat(unknown_principle)      -> nfs_stat(eio);
nfs_stat(lock_conflict)          -> nfs_stat(eio);
nfs_stat(not_a_directory)        -> nfs_stat(enotdir);
nfs_stat(file_is_a_directory)    -> nfs_stat(eisdir);
nfs_stat(cannot_delete)          -> nfs_stat(eacces);
nfs_stat(eof)                    -> nfs_stat(eio);
nfs_stat(_)                      -> nfs_stat(eio).

