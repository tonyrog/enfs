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
-define(mod_fsid_tab, nfs_mod_fsid).   %% Module   => FSID
-define(fsid_mod_tab, nfs_fsid_mod).   %% FSID     => Module
%% misc:
%%   next_fsid          : COUNTER
%%   {next_fileid,FSID} : COUNTER
-define(misc_tab,     nfs_misc).

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
	  fh,     %% root file handle
	  state   %% backend state
	}).

-record(state, {
	  mountpoints = [] :: [#mount_ent{}]
	 }).

-spec behaviour_info(Arg::callbacks) -> 
			    list({FunctionName::atom(), Arity::integer()}).
behaviour_info(callbacks) ->
    [{init, 1},        %% {rootid,state0}
     {terminate,1},    %% void
     {getattr, 1},     %% fhandle
     {setattr, 2},     %% sattrargs
     {lookup,  2},     %% diropargs
     {readlink, 1},    %% fhandle
     {read,    4},     %% readargs
     {write,   5},     %% write
     {create,3},       %% createargs
     {remove,2},       %% diropargs
     {rename,4},       %% renameargs
     {link,3},         %% linkargs
     {symlink,4},      %% symlinkargs
     {mkdir,3},        %% createargs
     {rmdir,2},        %% diropargs
     {readdir, 2},     %% readdirargs
     {statfs, 1}       %% fhandle
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

%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------

init([]) ->
    ?dbg("starting", []),
    start_mountd(),
    start_nfsd(),
    init_tabs(),
    ?dbg("init done", []),
    {ok, #state{mountpoints=dict:new()}}.

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

init_tabs() ->
    ets:new(?fh_id_tab,    [named_table, public, set]),
    ets:new(?id_fh_tab,    [named_table, public, set]),
    ets:new(?fsid_mod_tab, [named_table, public, set]),
    ets:new(?mod_fsid_tab, [named_table, public, set]),
    ets:new(?misc_tab,     [named_table, public, set]),
    ets:insert(?misc_tab,  {fh_suffix, make_suffix()}),
    ets:insert(?misc_tab,  {next_fsid, 0}),
    ok.

make_suffix() ->
    SufBits = (32 - 8) * 8,
    {A,B,C} = now(),
    S0 = A,
    S1 = (S0 * 1000000) + B,
    S2 = (S1 * 1000000) + C,
    B0 = <<S2:SufBits/integer>>,
    B0.

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

handle_call_({add_mountpoint, Path, Mod, Opts}, _From, State) ->
    ?dbg("adding mount point=~p,mod=~p,opts=~p", [Path,Mod,Opts]),
    Ent = #mount_ent { path=Path, mod=Mod, opts=Opts,
		       fh=undefined, state=undefined },
    Ms = [Ent|State#state.mountpoints],
    {reply, ok, State#state{mountpoints=Ms}};

%% NFS/RPC callbacks

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
	{value,Ent = #mount_ent { opts=Opts, mod=Mod, fh=Fh },Es1} ->
	    if Fh =:= undefined ->
		    case callback(Mod, init, [Opts]) of
			{error,_} ->
			    {reply, {1, void}, State};
			{Root,MState0} ->
			    {ok, RootFH} = id2fh(Root, Mod),
			    Ent1 = Ent#mount_ent { fh=RootFH, state=MState0 },
			    State1 = State#state { mountpoints = [Ent1 | Es1] },
			    {reply, {0, RootFH}, State1}
		    end;
	       true ->
		    {reply, {0, Fh}, State}
	    end
    end;

handle_call_({mountproc_umnt_1, PathBin, _Client}, _From, State) ->
    ?dbg("unmount directory ~p", [PathBin]),
    Path = binary_to_list(PathBin),
    Es0 = State#state.mountpoints,
    case lists:keytake(Path, #mount_ent.path, Es0) of
	false ->
	    {reply, void, State};
	{value,Ent = #mount_ent { mod = Mod, fh = Fh, state=MState }, Es1} ->
	    if Fh =/= undefined ->
		    callback(Mod, terminate, [MState]),
		    Ent1 = Ent#mount_ent { fh=undefined, state=undefined },
		    State1 = State#state { mountpoints = [Ent1 | Es1] },
		    {reply, void, State1};
	       true ->
		    {reply, void, State}
	    end
    end;

handle_call_({mountproc_umntall_1, _Client}, _From, State) ->
    Es1 =
	lists:map(
	  fun(Ent = #mount_ent { mod=Mod,fh=Fh,state=MState}) when
		    Fh =/= undefined ->
		  callback(Mod, terminate, [MState]),
		  Ent#mount_ent { fh=undefined, state=undefined };
	     (Ent) ->
		  Ent
	  end, State#state.mountpoints),
    {reply, void, State#state { mountpoints = Es1 }};
		
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
		case fattr(FH, Mod) of
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
		case callback(Mod, setattr, [ID,Attrs]) of
		    ok ->
			case fattr(FH, Mod) of
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
		case callback(Mod, lookup, [DirID, Name]) of
		    {error, Error} ->
			{nfs_stat(Error), void};
		    {ok, ChildID} ->
			{ok, ChildFH} = id2fh(ChildID, Mod),
			case fattr(ChildFH, Mod) of
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
		case callback(Mod, readlink, [ID]) of
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
		case callback(Mod, read, [ID,Offset,Count,TotalCount]) of
		    {ok, IOList} ->
			case fattr(FH, Mod) of
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
		case callback(Mod, write, [ID,BeginOffset,Offset,TotalCount,
					   Data]) of
		    ok ->
			case fattr(FH, Mod) of
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
		case callback(Mod, create, [DirID, Name, Attr]) of
		    {ok, ChildID} ->
			{ok, ChildFH} = id2fh(ChildID, Mod),
			case fattr(ChildFH, Mod) of
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
		case callback(Mod, remove, [DirID, Name]) of
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
			case callback(ModFrom, rename, [DirFromID,From,
							DirToID,To]) of
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
			case callback(ModFrom, link, [FromID,ToID,ToName]) of
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
		case callback(Mod, symlink, [FromID,FromName,ToPath,SAttr]) of
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
		case callback(Mod, mkdir, [DirID, Name, Attr]) of
		    {error, Error} ->
			{nfs_stat(Error), void};
		    {ok, ChildID} ->
			{ok, ChildFH} = id2fh(ChildID, Mod),
			case fattr(ChildFH, Mod) of
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
		case callback(Mod, rmdir, [DirID, Name]) of
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
		%% FIXME: handle big count!
		case callback(Mod, readdir, [ID,Count]) of
		    {error, ErrCode} ->
			{nfs_stat(ErrCode), void};
		    {ok, Names} ->
			Entries = entries(Mod, ID, Names, Cookie),
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
		case callback(Mod, statfs, [ID]) of
		    {ok, Res = {_Tsize, _Bsize, _Blocks, _Bfree, _Bavail}} ->
			{nfs_stat(ok), Res};
		    {error, Reason} ->
			{nfs_stat(Reason), void}
		end;
	    error ->
		{nfs_stat(estale), void}
	end,
    {reply, R, State};

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

%% ----------------------------------------------------------------------
%% getattr
%% ----------------------------------------------------------------------

callback(Mod, Func, Args) ->
    ?dbg("callback ~s:~s ~p\n", [Mod,Func,Args]),
    Res = (catch apply(Mod,Func,Args)),
    case Res of
	{'EXIT',Rsn} ->
	    io:format("Error in ~s: ~p~n", [Func,Rsn]),
	    {error, io};
	_ ->
	    ?dbg("result = ~p\n", [Res]),
	    Res
    end.

fattr(FH, Mod) ->
    {ok, ID} = fh2id(FH),
    case callback(Mod, getattr, [ID]) of
	{ok, As} -> {ok,make_fattr(FH,As)};
	Error -> Error
    end.


entries(_Mod, _ID, [], _N) ->
    void;
entries(Mod, ID, [H|T], N) ->
    N1 = N+1,
    case callback(Mod, lookup, [ID, H]) of
	{ok, _CID} ->
	    {id2fileid(ID, Mod),	% fileid
	     H,				% name
	     <<N1:32/integer>>,		% cookie
	     entries(Mod, ID, T, N1)	% nextentry
	    };
	{error, _Error} ->
	    %% just skip this one
	    entries(Mod, ID, T, N1)
    end.

export_entries([]) ->
    void;
export_entries([#mount_ent{path=Path}|Es]) ->
    Groups = void,  %% fixme
    {erlang:iolist_to_binary(Path),
     Groups,
     export_entries(Es)}.

%% id2fh(ID, FSID | TemplateFH)
%% Returns: {ok, FH}
%% id2fh(ID, TemplateFH) when is_binary(TemplateFH) ->
%%     id2fh(ID, fh2fsid(TemplateFH));
id2fh(ID, Mod) ->
    case ets:lookup(?id_fh_tab, ID) of
	[{_, FH}] ->
	    {ok, FH};
	[] ->
	    {ok, new_fh(ID, Mod)}
    end.

id2fileid(ID, Mod) ->
    {ok, FH} = id2fh(ID, Mod),
    fh2fileid(FH).



new_fh(ID, Mod) ->
    FSID = mod2fsid(Mod),
    Suf  = ets:lookup_element(?misc_tab, fh_suffix, 2),
    N    = ets:update_counter(?misc_tab,{next_fileid,FSID},1),
    FH = <<N:32/integer, FSID:32/integer, Suf/binary>>,
    ets:insert(?id_fh_tab, {ID, FH}),
    ets:insert(?fh_id_tab, {FH, ID}),
    FH.

fh2mod(<<_FileID:32/integer, FSID:32/integer, _Pad/binary>>) ->
    fsid2mod(FSID).

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


mod2fsid(Mod) ->
    case ets:lookup(?mod_fsid_tab, Mod) of
	[{_, FSID}] ->
	    FSID;
	[] ->
	    new_fsid(Mod)
    end.

new_fsid(Mod) when is_atom(Mod) ->
    FSID = ets:update_counter(?misc_tab, next_fsid, 1),
    ets:insert(?misc_tab,     {{next_fileid,FSID}, 0}),
    ets:insert(?fsid_mod_tab, {FSID, Mod}),
    ets:insert(?mod_fsid_tab, {Mod, FSID}),
    FSID.

fsid2mod(FSID) ->
    ets:lookup_element(?fsid_mod_tab, FSID, 2).

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

access(x)   -> ?MODE_OX;
access(w)   -> ?MODE_OW;
access(r)   -> ?MODE_OR.

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
nfs_stat(wflush) -> 'NFSERR_WFLUSH'.	        %% write cache flushed
