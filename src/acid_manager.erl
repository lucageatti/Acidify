%%
%% @authors:
%%      Luca Geatti <geatti.luca@spes.uniud.it>
%%      Federico Igne <igne.federico@spes.uniud.it>
%% 
%% @supervisor:
%%      Marino Miculan <>
%% 
%% Dipartimento di Scienze Matematiche, Informatiche e Fisiche (DMIF)
%% University of Udine - 33100 Udine (UD) - Italy
%%
-module (acid_manager).
-behaviour (gen_server).

-export ([
    start/2
  , start_link/2
  , init/1
  , terminate/2
  , handle_call/3
  , handle_cast/2
  , handle_info/2
  ]).

-export ([
    atomic/2
  , atomic/3
  , list/0
  , spawn_client/3
  , spawn_client/4
  , terminate_clients/1
  , terminate_all/0
  ]).

%%% SERVER STATE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record (state, {
      servers = []    % ACID cluster initial servers
    , clients = #{}   % ACID clients
}).

%%% START MANAGER %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Start the ACID manager.
%% `start` and `start_link` functions for the ACID manager.
-spec start( Servers :: [node()]
           , Opts    :: [atom()])
          -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.

start(Servers, Opts) 
    -> gen_server:start({local, acid}, ?MODULE, [Servers, Opts], []).

-spec start_link( Servers :: [node()]
                , Opts    :: [atom()])
               -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.

start_link(Servers, Opts)
    -> gen_server:start_link({local, acid}, ?MODULE, [Servers, Opts], []).

%%% INTERFACE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% %% @equiv acid_manager:atomic(Name, Transaction, infinity).
% -spec atomic( Name        :: atom()
%             , Transaction :: gen_acid:transaction() )
%            -> {ok, Result :: term()} | {error, Reason :: term()}.

% atomic(Name, Transaction)
%     -> ?MODULE:atomic(Name, Transaction, infinity).

% %% @doc Transaction executionc call.
% %% Blocking call for transaction execution.
% -spec atomic( Name        :: atom()
%             , Transaction :: gen_acid:transaction()
%             , Timeout     :: timeout() )
%            -> {ok, Result :: term()} | {error, Reason :: term()}.

% atomic(Name, Transaction, Timeout)
%     -> gen_server:call(acid, {atomic, Name, Transaction, Timeout}, infinity).

%% @equiv acid_manager:atomic(Name, Transaction, infinity).
-spec atomic( Name        :: atom()
            , Transaction :: string() )
           -> {ok, Result :: term()} | {error, Reason :: term()}.

atomic(Name, Transaction)
    -> ?MODULE:atomic(Name, Transaction, infinity).

%% @doc Transaction executionc call.
%% Blocking call for transaction execution.
-spec atomic( Name        :: atom()
            , Transaction :: string()
            , Timeout     :: timeout() )
           -> {ok, Result :: term()} | {error, Reason :: term()}.

atomic(Name, Transaction, Timeout)
    -> case acid_lexer:string(Transaction) of
          {ok,Tokens,_} -> case acid_parser:parse(Tokens) of
                                {ok, Transaction1} -> gen_server:call(acid, {atomic, Name, Transaction1, Timeout}, infinity);
                                Err                -> Err
                           end;
          Err           -> {error, Err}
       end.

%% @doc Listing current active clients.
-spec list() -> {ok, [atom()]}.

list() -> gen_server:call(acid, list, infinity).

%% @equiv acid_manager:spawn_client(Name, Module, default).
-spec spawn_client( Name        :: atom()
                  , Module      :: module()
                  , ConnectArgs :: term() )
                 -> ok | {error, Reason :: term()}.

spawn_client(Name, Module, ConnectArgs)
    -> ?MODULE:spawn_client(Name, Module, default, ConnectArgs).

%% @doc Spawn a new client.
%% Blocking call for client creation.
-spec spawn_client( Name        :: atom()
                  , Module      :: module()
                  , Workspace   :: atom()
                  , ConnectArgs :: term() )
                 -> ok | {error, Reason :: term()}.

spawn_client(Name, Module, Workspace, ConnectArgs)
    -> gen_server:call(acid, {spawn_client, Name, Module, Workspace, ConnectArgs}, infinity).

%% @doc Terminate client(s).
%% Terminates the clients indentified with a list of `Names`.
-spec terminate_clients( Names :: [atom()] ) -> ok.

terminate_clients(Names)
    -> gen_server:cast(acid, {terminate_clients, Names}).

%% @doc Terminate all clients.
-spec terminate_all() -> ok.

terminate_all()
    -> gen_server:cast(acid, terminate_all).

%%% `GEN_SERVER` BEHAVIOUR IMPL %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc `init` implementation.
%% It connects to the ACID network (casts a `net_kernel:connect` for each of the
%% nodes provided with `Servers`) and then starts the manager with a new state.
%% Servers :: [node()]
%% Opts    :: term()
init([Servers, _Opts])
    -> case connect(Servers) of
          true  -> {ok, #state{ servers = Servers }};
          false -> {stop, connection_failed}
       end.

%% @doc `terminate` implementation.
%% Terminates all clients and disconnects from the Acid network.
terminate(_Reason, State)
    -> stop(maps:keys(State#state.clients), State#state.clients),
       lists:map(fun erlang:disconnect_node/1, erlang:nodes()).

%% @doc Handle Call: atomic transaction execution.
%% It forwards the transaction 'Transaction' to the ACID client `Name` with
%% timeout `Timeout`.
handle_call({atomic, Name, Transaction, Timeout}, _From, State)
    -> Reply = case maps:find(Name, State#state.clients) of
                    {ok, Pid} -> gen_acid:atomic(Pid, Transaction, Timeout);
                    _         -> {error, client_does_not_exist}
       end,
       {reply, Reply, State};

%% @doc Handle Call: listing current clients.
%% Returns the list of clients currently active in the ACID node and
%% controlled by the manager.
handle_call(list, _From, State)
    -> {reply, maps:keys(State#state.clients), State};

%% @doc Handle Call: client spawning.
%% It spawns a new client identified with `Name`. The client is provided
%% via the module `Module` implementing the behaviour `gen_acid`.
%% A workspace (work group within the ACID network) may be specified with
%% variable `Workspace`.
handle_call({spawn_client, Name, Module, Workspace, ConnectArgs}, _From, State)
    -> case (maps:is_key(Name, State#state.clients)) of
          true   -> {reply, {error, duplicate_name}, State};
          false  -> multicast:create_group({Module, Workspace}),
                    {ok, Pid} = gen_acid:start(Module, Workspace, ConnectArgs),
                    Map = State#state.clients,
                    State1 = State#state{ clients = Map#{Name => Pid} },
                    {reply, ok, State1}
       end.

%% @doc Handle Call: terminate clients.
%% Terminates a list `Names` of clients. Names not currently contained in the
%% client map are ignored.
handle_cast({terminate_client, Names}, State)
    -> stop(Names, State#state.clients),
       {noreply, State#state{ clients = maps:without(Names, State#state.clients) }};

%% @doc Handle Call: terminate all clients.
%% Terminates all clients, restoring the initial manager state.
handle_cast(terminate_all, State) ->
    stop(maps:keys(State#state.clients), State#state.clients),
    {noreply, State#state{ clients = #{} }}.

%% @doc Handle Info: unexpected messages
handle_info(Msg, State) ->
    io:format("Unexpected message: ~p~n",[Msg]),
    {noreply, State}.

%%% AUXILIARY FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Connect to ACID cluster.
%% Takes a list of nodes `S` and connects to them via `net_kernel:connect/1`.
-spec connect(S :: [node()]) -> boolean().

connect([]) -> true;
connect(S)  -> %lists:foldl(fun(N, Acc) -> Acc and net_kernel:connect(N) end, false, S)
               lists:any(fun net_kernel:connect_node/1, S).

%% @doc Stop a list of clients.
-spec stop([node()], Map :: map()) -> ok.
stop([], _Map) -> ok;
stop([N|Names], Map)
    -> case maps:get(N, Map) of 
            {ok, Pid} -> gen_acid:stop(Pid);
            _         -> ok
       end, 
       stop(Names,Map).