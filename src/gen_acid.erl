
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

-module(gen_acid).
-behaviour (gen_statem).

%%% @doc GEN_ACID behaviour.
%%%
%%% This behaviour provides a simple abstraction for atomic transactions over a
%%% shared memory. Thanks to the user-defined callbacks it is possible to implement
%%% atomic transactions over any storage support.
%%%
%%% The module implements a finite state machine (gen_statem behaviour). Once the FSM
%%% is started it is possible to issue a transaction through the API function
%%%      atomic(<fsm>, <trans>, <timeout>)
%%%
%%% A simple and intuitive DDL language is defined in order to build up transactions
%%% 
%%%      Fun ::= fun((...) -> term())
%%%      Guard ::= fun((...) -> boolean())
%%% 
%%%      ExceptionHandler ::= []
%%%                         | [(`default`,Transaction)]
%%%                         | (atom(),Transaction) : ExHandler
%%%      
%%%      Transaction ::= [TransOp]
%%%      TransOp ::= NewOp | GetOp | PutOp | RetryOp
%%%                | TossOp | TryCatchOp | OrElseOp |  IfElseOp | WhileOp
%%% 
%%%      NewOp ::= `new` atom() Fun [atom()]
%%%      GetOp ::= `get` atom()
%%%      PutOp ::= `put` atom() Fun [atom()]
%%%      RetryOp ::= `retry`
%%%      TossOp ::=  `toss` atom() string()
%%%      TryCatchOp ::= `try_catch` Transaction ExHandler
%%%      OrElseOp ::= `or_else` Transaction Transaction
%%%      IfElseOp ::= `if_else` Guard [atom()] Transaction Transaction
%%%      WhileOp ::= `while` Guard [atom()] Transaction
%%%
%%% Functions `new_transaction/0`, and `add_to_transaction/2` are also provided to 
%%% build transactions in a compositional way.
%%%
%%%
%%% Our system represents an additional layer in the communication with
%%% the shared memory
%%% 
%%%  |              |              |              | 
%%%  |              |              |              | 
%%%  |     User    -->    Acid    -->   Shared    | 
%%%  |     App      |    System    |    Memory    |
%%%  |             <--            <--   Server    | 
%%%  |              |              |              |
%%%  |              |              |              |
%%%               (API)       (Callbacks)
%%%
%%% User application communicates with an ACID manager, which can manage multiple
%%% implementations of the behaviour (from now on called ACID clients) and work 
%%% on different (shared) memories.
%%%
%%% When the user requests a new ACID client it provides an implementation of this
%%% behaviour to the manager. This is passed to the behaviour itself to instantiate it.
%%%
%%% @end

%%% TYPE SPECIFICATION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export_type ([transaction/0]).

-type exception() :: atom().
-type exception_handler() :: {exception(), transaction()}.

-type trans_op() :: {new, term(), fun((...) -> term()), [term()]} 
                  | {get, term()}
                  | {put, term(), fun((...) -> term()), [term()]}
                  | retry
                  | {toss, exception()}
                  | {try_catch, transaction(), [exception_handler()]}
                  | {or_else, transaction(), transaction()}
                  | {if_else, fun((...) -> boolean()), [term()], transaction(), transaction()}
                  | {while, fun((...) -> boolean()), [term()], transaction()}
                  .

-type transaction() :: [trans_op()].

-type tn() :: {integer(), node()}.


%%% EXPORT DECLARATION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% FSM API
-export ([
      start/3
    , start_link/3
    , stop/1
    , atomic/3
    , transaction_executed/3
]).

%% FSM Callbacks
-export ([
    callback_mode/0,
    init/1,
    terminate/3,
    idle/3,
    bv_startTN/3,
    working/3,
    init_validation/3,
    validation/3,
    commit/3,
    wait/3
]).

%% Transaction DDL
-export ([
      new_transaction/0
    , add_to_transaction/2
    , new/3
    , get/1
    , put/3
    , retry/0
    , toss/1
    , try_catch/2
    , or_else/2
    , if_else/4
    , while/3
]).

%% FSM State Definition
-record (acid_data, {
       callback,
       connect_info,
       workspace,
       startTN,
       currentTN,
       committedTN = {0, node()},
       acceptedTN = 0,
       proposedTN = 0,
       currentTransaction,
       committedTransactions = [],
       currentExecutor = {self(), erlang:make_ref()},
       rdSet = sets:new(),
       wrSet = sets:new(),
       asyncWaitFrom = [],
       result = null
}).


%%% CALLBACK DEFINITION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Connecting to the (shared) memory.
%% Establish a connection with the shared memory. `ConnectArgs` are provided
%% by the user. Any information in `ConnectInfo` is passed to the functions
%% `raw_new/3`, `raw_get/2`, `raw_put/3` in order to be able to contact the memory.
-callback connect(ConnectArgs :: term()) -> {ok, ConnectInfo :: term()} | {error, Reason :: term()}.

%% @doc Disconnecting from the memory.
%% The user may specify how to disconnect from the shared memory.
-callback disconnect(ConnectInfo :: term()) -> ok | {error, Reason :: term()}.

%% @doc Interacting with the shared memory
%% Callbacks to create a new variable, retrieve a value or override a value
%% in the shared memory
-callback raw_new(ConnectInfo :: term(), Name :: term(), Val :: term()) -> ok | {error, Reason :: term()}.
-callback raw_get(ConnectInfo :: term(), Name :: term()) -> {ok, Val :: term()} | {error, Reason :: term()}.
-callback raw_put(ConnectInfo :: term(), Name :: term(), Val :: term()) -> ok | {error, Reason :: term()}.

%%% TRANSACTION DEFINITION LANGUAGE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Creates a new transaction.
-spec new_transaction() -> transaction().

new_transaction() -> []. 

%% @doc Add a new operation to a given transaction.
-spec add_to_transaction(
    Op :: trans_op(),
    T :: transaction()
) -> transaction().

add_to_transaction(Op,T) -> T ++ [Op].

%% @doc Create a new variable (with a fresh identifier) initialized to `Fun(Params)`.
-spec new(
    Id :: term(),
    Fun :: fun((...) -> term()),
    Params :: [term()]
) -> trans_op().

new(Id, Fun, Params) 
    when is_function(Fun, length(Params))
    -> {new, Id, Fun, Params}.

%% @doc Read the value of the shared variable `Id`.
-spec get(
    Id :: term()
) -> trans_op().

get(Id) 
    -> {get, Id}.

%% @doc Override the value of the shared variable `Id` with `Fun(Params)`.
-spec put(
    Id :: term(),
    Fun :: fun((...) -> term()),
    Params :: [term()]
) -> trans_op().

put(Id, Fun, Params) 
    when is_function(Fun, length(Params))
    -> {put, Id, Fun, Params}.

%% @doc Force the computation to restart.
-spec retry() -> trans_op().

retry() -> {retry}.

%% @doc Throw a user-defined exception.
-spec toss(
    Exc :: exception()
) -> trans_op().

toss(Exc)
    when is_atom(Exc)
    -> {toss, Exc}.

%% @doc Catch user-defined/system exceptions.
%% `Handler` is used to handle caught exceptions.
%% @end
-spec try_catch(
    Trans :: transaction(),
    Handler :: [exception_handler()]
) -> trans_op().

try_catch(Trans, Handler)
    when is_list(Trans)
       , is_list(Handler)
    -> {try_catch, Trans, Handler}.

%% @doc Compositional operator.
%% 'Trans1' is executed first; if the execution leads to an explicit `retry`,
%% the control is redirected to `Trans2`.
%% @end
-spec or_else(
    Trans1 :: transaction(),
    Trans2 :: transaction()
) -> trans_op().

or_else(Trans1, Trans2) 
    when is_list(Trans1)
       , is_list(Trans2)
    -> {or_else, Trans1, Trans2}.

%% @doc "If-Else" control operator.
%% If `Guard(Params)` is true, `Trans1` is executed; `Trans2` otherwise.
%% @end
-spec if_else(
    Guard :: fun((...) -> boolean()),
    Params :: [term()],
    Trans1 :: transaction(),
    Trans2 :: transaction()
) -> trans_op().

if_else(Guard, Params, Trans1, Trans2)
    when is_function(Guard,length(Params))
       , is_list(Trans1)
       , is_list(Trans2)
    -> {if_else, Guard, Params, Trans1, Trans2}.

%% @doc "While" control operator.
%% While `Guard(Params)` is true, `Trans1` is executed.
%% @end
-spec while(
    Guard :: fun((...) -> boolean()),
    Params :: [term()],
    Trans :: transaction()
) -> trans_op().

while(Guard, Params, Trans)
    when is_function(Guard,length(Params))
       , is_list(Trans)
    -> {while, Guard, Params, Trans}.


%%% API IMPLEMENTATION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Starting the FSM.
%% The `Module` parameter denotes the module where the callbacks are
%% implemented.
%% @end

-spec start(
    Module      :: module(),
    Workspace   :: term(),
    ConnectArgs :: term()
) -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.

start(Module, Workspace, ConnectArgs)
    -> gen_statem:start(?MODULE, {Module, Workspace, ConnectArgs}, []).

-spec start_link(
    Module      :: module(),
    Workspace   :: term(),
    ConnectArgs :: term()
) -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.

start_link(Module, Workspace, ConnectArgs)
    -> gen_statem:start_link(?MODULE, {Module, Workspace, ConnectArgs}, []).

%% @doc Stopping the FSM.
%% Callback `terminate/3` will be automaically called during the stopping process.
%% @end
-spec stop(Pid :: pid()) -> ok.

stop(Pid) -> gen_statem:stop(Pid, normal, infinity).


%%% CLIENT API %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% N.B. in a few cases we have defined the functions using explicit currification.
%% This is simply done to handle multicast properly.

%% @doc Request/Answer Committed TN.
%% According to the classical algorithm for distributed commit with backward validation,
%% whenever a client starts a new transaction, first computes the maximum TN of all
%% committed transactions. This is done with an asynchronous request to all clients in
%% the cluster. The client is put in an asynchronous waiting state in order to receive
%% all answers, still being able to handle incoming events.
%% @end

-spec request_committed_tn(Ref :: reference()) -> fun((pid()) -> ok).

request_committed_tn(Ref)
    -> fun(Pid) -> gen_statem:cast(Pid, {request_committed_tn, self(), Ref}) end.


-spec answer_committed_tn(Pid :: pid(), Ref :: reference(), LatestCommittedTN :: tn()) -> ok.

answer_committed_tn(Pid, Ref, LatestCommittedTN)
    -> gen_statem:cast(Pid, {answer_committed_tn, self(), Ref, LatestCommittedTN}).

%% @doc Request/Answer TN and WriteSet.
%% According to the classical algorithm for distributed commit with backward validation,
%% whenever a client (with transaction 'current_t') exits its working phase and starts the 
%% validation of its working set, it retrieves all TN and Write Sets of clients with 
%% transaction 't' such that StartTN <= TN(t) <= TN(current_t). 
%% This is done with an asynchronous communication, with the requesting client communicating
%% its own StartTN and TN, and the other clients answering accordingly.
%% If a client transaction is not in the range {StartTN..TN} it will answer with and empty
%% Write Set.
%% @end

-spec request_tn_and_ws(Ref :: reference(), StartTN :: tn(), TN :: tn()) -> fun((pid()) -> ok).

request_tn_and_ws(Ref, StartTN, TN)
    -> fun(Pid) -> gen_statem:cast(Pid, {request_tn_and_ws, self(), Ref, StartTN, TN}) end.

-spec answer_tn_and_ws(Pid :: pid(), Ref :: reference(), WrSet :: sets:set()) -> ok.

answer_tn_and_ws(Pid, Ref, WrSet)
    -> gen_statem:cast(Pid, {answer_tn_and_ws, self(), Ref, WrSet}).

%% @doc ISIS Algorithm for Sequential TN Assignment.
%% This implementation of the ISIS algorithm will grant the assignment of TN in a 
%% sequencial way. Therefore a total ordering of the committed transactions is achieved.
%% The algorithm can be briefly described as follow:
%% 1. ('init_isis') a client C that wants to enter the validation phase broadcasts
%%    a message to the cluster asking for a TN proposal. Every client keeps a 'proposedTN'
%%    field in the data record, in order to keep track of the TN proposed (a client cannot
%%    propose the same TN twice).
%% 2. ('answer_proposed_tn') Each of the clients computes a new 'proposedTN':
%%              proposedTn := max(proposedTn,acceptedTn) + 1;
%%    and send it back to the requesting peer. C itself makes a personal proposal.
%% 3. ('term_isis') once received *all* proposals, the client C computes the maximum and
%%    set this one as its personal TN. It then broadcasts the new TN to all the clients.
%%    Each client will then update the 'acceptedTN' accordingly (keeping track of the max
%%    TN accepted so far).
%%
%% N.B.1: If the validation fails the client will release the TN; if the validation/commit
%% process succedes the TN is retained, for future validation requests.
%% N.B.2: a client global unique identifier must be encoded in the TN proposal, in order to
%% avoid situations where different cilent make the same proposal to each other. We use the 
%% node name in this case (`node()`).
%% @end

-spec init_isis(Ref :: reference()) -> fun((pid()) -> ok).

init_isis(Ref)
    -> fun(Pid) -> gen_statem:cast(Pid, {init_isis, self(), Ref}) end.

-spec answer_proposed_tn(Pid :: pid(), Ref :: reference(), ProposedTN :: tn()) -> ok.

answer_proposed_tn(Pid, Ref, ProposedTN)
    -> gen_statem:cast(Pid, {answer_proposed_tn, self(), Ref, ProposedTN}).

-spec term_isis(AcceptedTN :: tn()) -> fun((pid()) -> ok).

term_isis(AcceptedTN)
    -> fun(Pid) -> gen_statem:cast(Pid, {term_isis, AcceptedTN}) end.

%% @doc Transaction Execution.
%% Ensures the atomic execution of the input transaction. The call is translated to a
%% synchronous request to the ACID FSM. We strongly suggest the use of a 'Timeout', 
%% since is the only way to detect anomalies (no other timeout is set ACID-side).
%% 'transaction_executed' is an internal asynchronous event used by the Transaction Executor
%% to notify the client of the transaction result (see 'executor' code for a more detailed
%% description of the possible transaction results).
%% @end

-spec atomic( Pid     :: pid()
            , Trans   :: transaction()
            , Timeout :: timeout())
           -> {ok, Result :: term()} | {error, Reason :: term()}.

atomic(Pid, Trans, Timeout)
    -> try gen_statem:call(Pid, {atomic, Trans}, Timeout) of
          Reply -> Reply
       catch
          exit:Reason -> gen_statem:cast(Pid, reset),
                         {error,Reason}
       end.

-spec transaction_executed( Pid :: pid()
                          , From :: pid()
                          , Result :: executor:transaction_result())
                         -> ok.

transaction_executed(Pid, From, Result)
    -> gen_statem:cast(Pid, {transaction_executed, From, Result}).


%% @doc Wake a waiting client.
%% Wakes a client that is waiting for some modifications on its read set.
%% @end

-spec wake(WS :: sets:set()) -> fun((pid()) -> ok).

wake(WS)
  -> fun(Pid) -> gen_statem:cast(Pid, {wake,WS}) end.


%% @doc Changing state manually.
%% Forces the state to change with a cast to `self()`.
-spec change_state(Next :: atom()) -> ok.

change_state(Next)
    -> gen_statem:cast(self(), {change_state, Next}).

%% N.B. most of the client API is not exported since it is only used for communication
%%      between peers. On the other hand we don't want external users to use the API
%%      in a wrong way.


%%% 'GEN_STATEM' CALLBACK IMPL %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Callback Mode.
%% Specifies the mode selected for the FSM implementation:
%% 'state_function': events are handled with state-specific functions; for example an
%%                   event 'e' raised in the state 'state0' will be handled with
%%                   functions 'state0/3' or 'state0/4'. Global and unexpected events 
%%                   are redirected to a catch-all 'handle_event/3' function;
%% 'state_enter': upon state change, a special event 'event' will be raised, handled
%%                by the new state itself.
callback_mode() -> [state_functions, state_enter].

%% @doc Init.
%% Initialization function that will connect to the shared memory via `Module:connect/1`
%% call. It will then join the workspace it will work in via `multicast:join_group/1`.
%%
%% The `idle` state is the entry point of the FSM.
%% @end

init({Module, Workspace, ConnectArgs})
    ->  process_flag(trap_exit,true),
        case Module:connect(ConnectArgs) of
            {ok, ConnectInfo} -> case multicast:join_group({Module, Workspace}) of
                                    ok -> Data = #acid_data{ workspace = {Module, Workspace}
                                                           , callback  = Module
                                                           , connect_info = ConnectInfo
                                          },
                                          {ok, idle, Data};
                                    {error, Reason} -> {stop, Reason} 
                                 end;
            {error, Reason} -> {stop, Reason}
        end.

%% @doc Terminate.
%% Function called by the supervisor, before an '{exit, shutdown}' is received.
%% It is often used to save a permanent state (or a recoverable state of the FSM).
%% Here we simply disconnect from the shared memory via `Mod:disconnect/1` and leave
%% the workspace we are currently working in.
%% @end

terminate( _Reason
         , _State
         , #acid_data{ callback     = Mod
                     , workspace    = Workspace
                     , connect_info = ConnectInfo }
         ) 
        -> case Mod:disconnect(ConnectInfo) of
                ok              -> multicast:leave_group(Workspace);
                {error, Reason} -> {stop, Reason}
           end.

%% @doc Idle Phase.
%% The FSM will keep waiting for incoming transaction requests from the user.
%% As a design choice, the client will handle a transaction request at a time,
%% keeping the ACID manager (and the user) in a busy wait until the result is
%% ready (or the timeout runs of).
%% @end

idle( {call, From}
    , {atomic, Transaction}
    , Data
    )
    -> {next_state, bv_startTN, Data#acid_data{ currentTransaction = {From, Transaction} }};

idle(EventType, EventContent, Data)
    -> handle_event(EventType, EventContent, Data).

%% @doc Backward Validation: 'startTN' computation.
%% In order to start the transaction we need to compute 'startTN' first.
%% Informally the value will tell us that any transaction with TN > startTN
%% may interfere with our computation (for example committing changes to some
%% shared variable we read before its commit to succeed).
%% The following steps describe the behaviour of the current state:
%% 1) (enter state action) the client broadcasts a request for latest 
%%    committed transaction number (TN);
%% 2) the client saves the workspace view ('asyncWaitFrom'), along with a reference
%%    associated with messages sent in step (1);
%% 3) the client waits for *all* answers in an asynchronous way, being able to 
%%    handle global messages as well. Whenever it receives an answer, it will
%%    update the 'startTN' field, taking the bigger between the current and 
%%    the one received;
%% 4) once all messages are received, it finally moves to the next state
%%    ('working').
%% @end

bv_startTN( enter
          , _Prev
          , Data = #acid_data{ workspace   = Workspace
                             , committedTN = CTN }
          )
%    when Prev =:= idle orelse Prev =:= validation orelse Prev =:= wait
    -> Ref = erlang:make_ref(),
       case multicast:send_to_group(request_committed_tn(Ref), Workspace) of
            {ok, []} -> change_state(working),
                        {keep_state, Data#acid_data{ startTN = CTN }};
            {ok, Members} -> 
                {keep_state, Data#acid_data{ startTN = CTN, asyncWaitFrom = {Ref, Members} }};
            {error, Reason} -> {exit, Reason}
       end;

bv_startTN( cast
          , {answer_committed_tn, From, Ref, LatestCommittedTN}
          , Data = #acid_data{ startTN = STN
                             , asyncWaitFrom = {Ref, Members} }
          )
    -> case lists:member(From, Members) of
          true  -> NewMembers = lists:delete(From,Members),
                   NewStartTN = erlang:max(STN, LatestCommittedTN),
                   case NewMembers of
                        [] -> {next_state, working, Data#acid_data{ startTN = NewStartTN }};
                        _  -> {keep_state, Data#acid_data{ startTN = NewStartTN, asyncWaitFrom = {Ref, NewMembers} }}
                   end;
          false -> keep_state_and_data
       end;

bv_startTN(EventType, EventContent, Data)
    -> handle_event(EventType, EventContent, Data).

%% @doc Working Phase.
%% The working phase is the one that spawns the executor for the
%% current requested transaction, and waits for the result. In the 
%% enter state action it spawns the executor and monitor the process.
%% It will then store the Pid in the 'acid_data' structure,
%% to handle future unexpected behaviours.
%% The executor will notify the result of the transaction with the
%% event
%%      {transaction_executed, From, Result}
%% The client will handle the different kinds of result as follow:
%% {ok, Log, Res}: since the transaction is successfully executed, read and
%%                 write sets will be deduced from the 'Log' and the client
%%                 moves to the validation process. The result of a
%%                 transaction is computed "a-la Erlang", where the value of
%%                 the last expression is returned;
%% {retry, Log}: an explicit 'retry' has been found in the transaction
%%               code. The client moves to the state 'wait' until 
%%               further notification is received. After that, the client
%%               will retry the transaction execution;
%% {throw, Exception, Log}: the transaction has thrown an exception. This
%%                          will be notified to the user (with 'gen_statem:reply/2'
%%                          , and the client will simply forget about the transaction.
%%                          
%% N.B.1 read and write sets are generated folding the Log dictionary.
%% N.B.2 while read set simply keeps the variable reference, the write
%%       set keeps track of the new value (used in the commit phase).
%% @end

working( enter
       , _Prev
       , Data = #acid_data{ callback           = Mod
                          , currentTransaction = {_From,Transaction}
                          , connect_info       = ConnectInfo }
       )
%    when Prev =:= bv_startTN orelse Prev =:= working  
    -> Token = erlang:spawn_monitor(executor,start,[self(), ConnectInfo, Transaction, Mod]),
       {keep_state, Data#acid_data{ currentExecutor = Token }};

working( cast
       , {transaction_executed, From, Result}
       , Data = #acid_data{ currentTransaction = {User, _T}
                          , currentExecutor = {From, _Ref} }
       )
    -> case Result of
        {ok, Log, Val}   -> {RdSet, WrSet} = computeRWSets(Log),
                            {next_state, init_validation, Data#acid_data{ rdSet = RdSet, wrSet = WrSet, result = Val }};
        {retry, Log}     -> {RdSet, _WrSet} = computeRWSets(Log),
                            {next_state, wait, Data#acid_data{ rdSet = RdSet }};
        {toss, Ex, _Log} -> gen_statem:reply(User, {error, Ex}),
                            {next_state, idle, Data}
       end;

working( info
       , {'DOWN', Ref, process, From, Info}
       , #acid_data{ currentExecutor = {From, Ref} })
    when Info =/= normal
    -> repeat_state_and_data;

working(EventType, EventContent, Data)
    -> handle_event(EventType, EventContent, Data).

%% @doc Backward Validation: Initialize Validation.
%% Before starting the actual initialization the client needs to 
%% compute the TN for the current transaction. To do so, in a totally
%% distributed way, we use the ISIS algorithm. Refer to the 
%% API section to have an overview of the algorithm.
%% Here the client, with the enter state action will compute its own
%% proposal for the TN; it then asks to do the same to all the other
%% nodes in the cluster, and waits for the answer (async wait). Each
%% time it receive a new proposal, it will compare it with the current
%% one and take the maximum. Once all answers are received, the client
%% moves to the next state ('validation'). 
%% @end

init_validation( enter
               , _Prev %working
               , Data = #acid_data{ workspace  = Workspace
                                  , acceptedTN = ATN
                                  , proposedTN = PTN }
               )
    ->  Ref = erlang:make_ref(),
        Max = 1 + erlang:max(ATN, PTN), 
        case multicast:send_to_group(init_isis(Ref), Workspace) of
            {ok, []} -> change_state(validation),
                        {keep_state, Data#acid_data{ proposedTN = Max, currentTN = {Max, node()} }};
            {ok, Members} -> 
                        {keep_state, Data#acid_data{ proposedTN = Max, currentTN = {Max, node()}, asyncWaitFrom = {Ref, Members} }};
            {error, Reason} -> {exit, Reason}
        end;

init_validation( cast
               , {answer_proposed_tn, From, Ref, ProposedTN}
               , Data = #acid_data{ workspace     = Workspace
                                  , currentTN     = CTN
                                  , acceptedTN    = ATN
                                  , asyncWaitFrom = {Ref, Members} }
               )
    -> case lists:member(From, Members) of
          true  -> NewCurrentTN = erlang:max(CTN, ProposedTN),
                   NewMembers = lists:delete(From, Members),
                   case NewMembers of
                     [] -> multicast:send_to_group(term_isis(NewCurrentTN), Workspace),
                           {next_state, validation, Data#acid_data{ acceptedTN = erlang:max(ATN, erlang:element(1,NewCurrentTN)), currentTN = NewCurrentTN }};
                     _  -> {keep_state, Data#acid_data{ currentTN = NewCurrentTN, asyncWaitFrom = {Ref, NewMembers} }}
                   end;
          false -> keep_state_and_data
       end;

init_validation(EventType, EventContent, Data)
    -> handle_event(EventType, EventContent, Data).

%% @doc Validation Phase.
%% This state will handle the actual validation phases. First of
%% all (enter state action) the client will request all write sets of 
%% transactions with TN between the 'startTN' field and its own TN.
%% Each time it receives a new one, it validates its own read set against it
%% in order to be able to interrupt the process in case of an early conflict.
%% If the validation process is completed (all messages received), the client
%% will proceed with the commit of the write set (`commit` state).
%%
%% N.B. The write set of the committed transaction is stashed in order to be able
%%      to reply to write set requests.
%% @end

validation( enter
          , _Prev %init_validation
          , Data = #acid_data{ workspace = Workspace
                             , startTN   = STN
                             , currentTN = CTN }
          )
    -> io:format("Transaction Number: ~w~n", [CTN]),
       Ref = erlang:make_ref(),
       case multicast:send_to_group(request_tn_and_ws(Ref, STN, CTN), Workspace) of
            {ok, []} -> change_state(commit),
                        keep_state_and_data;
            {ok, Members} -> {keep_state, Data#acid_data{ asyncWaitFrom = {Ref, Members} }};
            {error, Reason} -> {exit, Reason}
       end;

validation( cast
          , {answer_tn_and_ws, From, Ref, WrSet}
          , Data = #acid_data{ rdSet         = RdSet
                             , asyncWaitFrom = {Ref, Members} }
          )
    -> case lists:member(From, Members) of
          true  -> case checkValidity(RdSet, WrSet) of
                      true  ->  NewMembers = lists:delete(From, Members),
                                case NewMembers of 
                                   [] -> {next_state, commit, Data};
                                   _  -> {keep_state, Data#acid_data{ asyncWaitFrom = {Ref, NewMembers} }}
                                end;
                      false -> {next_state, bv_startTN, Data#acid_data{ currentTN = {0, node()}, rdSet = sets:new(), wrSet = sets:new() }}
                   end;
          false -> keep_state_and_data
       end;

validation(EventType, EventContent, Data)
    -> handle_event(EventType, EventContent, Data).

%% @doc Commit Phase.
%% This is actually a fake state to logically divide validation and commit phases.
%% The client commits any change in the write set in the "enter state" action, and then 
%% changes state. If the commit fails, the client will stop, notifying the user.
%% @end

commit( enter
      , _Prev %validation
      , Data = #acid_data{ callback           = Mod
                         , workspace          = Workspace
                         , connect_info       = ConnectInfo
                         , currentTN          = CTN
                         , currentTransaction = {From, _Trans}
                         , wrSet              = WrSet
                         , result             = Result}
      )
    -> case store(Mod, ConnectInfo, sets:to_list(WrSet)) of
            ok -> WS = sets:fold(fun(X,Acc) -> sets:add_element(erlang:element(1,X),Acc) end, sets:new(), WrSet),
                  CT = {CTN, WS},
                  NewData = Data#acid_data{ committedTN = Data#acid_data.currentTN
                                          , committedTransactions = [CT | Data#acid_data.committedTransactions]
                                          , currentTN = {0, node()}
                                          , rdSet = sets:new()
                                          , wrSet = sets:new() },
                  gen_statem:reply(From, {ok, Result}),
                  multicast:send_to_group(wake(WS), Workspace),
                  change_state(idle),
                  {keep_state, NewData};
            {error, Reason} -> {stop_and_reply, Reason, [{reply, From, {error, Reason}}]}
       end;

commit(EventType, EventContent, Data)
    -> handle_event(EventType, EventContent, Data).

%% @doc Wait Phase.
%% The client waits for any modifications on variables in its 
%% read set. This is a smart way to handle explicit user retries.
%% While waiting, the client keeps handling global variables.
%%
%% N.B. Here `checkValidity/2` is used to find conflicts between
%% the two sets.
%% @end

wait( cast
    , {wake, WrSet}
    , Data = #acid_data{ rdSet = RdSet }
    )
    -> case checkValidity(RdSet,WrSet) of
          true  -> keep_state_and_data;
          false -> NewData = Data#acid_data{ currentTN = {0, node()}, rdSet = sets:new(), wrSet = sets:new() },
                   {next_state, bv_startTN, NewData}
       end;

wait(EventType, EventContent, Data)
    -> handle_event(EventType, EventContent, Data).


%%% GLOBAL EVENTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc 'committedTN' request.
%% the client will simply reply with the latest committed transaction TN.
%% @end

handle_event( cast
            , {request_committed_tn, From, Ref}
            , #acid_data{ committedTN = CTN }
            )
    -> answer_committed_tn(From, Ref, CTN),
       keep_state_and_data;

%% @doc Write set request.
%% The client will collect all personal transaction with TN in the range
%% specified by the event ('StartTN' and 'TN'), and will merge the write sets.
%% If a transaction is currently being validated, its write set needs to be
%% included too.
%%
%% As a design choice, all clients will reply to the request, even if the 
%% merged write set is empty.
%% @end

handle_event( cast
            , {request_tn_and_ws, From, Ref, StartTN, TN}
            , Data
            )
    -> answer_tn_and_ws(From, Ref, mergeWSs(StartTN, TN, Data)),
       keep_state_and_data;

%% @doc TN proposal request.
%% The client replies with a (fresh) proposal.
%% @end

handle_event( cast
            , {init_isis, From, Ref}
            , Data = #acid_data{ acceptedTN = ATN
                               , proposedTN = PTN }
            )
    -> Proposal = 1 + erlang:max(ATN, PTN),
       answer_proposed_tn(From, Ref, {Proposal, node()}),
       {keep_state, Data#acid_data{ proposedTN = Proposal }};

%% @doc New accepted TN notification.
%% The client updates the 'acceptedTN' field, if necessary.
%% @end

handle_event( cast
            , {term_isis, {NewATN,_}}
            , Data = #acid_data{ acceptedTN = ATN }
            )
    -> {keep_state, Data#acid_data{ acceptedTN = erlang:max(ATN, NewATN) }};

%% @doc Changing State.
%% Handles the `change_state` event.
%% @end
handle_event( cast
            , {change_state, Next}
            , Data)
    -> {next_state, Next, Data};

%% @doc Reset FSM after user timeout.
%% @end
handle_event(cast, reset, Data)
  -> {next_state, idle, Data#acid_data{ currentTN = {0, node()}, rdSet = sets:new(), wrSet = sets:new() }};

%% @doc Notifing unexpected events.
%% @end

%handle_event(enter, _State, _Data) -> keep_state_and_data;
%handle_event(info, {'DOWN', _Ref, process, _Pid, _Reason}, _Data ) -> io:format("[debug] Uncaught DOWN message~n", []), keep_state_and_data;
%handle_event(cast, {transaction_executed,_,_}, _Data) -> keep_state_and_data;
%handle_event(cast, {wake,_WS}, _Data) -> io:format("[debug] Uncaught WAKE message~n", []), keep_state_and_data;

handle_event(EventType, _EventContent, _Data)
    -> %io:format("[debug] Unespected event: ~s~n", [EventType]),
       keep_state_and_data.

%%% AUXILIARY FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Merge Write Sets.
%% Merge all Write Sets whose transaction number is between `From` and `To`.
%% It also takes into account if a transaction is currently being validated
%% (and hence it has to be included, too.
%% @end
%% @private
mergeWSs(From, To, #acid_data{ currentTN = CTN, wrSet = WrSet, committedTransactions = Ts})
    -> Init = if
            From < CTN andalso CTN < To
                    -> sets:fold(fun(X,Acc) -> sets:add_element(erlang:element(1,X), Acc) end, sets:new(), WrSet);
            true    -> sets:new()
       end,
       Filter = fun({TN,WR}, Acc) ->
            if
                From < TN andalso TN < To -> sets:union(WR,Acc);
                true -> Acc
            end
       end, 
       lists:foldl(Filter, Init, Ts).

%% @doc Check validity.
%% Check if the input read set and write set are disjoint.
%% @end
%% @private
checkValidity(RS,WS)
    -> sets:is_disjoint(RS,WS).

%% @doc Store a write set.
%% Stores a write set in the shared memory using user-defined callbacks.
%% @end
%% @private
store(_Mod, _ConnectInfo, []) -> ok;
store(Mod, ConnectInfo, [{Key, {Op, Val}} | Rest])
  -> Fun = case Op of
        n -> raw_new;
        _ -> raw_put
     end,
     case Mod:Fun(ConnectInfo, Key, Val) of
            ok  -> store(Mod, ConnectInfo, Rest);
            Err -> Err 
     end.

%% @doc Computing Read and Write Sets.
%% It computes read and write set from an executor log
%% @end
%% @private
computeRWSets(Log)
    -> Filter = fun(Key, Val = {Op, _}, {RdSet, WrSet}) ->
                    case Op of
                        n  -> {sets:add_element(Key,RdSet), sets:add_element({Key, Val}, WrSet)};
                        r  -> {sets:add_element(Key,RdSet), WrSet};
                        w  -> {RdSet                      , sets:add_element({Key, Val}, WrSet)};
                        rw -> {sets:add_element(Key,RdSet), sets:add_element({Key, Val}, WrSet)}
                    end
       end, 
       dict:fold(Filter, {sets:new(),sets:new()}, Log).