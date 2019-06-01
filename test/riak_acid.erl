
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

-module(riak_acid).
-behaviour(gen_acid).

-export([
      connect/1
		, disconnect/1 
		, raw_new/3 
		, raw_get/2 
		, raw_put/3
]).

-export ([
      id/0
    , const/1
    , inc/0
    , dec/0
    , eq/0
    , neq/0
    , gt/0
    , lt/0
    , ge/0
    , le/0
]).

-export ([
      newSem/3
    , synchronized/6
    , down/1
    , up/1
]).

%%% `GEN_ACID` CALLBACKS IMPLEMENTATIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Implementation of the 'connect/1' callback for Riak.
%% It takes as the only argument a list of pairs of the form {IP, Port}, specifying the IP address and
%% the port of the Riak server to which connect. It return either {ok, Pid} where Pid is the process
%% identifier of the Riak client or {error, no_riak_server_reachable} in the case all the Riak servers
%% are unreachable.
%% @end

-spec connect(ConnectArgs :: list({string(), integer()})) -> {ok, Pid :: pid()} | {error, no_riak_server_reachable}.

connect([]) 
	-> {error, no_riak_server_reachable};

connect([{IP, Port} | Rest]) 
	-> case riakc_pb_socket:start_link(IP, Port) of
			  {ok, Pid} 		   -> {ok, Pid}; %Pid is the process identifier of the Riak client
			  {error, _Reason} -> connect(Rest)
		 end.

%% @doc Implementation of the 'disconnect/1' callback for Riak.
%% It takes as the only parameter the pid of the Riak client and shut down it. It always
%% returns the atom 'ok'.
%% @end

-spec disconnect(ConnectInfo :: pid()) -> ok | {error, Reason :: term()}.

disconnect(ConnectInfo)
	-> erlang:exit(ConnectInfo, normal),
	   ok.

%% @doc Implementation of the 'raw_new/3' callback for Riak.
%% In Riak there is no difference between creating a new variable or overriding the value
%% of an existing one.
%% @end

-spec raw_new(Pid :: term(), Name :: term(), Val :: term()) -> ok | {error, Reason :: term()}.

raw_new(Pid, Name, Val)
    -> raw_put(Pid, Name, Val).

%% @doc Implementation of the 'raw_get/2' callback for Riak.
%% Calls `riakc_pb_socket:get/3` and retrieves the value if any value is returned. Marshaling is
%% needed in order to communicate with Riak Client.
%% @end

-spec raw_get(ConnectInfo :: term(), Name :: term()) -> {ok, Val :: term()} | {error, Reason :: term()}.

raw_get( Pid %ConnectInfo
       , {Bucket, Id}
       )
    -> case riakc_pb_socket:get(Pid, erlang:term_to_binary(Bucket), erlang:term_to_binary(Id)) of
            {ok, Obj}       -> {ok, erlang:binary_to_term(riakc_obj:get_value(Obj))};
            {error, Reason} -> {error, Reason}
       end.

%% @doc Implementation of the 'raw_put' callback for Riak.
%% Calls `riakc_pb_socket:put/2` and passes the object built with `riakc_obj:new/3`. Marshaling is
%% needed in order to communicate with Riak Client..
%% @end

-spec raw_put(ConnectInfo :: term(), Name :: term(), Val :: term()) -> ok | {error, Reason :: term()}.

% raw_put( Pid %ConnectInfo
%        , {Bucket, Id}
%        , {Type, Val}
%        )
%       -> case isOfType(Type, Val) of
%             true  -> Obj = riakc_obj:new(erlang:term_to_binary(Bucket), erlang:term_to_binary(Id), erlang:term_to_binary({Type, Val})), 
%          					   riakc_pb_socket:put(Pid, Obj);
%          	  false -> {error, bad_type_value}
%          end.

raw_put( Pid %ConnectInfo
       , {Bucket, Id}
       , Val
       )
      -> Obj = riakc_obj:new(erlang:term_to_binary(Bucket), erlang:term_to_binary(Id), erlang:term_to_binary(Val)), 
               riakc_pb_socket:put(Pid, Obj).

%%% AUXILIARY FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec isOfType(Type :: atom(), Val :: term()) -> boolean().

isOfType(Type, Val)
    -> case Type of
          atom    -> erlang:is_atom(Val);
          binary  -> erlang:is_binary(Val);
          boolean -> erlang:is_boolean(Val);
          float   -> erlang:is_float(Val);
          integer -> erlang:is_integer(Val);
          list    -> erlang:is_list(Val);
          string  -> erlang:is_string(Val);
          term    -> true;
          _       -> false
       end.

%% Generic Operators

id() -> fun(X) -> X end.
const(X) -> fun() -> X end.

inc() -> fun({T,X}) -> {T,X+1} end.
dec() -> fun({T,X}) -> {T,X-1} end.

eq()  -> fun({T,X},{T,Y}) -> X == Y end.
neq() -> fun({T,X},{T,Y}) -> X /= Y end.

gt() -> fun({T,X},{T,Y}) -> X > Y end.
lt() -> fun({T,X},{T,Y}) -> X < Y end.
ge() -> fun({T,X},{T,Y}) -> X >= Y end.
le() -> fun({T,X},{T,Y}) -> X =< Y end.


%% Implementation of a simple semaphore using atomic

newSem(Client, Name, Timeout)
    -> acid_manager:atomic(Client, [gen_acid:new(Name, ?MODULE:const({integer,1}), []) ], Timeout).

synchronized(Client, Mod, Fun, Params, Sem, Timeout)
    -> acid_manager:atomic(Client, down(Sem), Timeout),
       erlang:apply(Mod, Fun, Params),
       acid:atomic(Client, up(Sem), Timeout).

down(Sem)
    -> [ gen_acid:get(Sem)
       , gen_acid:if_else(fun({_,X}) -> X > 0 end, [Sem],
            [ gen_acid:put(Sem, ?MODULE:dec(), [Sem]) ],
            [ gen_acid:retry() ]
         )
       ].

up(Sem) 
    -> [ gen_acid:get(Sem)
       , gen_acid:put(Sem, ?MODULE:inc(), [Sem]) ].