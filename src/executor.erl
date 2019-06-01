-module (executor).

-export ([start/4]).

-type log() :: dict:dict(var(), log_value()).
-type var() :: <<_:1, _:_*1>>.
-type log_value() :: {read_write(), binary, binary(), binary()}
				   | {read_write(), atom, atom(), atom()}
				   | {read_write(), integer, integer(), integer()}
				   | {read_write(), float, float(), float()}
				   | {read_write(), string, string(), string()}
				   | {read_write(), list, list(), list()}
				   | {read_write(), term, term(), term()}.
-type read_write() :: r | w | rw | n.

-type transaction_result() :: {ok, log()} | {retry, log()} | {toss, term(), log()}.

%% @doc Start the executor.
%% The function simply calls the 'executor' function, collects the answer,
%% and replies to the client.
-spec start(From :: pid(), ConnectInfo :: term(), Transaction :: gen_acid:transaction(), CallbackMod :: module()) -> ok.
start(From, ConnectInfo, Transaction, CallbackMod)
	-> gen_acid:transaction_executed(From, self(), executor(Transaction, dict:new(), CallbackMod, ConnectInfo, null)).

%% @doc Executor.
%% The executor is the process that will carry on the computation of the 
%% atomic transaction. It will run on a separate process, in order to let
%% the main client to wait for the result in an asynchronous way.
%% The executor will basically go through the list of transactional operations,
%% working on a local representation of the shared memory. In particular,
%% reads are done directly communicating with the Riak Cluster, while 
%% writes are done locally, and will eventually be committed to
%% the cluster.
%%
%% A small list of operations is allowed inside transactions, and the structure
%% of the code  easily allows to extend the set with new commands:
%% - new: creates a new variable locally, that will eventually be committed
%%        in the shared memory. The variable identifier passed to the function
%%        needs to be fresh;
%% - get: reads a variable from the shared memory, and stores it in the
%%        local log. Multiple 'read' on the same variable will contact the
%%        Riak cluster only once to ensure consistency inside the same transaction;
%% - put: sets a variable to the value 'Fun(Params)'. Parameters allow to use shared 
%%        variable names inside function definition. The operation is done locally
%%        in the log, and will be committed after validation. Using the 'put'
%%        operator will read the variable from the memory, if it is currently not
%%        in the log;
%% - retry: forces a transaction retry;
%% - toss: throws an exception that can be handled internally with the operator
%%         'try_catch' or directly in the user code;
%% - try_catch: guards a subtransaction with an exception handler. If the transaction
%%              raises an exception, it may be handle properly. Otherwise it is
%%              propagated upwards;
%% - or_else: this operator allows to build transactions in a compositional way; in
%%            other words, let 't1' and 't2' be two transactions. Calling 
%%         			'atomic(t1), atomic(t2).'
%% 		      allows to execute the transactions atomically.
%% 			  With 'or_else' we can compose the transactions together in order to
%%            build one atomic transaction
%% 					'atomic(or_else(t1,t2))'
%%			  The semantics is straightforward: it tries the first transaction, if 
%%            the execution leads to a (implicit/explicit) 'retry', the control is
%%            redirected to the second transaction. Exceptions raised by either
%%            transactions are simply propagated upwards;
%% - if_else: if-then-else compositional operator. The guard is 'Guard(Params)';
%% - while: while compositional operator. The guard is 'Guard(Params)'.
%% @end
%%
%% @private
-spec executor(gen_acid:transaction(), log(), module(), term(), term()) -> transaction_result().
executor([], Log, _CallbackMod, _ConnectInfo, Result)
	-> {ok, Log, Result};

executor([{new, Id, Fun, Params} | TransOps], Log, Mod, ConnectInfo, _Result)
	-> Params1 = lists:map(evaluate(Log), Params),
	   case lists:any(fun is_error/1,Params1) of
	   		false -> Params2 = lists:map(fun(X) -> erlang:element(2,X) end, Params1),
	   				 try erlang:apply(Fun, Params2) of
	   				 	Val -> case dict:is_key(Id, Log) orelse is_element(Id, Mod, ConnectInfo) of
	   				 				true  -> {toss, duplicate_variable, Log};
	   				 				false -> Log1 = dict:store(Id, {n, Val}, Log),
	   				 						 executor(TransOps, Log1, Mod, ConnectInfo, Val)
	   				 		   end
	   				 catch
	   				 	error:Error -> {toss, Error, Log}
	   				 end;
	   		true  -> {toss, variable_not_found, Log}
	   end;

executor([{get, Id} | TransOps], Log, Mod, ConnectInfo, _Result)
	-> case dict:find(Id, Log) of 
			error -> case Mod:raw_get(ConnectInfo, Id) of
						{ok, Val} -> Log1 = dict:store(Id, {r, Val}, Log),
			   				 		 executor(TransOps, Log1, Mod, ConnectInfo, Val);
						{error, Reason} -> {toss, Reason, Log}
					 end;
			{ok, {_Op, Val}}  -> executor(TransOps, Log, Mod, ConnectInfo, Val)
	   end;

executor([{put, Id, Fun, Params} | TransOps], Log, Mod, ConnectInfo, _Result)
	-> Params1 = lists:map(evaluate(Log), Params),
	   case lists:any(fun is_error/1,Params1) of
	   		false -> Params2 = lists:map(fun(X) -> erlang:element(2,X) end, Params1),
	   				 try erlang:apply(Fun, Params2) of
	   				 	Val -> Log1 = dict:store(Id, {rw, Val}, Log),
	   				 		   executor(TransOps, Log1, Mod, ConnectInfo, Val)
	   				 catch
	   				 	error:Error -> {toss, Error, Log}
	   				 end;
	   		true  -> {toss, variable_not_found, Log}
	   end;

executor([{retry} | _TransOps], Log, _Mod, _ConnectInfo, _Result)
	-> {retry, Log};

executor([{toss, Exception, _Desc} | _TransOps], Log, _Mod, _ConnectInfo, _Result)
	-> {toss, Exception, Log};

executor([{try_catch, Transaction, Handler} | TransOps], Log, Mod, ConnectInfo, Result)
	-> case executor(Transaction, Log, Mod, ConnectInfo, Result) of
			{ok, Log1, Res1} -> executor(TransOps, Log1, Mod, ConnectInfo, Res1);
			{toss, Exc, Log1} -> case handle(Exc,Handler) of
										{caught, T} -> executor(lists:append(T,TransOps), Log, Mod, ConnectInfo, Result);
										not_caught  -> {toss, Exc, Log1}
								  end;
			Retry -> Retry
	   end;

executor([{or_else, Trans1, Trans2} | TransOps], Log, Mod, ConnectInfo, Result)
	-> case executor(Trans1, Log, Mod, ConnectInfo, Result) of
			{ok, Log1, Res1} -> executor(TransOps, Log1, Mod, ConnectInfo, Res1);
			{retry, Log1} -> case executor(Trans2, Log, Mod, ConnectInfo, Result) of
								{ok, Log2, Res2} -> executor(TransOps, Log2, Mod, ConnectInfo, Res2);
								{retry, Log2} -> {retry, dict:merge(fun(_K,X,_Y) -> X end, Log1, Log2)};
								Toss -> Toss
							 end;
			Toss -> Toss
	   end;

executor([{if_else, Guard, Params, Trans1, Trans2} | TransOps], Log, Mod, ConnectInfo, Result)
	-> Params1 = lists:map(evaluate(Log), Params),
	   case lists:any(fun is_error/1,Params1) of
	   		false -> Params2 = lists:map(fun(X) -> erlang:element(2,X) end, Params1),
	   				 case erlang:apply(Guard, Params2) of
	   				 	true -> executor(lists:append(Trans1,TransOps), Log, Mod, ConnectInfo, Result);
	   				 	false -> executor(lists:append(Trans2,TransOps), Log, Mod, ConnectInfo, Result)
	   				 end;
	   		true  -> {toss, variable_not_found, Log}
	   end;

executor([While = {while, Guard, Params, Trans} | TransOps], Log, Mod, ConnectInfo, Result)
	-> Params1 = lists:map(evaluate(Log), Params),
	   case lists:any(fun is_error/1,Params1) of
	   		false -> Params2 = lists:map(fun(X) -> erlang:element(2,X) end, Params1),
	   				 case erlang:apply(Guard, Params2) of
	   				 	true -> executor(lists:append(Trans, [While | TransOps]), Log, Mod, ConnectInfo, Result);
	   				 	false -> executor(TransOps, Log, Mod, ConnectInfo, Result)
	   				 end;
	   		true  -> {toss, variable_not_found, Log}
	   end;

executor([{sleep, Time} | TransOps], Log, Mod, ConnectInfo, Result)
	-> timer:sleep(Time),
	   executor(TransOps, Log, Mod, ConnectInfo, Result).

%% @doc Checks if the argument is of error type
%% @private
-spec is_error(term()) -> boolean().
is_error(Arg) -> Arg =:= error.


%% @doc Evaluate function.
%% Takes a bitstring value (shared variable identifier) and reads its value
%% in the local log.
%% @private
-spec evaluate(Log :: log()) -> fun((var()) -> {ok, term()} | error).
evaluate(Log)
	-> fun(Arg) -> case dict:find(Arg, Log) of
						{ok, {_,Val}} -> {ok, Val};
						error     -> error
	   			   end
	   end.


%% @doc Checks if a variable already exists in the Riak Cluster.
%% @private
-spec is_element(var(), module(), term()) -> boolean().
is_element(Id, Mod, ConnectInfo) 
	-> case Mod:raw_get(ConnectInfo, Id) of 
			{ok, _} -> true;
			_ 		-> false
		end.

%% @doc Handles exception, if possible.
%% @private
-spec handle(atom(), list({atom(), acid:transaction()})) -> {caught, acid:transaction()} | not_caught.
handle(_Excp, []) -> not_caught;
handle(Excp, [{Excp,T} | _Exceptions]) -> {caught,T};
handle(_Excp, [{default,T} | _Exceptions]) -> {caught,T};
handle(Excp, [_ | Exceptions]) -> handle(Excp, Exceptions).

