
-module (riak_tests).

-export ([ new_var/2
         , get_var/1
         , ex1/0
         , ex2/0
         , ex3/0
         %, ex4/1
         , ex5_down/1
         , ex5_up/1
         %, ex6_consume/1
         %, ex6_produce/2
    ]).

%% Suppose a call to `acid_manager:spawn_client/4` was made before running these examples
%%
%% acid_manager:spawn_client(client1, riak_acid, ws1, [{"192.168.1.2", 8187}, {"192.168.1.2", 8287}, {"192.168.1.2", 8387}]).
%%
%% These examples will show a few of the main features of a Riak implementation of our
%% `gen_acid` behaviour.

%% Utilities to create new variables and get their value.
new_var(Name,Val)
  -> acid_manager:atomic(client1, "NEW " ++ Name ++ " " ++ Val, 5000).

get_var(Name)
  -> acid_manager:atomic(client1, "GET " ++ Name, 5000).

%% Example 1
%% -> testing get and put operators.
%% -> testing if_else operator.
%% -> testing try-catch and toss operators.
%%
%% get x
%% get y
%% try
%%     if (x < y) then 
%%         put x x+1
%%     else 
%%         toss x_greater_equal_y
%%     end
%% catch
%%     x_greater_equal_y: put x x-1
%%     default:  
%% end

% ex1() ->
%     acid_manager:atomic( client1,
%         [ gen_acid:get({bucket,x})
%         , gen_acid:get({bucket,y})
%         , gen_acid:try_catch(
%             [ gen_acid:if_else(riak_acid:lt(), [{bucket,x}, {bucket,y}],
%                 [ gen_acid:put({bucket,x}, riak_acid:inc(), [{bucket,x}]) ],
%                 [ gen_acid:toss(x_greater_equal_y, "Don't panic!") ]
%               )
%             ],
%             [ {x_greater_equal_y,
%                 [ gen_acid:put({bucket,x}, riak_acid:dec(), [{bucket,x}]) ]
%               }
%             , {default, []}
%             ]
%           )
%         ]
%     ,5000).

ex1() -> acid_manager:atomic( client1, "
    GET @{bucket,x}
    GET @{bucket,y}
    TRY
    {
        IF (@{bucket,x} < @{bucket,y})
        THEN PUT @{bucket,x} @{bucket,x}+1
        ELSE THROW x_greater_equal_y
    
    } CATCH {

        x_greater_equal_y : PUT @{bucket,x} @{bucket,x}-1 
        default : { }
    }
    ", 5000).

%% Example 2
%% -> testing get and put operators.
%% -> testing while operator.
%% -> testing if_else operator.
%%
%% new i 0
%% get x 
%% get y 
%% while (i < x) do 
%%     if ( i mod 2 == 0) then
%%         put y y+i
%%     put i i+1
%% end

% ex2() ->
%     acid_manager:atomic( client1,
%         [ gen_acid:new({bucket,i}, riak_acid:const({integer, 0}), [])
%         , gen_acid:get({bucket,x})
%         , gen_acid:get({bucket,y})
%         , gen_acid:while(riak_acid:lt(), [{bucket,i},{bucket,x}],
%             [ gen_acid:if_else(fun({_,V}) -> (V rem 2) == 0 end, [{bucket,i}],
%                 [ gen_acid:put({bucket,y}, fun({T,V},{T,I}) -> {T,V + I} end, [{bucket,y},{bucket,i}])],
%                 []
%               )
%             , gen_acid:put({bucket,i}, riak_acid:inc(), [{bucket,i}])
%             ]
%           )
%         ]
%     ,5000).

ex2() -> acid_manager:atomic( client1, "
    NEW @{bucket,i} 0
    GET @{bucket,x}
    GET @{bucket,y}
    WHILE (@{bucket,i} < @{bucket,x})
    {
        IF (@{bucket,i} rem 2 = 0)
        THEN PUT @{bucket,y} @{bucket,y}+@{bucket,i}
        ELSE {}
        
        PUT @{bucket,i} @{bucket,i}+1
    }
    ", 5000).


%% Example 3
%% -> testing implicit new, put and get operators
%% -> testing or_else operator
%%
%% get x
%% get y 
%% {
%%     if (x < y) then 
%%         retry
%%
%% } orElse {
%%
%%     new z 0
%%     put z 2*(x+y)
%%     put x x+1    
%% }

% ex3() ->
%     acid_manager:atomic( client1,
%         [ gen_acid:get({bucket,x})
%         , gen_acid:get({bucket,y})
%         , gen_acid:or_else(
%             [ gen_acid:if_else(riak_acid:lt(), [{bucket,x}, {bucket,y}],
%                 [ gen_acid:retry() ],
%                 [ ]
%               )
%             ],
%             [ gen_acid:new({bucket,z}, riak_acid:const({integer,0}), [])
%             , gen_acid:put({bucket,z}, fun({T,X},{T,Y}) -> {T,2*(X + Y)} end, [{bucket,x},{bucket,y}])
%             , gen_acid:put({bucket,x}, riak_acid:inc(), [{bucket,x}])
%             ]
%           )
%         ]
%     ,infinity).

ex3() -> acid_manager:atomic( client1, "
    GET @{bucket,x}
    GET @{bucket,y}
    OR
    {
        IF (@{bucket,x} < @{bucket,y})
        THEN RETRY
        ELSE {}

    } ELSE {

        NEW @{bucket,z} 0
        PUT @{bucket,z} 2*(@{bucket,x}+@{bucket,y})
        PUT @{bucket,x} @{bucket,x}+1
    }
    ", infinity).

%% Example 4: SLEEP FUNCTION DOES NOT EXIST
%% -> testing put and get operators
%% -> testing if_else
%% -> testing implicit retry and wait state
%%
%% The test needs to be executed by different nodes;

% ex4(Time) ->
%     acid_manager:atomic( client1,
%         [ gen_acid:get({bucket,x})
%         , gen_acid:get({bucket,y})
%         , gen_acid:if_else(fun({_,X},{_,Y}) -> timer:sleep(Time), X < Y end, [{bucket,x}, {bucket,y}],
%             [ gen_acid:put({bucket,x}, riak_acid:inc(), [{bucket,x}]) ],
%             [ gen_acid:put({bucket,y}, riak_acid:inc(), [{bucket,y}]) ]
%           )
%         ]
%     ,infinity).

%% Example 5
%% Implementation of a simple mutex using atomic
%% transaction:
%%
%% down(Sem):
%%    get Sem
%%    if Sem > 0 then 
%%       put Sem Sem-1
%%    else
%%       retry
%%    end
%%
%% up(Sem):
%%    get Sem
%%    put Sem Sem+1

% ex5_down(Sem) ->
%     acid_manager:atomic( client1,
%       [ 
%         gen_acid:get(Sem)
%       , gen_acid:if_else(fun({_,X}) -> X > 0 end, [Sem],
%           [ gen_acid:put(Sem, riak_acid:dec(), [Sem]) ],
%           [ gen_acid:retry() ]
%           )
%       ]
%     , infinity).

% ex5_up(Sem) ->
%     acid_manager:atomic( client1,
%       [ 
%         gen_acid:get(Sem)
%       , gen_acid:put(Sem, riak_acid:inc(), [Sem])
%       ]
%     , 1000).

ex5_down(S) ->
    acid_manager:atomic(client1, "
    GET " ++ S ++ "
    IF (" ++ S ++ " > 0)
    THEN PUT " ++ S ++ " "  ++ S ++ "-1
    ELSE RETRY
    ", infinity).

ex5_up(S) ->
    acid_manager:atomic(client1, "
    GET " ++ S ++ "
    PUT " ++ S ++ " " ++ S ++ "+1
    ", 1000).

%% Example 6: THE LANGUAGE DOES NOT HANDLE LISTS AT THE MOMENT
%% Producer/Consumer implementation
%%
%% consume(Chan):
%%    get Chan
%%    if Chan.length > 0 then 
%%       pop Chan
%%    else
%%       retry
%%    end
%%
%% produce(Chan,Obj):
%%    get Chan
%%    pusht Obj Chan

% ex6_consume(Chan) ->
%     acid_manager:atomic(client1,
%         [ gen_acid:get(Chan)
%         , gen_acid:if_else(fun({_,X}) -> erlang:length(X) > 0 end, [Chan],
%             [ gen_acid:put(Chan, fun({T,X}) -> {T,erlang:tl(X)} end, [Chan])],
%             [ gen_acid:retry() ]
%           )
%         ]
%       , infinity).

% ex6_produce(Chan, Obj) ->
%     acid_manager:atomic( client1,
%       [ gen_acid:get(Chan)
%       , gen_acid:put(Chan, fun({T,X}) -> {T,[Obj | X]} end, [Chan])
%       ]
%     , 5000).