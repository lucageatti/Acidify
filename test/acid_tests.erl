
-module (acid_tests).

-export ([ ex1/0
         , ex2/0
         , ex3/0
         , ex4/1
         , ex5_down/1
         , ex5_up/1
         , ex6_consume/1
         , ex6_produce/2
    ]).

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

ex1() ->
    acid:atomic(
        [ acid:get(x)
        , acid:get(y)
        , acid:try_catch(
            [ acid:if_else(acid:lt(), [x, y],
                [ acid:put(x, acid:inc(), [x]) ],
                [ acid:toss(x_greater_equal_y) ]
              )
            ],
            [ {x_greater_equal_y,
                [ acid:put(x, acid:dec(), [x]) ]
              }
            , {default, []}
            ]
          )
        ]
    ,5000).

%% Example 2
%% -> testing get and put operators.
%% -> testing while operator.
%% -> testing if_else operator.
%%
%% new integer i 0
%% get x 
%% get y 
%% while (i < x) do 
%%     if ( i mod 2 == 0) then
%%         put y y+i
%%     put i i+1
%% end

ex2() ->
    acid:atomic(
        [ acid:new(i, integer, acid:const(0), [])
        , acid:get(x)
        , acid:get(y)
        , acid:while(acid:lt(), [i,x],
            [ acid:if_else(fun(I) -> (I rem 2) == 0 end, [i],
                [ acid:put(y, fun(Y,I) -> Y + I end, [y,i])],
                []
              )
            , acid:put(i, acid:inc(), [i])
            ]
          )
        ]
    ,5000).

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
%%     new integer z
%%     put z 2*(x+y)
%%     put x x+1    
%% }

ex3() ->
    acid:atomic(
        [ acid:get(x)
        , acid:get(y)
        , acid:or_else(
            [ acid:if_else(acid:lt(), [x, y],
                [ acid:retry() ],
                [ ]
              )
            ],
            [ acid:new(z, integer, fun() -> 0 end, [])
            , acid:put(z, fun(X,Y) -> 2*(X + Y) end, [x,y])
            , acid:put(x, acid:inc(), [x])
            ]
          )
        ]
    ,infinity).

%% Example 4
%% -> testing put and get operators
%% -> testing if_else
%% -> testing implicit retry and wait state
%%
%% The test needs to be executed by different nodes;

ex4(Time) ->
    acid:atomic(
        [ acid:get(x)
        , acid:get(y)
        , acid:sleep(Time)
        , acid:if_else(acid:lt(),[x, y],
            [ acid:put(x, acid:inc(), [x]) ],
            [ acid:put(y, acid:inc(), [y]) ]
          )
        ]
    ,infinity).

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

ex5_down(Sem) ->
    acid:atomic([ acid:get(Sem)
                , acid:if_else(fun(X) -> X > 0 end, [Sem],
                    [ acid:put(Sem, acid:dec(), [Sem]) ],
                    [ acid:retry() ]
                  )
                ]
    , infinity).

ex5_up(Sem) ->
    acid:atomic([ acid:get(Sem)
                , acid:put(Sem, acid:inc(), [Sem])
                ]
    , 1000).

%% Example 6
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

ex6_consume(Chan) ->
    acid:atomic([ acid:get(Chan)
                , acid:if_else(fun(X) -> erlang:length(X) > 0 end, [Chan],
                    [ acid:put(Chan, fun erlang:tl/1, [Chan] )],
                    [ acid:retry() ]
                  )
                ]
    , infinity).

ex6_produce(Chan, Obj) ->
    acid:atomic([ acid:get(Chan)
                , acid:put(Chan, fun(X) -> [ Obj | X ] end, [Chan] )
                ]
    , 5000).