
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

-module(amazon_s3_acid).
-behaviour(gen_acid).

-export([
      connect/1
        , disconnect/1 
        , raw_new/3 
        , raw_get/2 
        , raw_put/3
        , ex1/0
        , ex3/0
        , down/1
        , up/1
]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% `GEN_ACID` CALLBACKS IMPLEMENTATIONS 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Implementation of the 'connect' callback for Amazon S3
%% It takes as the only parameter the empty list. It returns either {error, configuration_failed} or {ok, Conf},
%% where 'Conf' is the record containing all the connection info.
%% @end
-spec connect(ConnectArgs :: []) -> {ok, erlcloud_aws:aws_config()} | {error, term()}.
connect([])
    -> case erlcloud_aws:auto_config() of
            {ok, Conf} -> {ok, Conf};
            undefined  -> {error, configuration_failed}
       end.



%% @doc Implementation of the 'disconnect' callback for Amazon S3
%% It does nothing, it simply returns 'ok'.
%% @end
-spec disconnect(ConnectInfo :: erlcloud_aws:aws_config()) -> ok .
disconnect(_ConnectInfo) -> ok.



%% @doc Implementation of the 'raw_new' callback for Amazon S3
%% In takes three parameters: 'ConnectInfo', that is the record with all the configuration info, 'Name' (the name 
%% of the variable which we want to create) and 'Val' (the value we want to write).
%% @end
-spec raw_new(ConnectInfo :: erlcloud_aws:aws_config()
            , Name :: {string(),string()}
            , Val :: {atom(), term()}
            ) -> ok | {error, Reason :: term()}.
raw_new(ConnectInfo, Name, Val)
    -> ?MODULE:raw_put(ConnectInfo, Name, Val).




%% @doc Implementation of the 'raw_get' callback for Amazon S3
%% It takes two parameters: 'ConnectInfo' (the record with all the configuration info) and 'Name' (the name
%% of the variable we want to read).
%% @end
-spec raw_get(ConnectInfo :: erlcloud_aws:aws_config()
            , Name :: {string(),string()}
            ) -> {ok, Val :: term()} | {error, Reason :: term()}.

raw_get(ConnectInfo, {Bucket, Name})
    -> Data = erlcloud_s3:get_object(Bucket, Name, ConnectInfo),
       Marshalled = erlang:element(2, lists:last(Data)),
       case erlang:is_binary(Marshalled) of
            true  -> {ok, erlang:binary_to_term(Marshalled)};
            false -> {error, variable_not_found}
       end.





%% @doc Implementation of the 'raw_put' callback for Amazon S3
%% In takes three parameters: 'ConnectInfo', that is the record with all the configuration info, 'Name' (the name 
%% of the variable which we want to overwrite) and 'Val' (the value we want to write).
%% @end
-spec raw_put(ConnectInfo :: erlcloud_aws:aws_config()
            , Name :: {string(),string()}
            , Value :: {atom(), term()}
            ) -> ok | {error, Reason :: term()}.

raw_put(ConnectInfo, {Bucket, Name}, Value={Type, Val})
    -> case isOfType(Type, Val) of
            true  -> erlcloud_s3:put_object(Bucket, Name, erlang:term_to_binary(Value), ConnectInfo),
                     ok;
            false -> {error, bad_type_value}
        end.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% AUXILIARY FUNCTIONS 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EXAMPLES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


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

ex1() ->[ gen_acid:get({"genacid","x"})
        , gen_acid:get({"genacid","y"})
        , gen_acid:try_catch(
            [ gen_acid:if_else(fun({_TX,X},{_TY,Y}) -> X<Y end, [{"genacid","x"}, {"genacid","y"}],
                [ gen_acid:put({"genacid","x"}, fun({T,X}) -> {T,X+1} end, [{"genacid","x"}]) ],
                [ gen_acid:toss(x_greater_equal_y) ]
              )
            ],
            [ {x_greater_equal_y,
                [ gen_acid:put({"genacid","x"}, fun({T,X}) -> {T,X-1} end, [{"genacid","x"}]) ]
              }
            , {default, []}
            ]
          )
        ].



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

ex3() ->[ gen_acid:get({"genacid","x"})
        , gen_acid:get({"genacid","y"})
        , gen_acid:or_else(
            [ gen_acid:if_else(fun({_TX,X},{_TY,Y}) -> X<Y end, [{"genacid","x"}, {"genacid","y"}],
                [ gen_acid:retry() ],
                [ ]
              )
            ]
            ,
            [ gen_acid:put({"genacid","z"}, fun() -> {integer,0} end, [])
            , gen_acid:put({"genacid","z"}, fun({_TX,X},{_TY,Y}) -> {integer, 2*(X + Y)} end, [{"genacid","x"}, {"genacid","y"}])
            , gen_acid:put({"genacid","x"}, fun({T,X}) -> {T,X+1} end, [{"genacid","x"}])
            ]
          )
        ].





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

down(Sem) ->[ gen_acid:get(Sem)
            , gen_acid:if_else(fun({_T,X}) -> X > 0 end, [Sem],
                        [ gen_acid:put(Sem, fun({T,X}) -> {T,X-1} end, [Sem]) ],
                        [ gen_acid:retry() ]
                      )
            ].


up(Sem) -> [ gen_acid:get(Sem)
          , gen_acid:put(Sem, fun({T,X}) -> {T,X+1} end, [Sem])
            ].







