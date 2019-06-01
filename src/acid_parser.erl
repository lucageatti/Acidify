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
-module(acid_parser).
-export([parse/1, parse_and_scan/1, format_error/1]).
-file("acid_parser.yrl", 124).


get_line(T) -> io:format("~p~n",[T]), erlang:element(2,T).


to_atom({S,L})   -> to_atom({S,L},S).
to_atom({_,L},S) -> {atom, L, S}.


to_AST({tvar,L,V})
	-> {ok,Ts,_} = erl_scan:string(V,L),
	   {ok,T} = erl_parse:parse_term(Ts),
	   erl_parse:abstract(T).

% prova anche
%	{ok,Ts,_} = erl_scan:string(V,L),
%	{ok,[Abs]} = erl_parse:parse_exprs(Ts),
%	Abs.
	   

unapply(Expr)
    -> L  = get_line(Expr),
       Vs = collect_vars(Expr),
       M  = [{V,newID()} || V <- Vs],
       {Ns,IDs} = lists:unzip(M),
       Fun  = {'fun', L, {clauses, [{clause, L, [ID(L) || ID <- IDs],[],[unapply(Expr,maps:from_list(M))]}]}},
       Args = to_args(Ns,L),
       {Fun, Args}.


unapply({tvar,L,V}, Map)
	-> F = maps:get(V,Map), F(L);

unapply(Expr, Map)
	when erlang:is_list(Expr)
	-> lists:map(fun(E) -> unapply(E, Map) end, Expr);

unapply(Expr, Map)
	when erlang:is_tuple(Expr)
	-> erlang:list_to_tuple(lists:map(fun(E) -> unapply(E, Map) end, erlang:tuple_to_list(Expr)));

unapply(Expr, _)
	-> Expr.


collect_vars({tvar,_,V})
	-> [V];

collect_vars(Expr)
	when erlang:is_list(Expr)
	-> lists:umerge(lists:map(fun collect_vars/1, Expr));

collect_vars(Expr)
	when erlang:is_tuple(Expr)
	-> lists:umerge(lists:map(fun collect_vars/1, erlang:tuple_to_list(Expr)));

collect_vars(_)
	-> [].


newID()
    -> ID = erlang:list_to_atom([$X | erlang:integer_to_list(erlang:unique_integer([positive]))]),
       fun(L) -> {var,L,ID} end.


to_args([],L) -> {nil, L};
to_args([V | Vs],L)
	-> {cons, L, to_AST({tvar,L,V}), to_args(Vs,L)}.

to_value(AST)
	-> erl_eval:expr(AST,erl_eval:new_bindings(),none,none,value).
-file("/home/federico/.kerl/20.0/lib/parsetools-2.1.5/include/yeccpre.hrl", 0).
%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2017. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The parser generator will insert appropriate declarations before this line.%

-type yecc_ret() :: {'error', _} | {'ok', _}.

-spec parse(Tokens :: list()) -> yecc_ret().
parse(Tokens) ->
    yeccpars0(Tokens, {no_func, no_line}, 0, [], []).

-spec parse_and_scan({function() | {atom(), atom()}, [_]}
                     | {atom(), atom(), [_]}) -> yecc_ret().
parse_and_scan({F, A}) ->
    yeccpars0([], {{F, A}, no_line}, 0, [], []);
parse_and_scan({M, F, A}) ->
    Arity = length(A),
    yeccpars0([], {{fun M:F/Arity, A}, no_line}, 0, [], []).

-spec format_error(any()) -> [char() | list()].
format_error(Message) ->
    case io_lib:deep_char_list(Message) of
        true ->
            Message;
        _ ->
            io_lib:write(Message)
    end.

%% To be used in grammar files to throw an error message to the parser
%% toplevel. Doesn't have to be exported!
-compile({nowarn_unused_function, return_error/2}).
-spec return_error(integer(), any()) -> no_return().
return_error(Line, Message) ->
    throw({error, {Line, ?MODULE, Message}}).

-define(CODE_VERSION, "1.4").

yeccpars0(Tokens, Tzr, State, States, Vstack) ->
    try yeccpars1(Tokens, Tzr, State, States, Vstack)
    catch 
        error: Error ->
            Stacktrace = erlang:get_stacktrace(),
            try yecc_error_type(Error, Stacktrace) of
                Desc ->
                    erlang:raise(error, {yecc_bug, ?CODE_VERSION, Desc},
                                 Stacktrace)
            catch _:_ -> erlang:raise(error, Error, Stacktrace)
            end;
        %% Probably thrown from return_error/2:
        throw: {error, {_Line, ?MODULE, _M}} = Error ->
            Error
    end.

yecc_error_type(function_clause, [{?MODULE,F,ArityOrArgs,_} | _]) ->
    case atom_to_list(F) of
        "yeccgoto_" ++ SymbolL ->
            {ok,[{atom,_,Symbol}],_} = erl_scan:string(SymbolL),
            State = case ArityOrArgs of
                        [S,_,_,_,_,_,_] -> S;
                        _ -> state_is_unknown
                    end,
            {Symbol, State, missing_in_goto_table}
    end.

yeccpars1([Token | Tokens], Tzr, State, States, Vstack) ->
    yeccpars2(State, element(1, Token), States, Vstack, Token, Tokens, Tzr);
yeccpars1([], {{F, A},_Line}, State, States, Vstack) ->
    case apply(F, A) of
        {ok, Tokens, Endline} ->
            yeccpars1(Tokens, {{F, A}, Endline}, State, States, Vstack);
        {eof, Endline} ->
            yeccpars1([], {no_func, Endline}, State, States, Vstack);
        {error, Descriptor, _Endline} ->
            {error, Descriptor}
    end;
yeccpars1([], {no_func, no_line}, State, States, Vstack) ->
    Line = 999999,
    yeccpars2(State, '$end', States, Vstack, yecc_end(Line), [],
              {no_func, Line});
yeccpars1([], {no_func, Endline}, State, States, Vstack) ->
    yeccpars2(State, '$end', States, Vstack, yecc_end(Endline), [],
              {no_func, Endline}).

%% yeccpars1/7 is called from generated code.
%%
%% When using the {includefile, Includefile} option, make sure that
%% yeccpars1/7 can be found by parsing the file without following
%% include directives. yecc will otherwise assume that an old
%% yeccpre.hrl is included (one which defines yeccpars1/5).
yeccpars1(State1, State, States, Vstack, Token0, [Token | Tokens], Tzr) ->
    yeccpars2(State, element(1, Token), [State1 | States],
              [Token0 | Vstack], Token, Tokens, Tzr);
yeccpars1(State1, State, States, Vstack, Token0, [], {{_F,_A}, _Line}=Tzr) ->
    yeccpars1([], Tzr, State, [State1 | States], [Token0 | Vstack]);
yeccpars1(State1, State, States, Vstack, Token0, [], {no_func, no_line}) ->
    Line = yecctoken_end_location(Token0),
    yeccpars2(State, '$end', [State1 | States], [Token0 | Vstack],
              yecc_end(Line), [], {no_func, Line});
yeccpars1(State1, State, States, Vstack, Token0, [], {no_func, Line}) ->
    yeccpars2(State, '$end', [State1 | States], [Token0 | Vstack],
              yecc_end(Line), [], {no_func, Line}).

%% For internal use only.
yecc_end({Line,_Column}) ->
    {'$end', Line};
yecc_end(Line) ->
    {'$end', Line}.

yecctoken_end_location(Token) ->
    try erl_anno:end_location(element(2, Token)) of
        undefined -> yecctoken_location(Token);
        Loc -> Loc
    catch _:_ -> yecctoken_location(Token)
    end.

-compile({nowarn_unused_function, yeccerror/1}).
yeccerror(Token) ->
    Text = yecctoken_to_string(Token),
    Location = yecctoken_location(Token),
    {error, {Location, ?MODULE, ["syntax error before: ", Text]}}.

-compile({nowarn_unused_function, yecctoken_to_string/1}).
yecctoken_to_string(Token) ->
    try erl_scan:text(Token) of
        undefined -> yecctoken2string(Token);
        Txt -> Txt
    catch _:_ -> yecctoken2string(Token)
    end.

yecctoken_location(Token) ->
    try erl_scan:location(Token)
    catch _:_ -> element(2, Token)
    end.

-compile({nowarn_unused_function, yecctoken2string/1}).
yecctoken2string({atom, _, A}) -> io_lib:write_atom(A);
yecctoken2string({integer,_,N}) -> io_lib:write(N);
yecctoken2string({float,_,F}) -> io_lib:write(F);
yecctoken2string({char,_,C}) -> io_lib:write_char(C);
yecctoken2string({var,_,V}) -> io_lib:format("~s", [V]);
yecctoken2string({string,_,S}) -> io_lib:write_string(S);
yecctoken2string({reserved_symbol, _, A}) -> io_lib:write(A);
yecctoken2string({_Cat, _, Val}) -> io_lib:format("~tp", [Val]);
yecctoken2string({dot, _}) -> "'.'";
yecctoken2string({'$end', _}) -> [];
yecctoken2string({Other, _}) when is_atom(Other) ->
    io_lib:write_atom(Other);
yecctoken2string(Other) ->
    io_lib:format("~tp", [Other]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



-file("acid_parser.erl", 259).

-dialyzer({nowarn_function, yeccpars2/7}).
yeccpars2(0=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(1=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_1(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(2=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_2(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(3=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_3(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(4=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(5=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(6=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(7=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(8=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(9=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(10=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(11=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(12=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(13=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_7(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(14=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_14(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(15=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_15(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(16=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_16(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(17=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_14(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(18=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_14(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(19=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_14(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(20=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_20(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(21=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_21(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(22=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_22(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(23=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_23(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(24=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_24(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(25=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_14(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(26=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_14(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(27=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_14(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(28=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_14(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(29=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_14(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(30=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_14(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(31=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_14(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(32=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_14(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(33=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_14(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(34=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_14(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(35=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_14(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(36=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_14(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(37=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_14(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(38=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_38(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(39=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_39(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(40=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_40(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(41=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_41(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(42=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_42(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(43=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_43(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(44=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_44(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(45=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_45(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(46=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_46(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(47=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_47(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(48=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_48(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(49=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_49(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(50=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_50(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(51=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_51(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(52=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(53=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_53(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(54=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_54(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(55=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_55(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(56=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_56(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(57=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_57(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(58=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_58(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(59=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_59(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(60=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(61=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_61(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(62=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_62(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(63=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_63(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(64=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_64(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(65=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_65(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(66=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_66(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(67=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_67(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(68=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_68(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(69=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_69(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(70=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_70(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(71=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_71(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(72=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(73=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_73(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(74=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_74(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(75=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_14(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(76=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_76(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(77=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_77(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(78=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(79=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_79(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(80=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_14(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(81=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_81(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(82=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_82(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(83=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(84=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_84(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(85=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(86=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_86(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(87=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_87(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(88=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_88(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(Other, _, _, _, _, _, _) ->
 erlang:error({yecc_bug,"1.4",{missing_state_in_action_table, Other}}).

-dialyzer({nowarn_function, yeccpars2_0/7}).
yeccpars2_0(S, get, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 4, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, 'if', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 5, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, new, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 6, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 7, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, put, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 8, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, retry, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, throw, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 10, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, 'try', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, while, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_1/7}).
yeccpars2_1(_S, '$end', _Ss, Stack, _T, _Ts, _Tzr) ->
 {ok, hd(Stack)};
yeccpars2_1(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_2_(Stack),
 yeccgoto_program(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_3(S, get, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 4, Ss, Stack, T, Ts, Tzr);
yeccpars2_3(S, 'if', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 5, Ss, Stack, T, Ts, Tzr);
yeccpars2_3(S, new, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 6, Ss, Stack, T, Ts, Tzr);
yeccpars2_3(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 7, Ss, Stack, T, Ts, Tzr);
yeccpars2_3(S, put, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 8, Ss, Stack, T, Ts, Tzr);
yeccpars2_3(S, retry, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_3(S, throw, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 10, Ss, Stack, T, Ts, Tzr);
yeccpars2_3(S, 'try', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_3(S, while, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_3_(Stack),
 yeccgoto_commands(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_4/7}).
yeccpars2_4(S, tvar, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 87, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_5/7}).
yeccpars2_5(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_5(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_6/7}).
yeccpars2_6(S, tvar, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 80, Ss, Stack, T, Ts, Tzr);
yeccpars2_6(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_7(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_7(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_8/7}).
yeccpars2_8(S, tvar, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_8(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_9_(Stack),
 yeccgoto_command(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_10/7}).
yeccpars2_10(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_10(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_11: see yeccpars2_7

%% yeccpars2_12: see yeccpars2_5

%% yeccpars2_13: see yeccpars2_7

-dialyzer({nowarn_function, yeccpars2_14/7}).
yeccpars2_14(S, '!', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_14(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_14(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_14(S, false, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_14(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_14(S, true, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_14(S, tvar, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_14(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expr(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_16(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_16(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_16(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_16/7}).
yeccpars2_cont_16(S, '&&', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_16(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_16(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_16(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_16(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_16(S, '<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_16(S, '<=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_16(S, '<>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_16(S, '=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_16(S, '>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_16(S, '>=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_16(S, 'rem', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_16(S, '||', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_16(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_17: see yeccpars2_14

%% yeccpars2_18: see yeccpars2_14

%% yeccpars2_19: see yeccpars2_14

yeccpars2_20(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_20_(Stack),
 yeccgoto_expr(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_21(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expr(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_22(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_22_(Stack),
 yeccgoto_expr(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_23(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expr(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_24(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_24_(Stack),
 yeccgoto_uminus(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_25: see yeccpars2_14

%% yeccpars2_26: see yeccpars2_14

%% yeccpars2_27: see yeccpars2_14

%% yeccpars2_28: see yeccpars2_14

%% yeccpars2_29: see yeccpars2_14

%% yeccpars2_30: see yeccpars2_14

%% yeccpars2_31: see yeccpars2_14

%% yeccpars2_32: see yeccpars2_14

%% yeccpars2_33: see yeccpars2_14

%% yeccpars2_34: see yeccpars2_14

%% yeccpars2_35: see yeccpars2_14

%% yeccpars2_36: see yeccpars2_14

%% yeccpars2_37: see yeccpars2_14

yeccpars2_38(S, '&&', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_38(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_38(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_38(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_38(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_38(S, '<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_38(S, '<=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_38(S, '<>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_38(S, '=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_38(S, '>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_38(S, '>=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_38(S, 'rem', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_38(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_38_(Stack),
 yeccgoto_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_39_(Stack),
 yeccgoto_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_40(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_40(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_40(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_40(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_40(S, 'rem', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_40(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_40_\'$end\''(Stack),
 yeccgoto_expr(hd(Nss), '$end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_40(_S, '&&', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_40_\'&&\''(Stack),
 yeccgoto_expr(hd(Nss), '&&', Nss, NewStack, T, Ts, Tzr);
yeccpars2_40(_S, ')', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_40_\')\''(Stack),
 yeccgoto_expr(hd(Nss), ')', Nss, NewStack, T, Ts, Tzr);
yeccpars2_40(_S, '<>', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_40_\'<>\''(Stack),
 yeccgoto_expr(hd(Nss), '<>', Nss, NewStack, T, Ts, Tzr);
yeccpars2_40(_S, '=', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_40_\'=\''(Stack),
 yeccgoto_expr(hd(Nss), '=', Nss, NewStack, T, Ts, Tzr);
yeccpars2_40(_S, atom, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_40_atom(Stack),
 yeccgoto_expr(hd(Nss), atom, Nss, NewStack, T, Ts, Tzr);
yeccpars2_40(_S, 'catch', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_40_\'catch\''(Stack),
 yeccgoto_expr(hd(Nss), 'catch', Nss, NewStack, T, Ts, Tzr);
yeccpars2_40(_S, else, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_40_else(Stack),
 yeccgoto_expr(hd(Nss), else, Nss, NewStack, T, Ts, Tzr);
yeccpars2_40(_S, get, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_40_get(Stack),
 yeccgoto_expr(hd(Nss), get, Nss, NewStack, T, Ts, Tzr);
yeccpars2_40(_S, 'if', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_40_\'if\''(Stack),
 yeccgoto_expr(hd(Nss), 'if', Nss, NewStack, T, Ts, Tzr);
yeccpars2_40(_S, new, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_40_new(Stack),
 yeccgoto_expr(hd(Nss), new, Nss, NewStack, T, Ts, Tzr);
yeccpars2_40(_S, 'or', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_40_\'or\''(Stack),
 yeccgoto_expr(hd(Nss), 'or', Nss, NewStack, T, Ts, Tzr);
yeccpars2_40(_S, put, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_40_put(Stack),
 yeccgoto_expr(hd(Nss), put, Nss, NewStack, T, Ts, Tzr);
yeccpars2_40(_S, retry, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_40_retry(Stack),
 yeccgoto_expr(hd(Nss), retry, Nss, NewStack, T, Ts, Tzr);
yeccpars2_40(_S, throw, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_40_throw(Stack),
 yeccgoto_expr(hd(Nss), throw, Nss, NewStack, T, Ts, Tzr);
yeccpars2_40(_S, 'try', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_40_\'try\''(Stack),
 yeccgoto_expr(hd(Nss), 'try', Nss, NewStack, T, Ts, Tzr);
yeccpars2_40(_S, while, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_40_while(Stack),
 yeccgoto_expr(hd(Nss), while, Nss, NewStack, T, Ts, Tzr);
yeccpars2_40(_S, '||', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_40_\'||\''(Stack),
 yeccgoto_expr(hd(Nss), '||', Nss, NewStack, T, Ts, Tzr);
yeccpars2_40(_S, '}', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_40_\'}\''(Stack),
 yeccgoto_expr(hd(Nss), '}', Nss, NewStack, T, Ts, Tzr);
yeccpars2_40(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_41(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_41(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_41(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_41(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_41(S, 'rem', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_41(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_41_\'$end\''(Stack),
 yeccgoto_expr(hd(Nss), '$end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_41(_S, '&&', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_41_\'&&\''(Stack),
 yeccgoto_expr(hd(Nss), '&&', Nss, NewStack, T, Ts, Tzr);
yeccpars2_41(_S, ')', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_41_\')\''(Stack),
 yeccgoto_expr(hd(Nss), ')', Nss, NewStack, T, Ts, Tzr);
yeccpars2_41(_S, '<>', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_41_\'<>\''(Stack),
 yeccgoto_expr(hd(Nss), '<>', Nss, NewStack, T, Ts, Tzr);
yeccpars2_41(_S, '=', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_41_\'=\''(Stack),
 yeccgoto_expr(hd(Nss), '=', Nss, NewStack, T, Ts, Tzr);
yeccpars2_41(_S, atom, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_41_atom(Stack),
 yeccgoto_expr(hd(Nss), atom, Nss, NewStack, T, Ts, Tzr);
yeccpars2_41(_S, 'catch', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_41_\'catch\''(Stack),
 yeccgoto_expr(hd(Nss), 'catch', Nss, NewStack, T, Ts, Tzr);
yeccpars2_41(_S, else, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_41_else(Stack),
 yeccgoto_expr(hd(Nss), else, Nss, NewStack, T, Ts, Tzr);
yeccpars2_41(_S, get, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_41_get(Stack),
 yeccgoto_expr(hd(Nss), get, Nss, NewStack, T, Ts, Tzr);
yeccpars2_41(_S, 'if', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_41_\'if\''(Stack),
 yeccgoto_expr(hd(Nss), 'if', Nss, NewStack, T, Ts, Tzr);
yeccpars2_41(_S, new, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_41_new(Stack),
 yeccgoto_expr(hd(Nss), new, Nss, NewStack, T, Ts, Tzr);
yeccpars2_41(_S, 'or', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_41_\'or\''(Stack),
 yeccgoto_expr(hd(Nss), 'or', Nss, NewStack, T, Ts, Tzr);
yeccpars2_41(_S, put, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_41_put(Stack),
 yeccgoto_expr(hd(Nss), put, Nss, NewStack, T, Ts, Tzr);
yeccpars2_41(_S, retry, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_41_retry(Stack),
 yeccgoto_expr(hd(Nss), retry, Nss, NewStack, T, Ts, Tzr);
yeccpars2_41(_S, throw, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_41_throw(Stack),
 yeccgoto_expr(hd(Nss), throw, Nss, NewStack, T, Ts, Tzr);
yeccpars2_41(_S, 'try', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_41_\'try\''(Stack),
 yeccgoto_expr(hd(Nss), 'try', Nss, NewStack, T, Ts, Tzr);
yeccpars2_41(_S, while, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_41_while(Stack),
 yeccgoto_expr(hd(Nss), while, Nss, NewStack, T, Ts, Tzr);
yeccpars2_41(_S, '||', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_41_\'||\''(Stack),
 yeccgoto_expr(hd(Nss), '||', Nss, NewStack, T, Ts, Tzr);
yeccpars2_41(_S, '}', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_41_\'}\''(Stack),
 yeccgoto_expr(hd(Nss), '}', Nss, NewStack, T, Ts, Tzr);
yeccpars2_41(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_42(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_42(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_42(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_42(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_42(S, '<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_42(S, '<=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_42(S, '>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_42(S, '>=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_42(S, 'rem', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_42(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_42_\'$end\''(Stack),
 yeccgoto_expr(hd(Nss), '$end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_42(_S, '&&', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_42_\'&&\''(Stack),
 yeccgoto_expr(hd(Nss), '&&', Nss, NewStack, T, Ts, Tzr);
yeccpars2_42(_S, ')', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_42_\')\''(Stack),
 yeccgoto_expr(hd(Nss), ')', Nss, NewStack, T, Ts, Tzr);
yeccpars2_42(_S, atom, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_42_atom(Stack),
 yeccgoto_expr(hd(Nss), atom, Nss, NewStack, T, Ts, Tzr);
yeccpars2_42(_S, 'catch', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_42_\'catch\''(Stack),
 yeccgoto_expr(hd(Nss), 'catch', Nss, NewStack, T, Ts, Tzr);
yeccpars2_42(_S, else, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_42_else(Stack),
 yeccgoto_expr(hd(Nss), else, Nss, NewStack, T, Ts, Tzr);
yeccpars2_42(_S, get, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_42_get(Stack),
 yeccgoto_expr(hd(Nss), get, Nss, NewStack, T, Ts, Tzr);
yeccpars2_42(_S, 'if', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_42_\'if\''(Stack),
 yeccgoto_expr(hd(Nss), 'if', Nss, NewStack, T, Ts, Tzr);
yeccpars2_42(_S, new, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_42_new(Stack),
 yeccgoto_expr(hd(Nss), new, Nss, NewStack, T, Ts, Tzr);
yeccpars2_42(_S, 'or', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_42_\'or\''(Stack),
 yeccgoto_expr(hd(Nss), 'or', Nss, NewStack, T, Ts, Tzr);
yeccpars2_42(_S, put, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_42_put(Stack),
 yeccgoto_expr(hd(Nss), put, Nss, NewStack, T, Ts, Tzr);
yeccpars2_42(_S, retry, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_42_retry(Stack),
 yeccgoto_expr(hd(Nss), retry, Nss, NewStack, T, Ts, Tzr);
yeccpars2_42(_S, throw, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_42_throw(Stack),
 yeccgoto_expr(hd(Nss), throw, Nss, NewStack, T, Ts, Tzr);
yeccpars2_42(_S, 'try', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_42_\'try\''(Stack),
 yeccgoto_expr(hd(Nss), 'try', Nss, NewStack, T, Ts, Tzr);
yeccpars2_42(_S, while, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_42_while(Stack),
 yeccgoto_expr(hd(Nss), while, Nss, NewStack, T, Ts, Tzr);
yeccpars2_42(_S, '||', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_42_\'||\''(Stack),
 yeccgoto_expr(hd(Nss), '||', Nss, NewStack, T, Ts, Tzr);
yeccpars2_42(_S, '}', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_42_\'}\''(Stack),
 yeccgoto_expr(hd(Nss), '}', Nss, NewStack, T, Ts, Tzr);
yeccpars2_42(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_43(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_43(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_43(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_43(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_43(S, '<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_43(S, '<=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_43(S, '>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_43(S, '>=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_43(S, 'rem', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_43(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_43_\'$end\''(Stack),
 yeccgoto_expr(hd(Nss), '$end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_43(_S, '&&', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_43_\'&&\''(Stack),
 yeccgoto_expr(hd(Nss), '&&', Nss, NewStack, T, Ts, Tzr);
yeccpars2_43(_S, ')', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_43_\')\''(Stack),
 yeccgoto_expr(hd(Nss), ')', Nss, NewStack, T, Ts, Tzr);
yeccpars2_43(_S, atom, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_43_atom(Stack),
 yeccgoto_expr(hd(Nss), atom, Nss, NewStack, T, Ts, Tzr);
yeccpars2_43(_S, 'catch', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_43_\'catch\''(Stack),
 yeccgoto_expr(hd(Nss), 'catch', Nss, NewStack, T, Ts, Tzr);
yeccpars2_43(_S, else, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_43_else(Stack),
 yeccgoto_expr(hd(Nss), else, Nss, NewStack, T, Ts, Tzr);
yeccpars2_43(_S, get, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_43_get(Stack),
 yeccgoto_expr(hd(Nss), get, Nss, NewStack, T, Ts, Tzr);
yeccpars2_43(_S, 'if', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_43_\'if\''(Stack),
 yeccgoto_expr(hd(Nss), 'if', Nss, NewStack, T, Ts, Tzr);
yeccpars2_43(_S, new, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_43_new(Stack),
 yeccgoto_expr(hd(Nss), new, Nss, NewStack, T, Ts, Tzr);
yeccpars2_43(_S, 'or', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_43_\'or\''(Stack),
 yeccgoto_expr(hd(Nss), 'or', Nss, NewStack, T, Ts, Tzr);
yeccpars2_43(_S, put, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_43_put(Stack),
 yeccgoto_expr(hd(Nss), put, Nss, NewStack, T, Ts, Tzr);
yeccpars2_43(_S, retry, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_43_retry(Stack),
 yeccgoto_expr(hd(Nss), retry, Nss, NewStack, T, Ts, Tzr);
yeccpars2_43(_S, throw, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_43_throw(Stack),
 yeccgoto_expr(hd(Nss), throw, Nss, NewStack, T, Ts, Tzr);
yeccpars2_43(_S, 'try', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_43_\'try\''(Stack),
 yeccgoto_expr(hd(Nss), 'try', Nss, NewStack, T, Ts, Tzr);
yeccpars2_43(_S, while, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_43_while(Stack),
 yeccgoto_expr(hd(Nss), while, Nss, NewStack, T, Ts, Tzr);
yeccpars2_43(_S, '||', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_43_\'||\''(Stack),
 yeccgoto_expr(hd(Nss), '||', Nss, NewStack, T, Ts, Tzr);
yeccpars2_43(_S, '}', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_43_\'}\''(Stack),
 yeccgoto_expr(hd(Nss), '}', Nss, NewStack, T, Ts, Tzr);
yeccpars2_43(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_44(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_44(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_44(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_44(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_44(S, 'rem', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_44(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_44_\'$end\''(Stack),
 yeccgoto_expr(hd(Nss), '$end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_44(_S, '&&', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_44_\'&&\''(Stack),
 yeccgoto_expr(hd(Nss), '&&', Nss, NewStack, T, Ts, Tzr);
yeccpars2_44(_S, ')', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_44_\')\''(Stack),
 yeccgoto_expr(hd(Nss), ')', Nss, NewStack, T, Ts, Tzr);
yeccpars2_44(_S, '<>', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_44_\'<>\''(Stack),
 yeccgoto_expr(hd(Nss), '<>', Nss, NewStack, T, Ts, Tzr);
yeccpars2_44(_S, '=', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_44_\'=\''(Stack),
 yeccgoto_expr(hd(Nss), '=', Nss, NewStack, T, Ts, Tzr);
yeccpars2_44(_S, atom, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_44_atom(Stack),
 yeccgoto_expr(hd(Nss), atom, Nss, NewStack, T, Ts, Tzr);
yeccpars2_44(_S, 'catch', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_44_\'catch\''(Stack),
 yeccgoto_expr(hd(Nss), 'catch', Nss, NewStack, T, Ts, Tzr);
yeccpars2_44(_S, else, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_44_else(Stack),
 yeccgoto_expr(hd(Nss), else, Nss, NewStack, T, Ts, Tzr);
yeccpars2_44(_S, get, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_44_get(Stack),
 yeccgoto_expr(hd(Nss), get, Nss, NewStack, T, Ts, Tzr);
yeccpars2_44(_S, 'if', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_44_\'if\''(Stack),
 yeccgoto_expr(hd(Nss), 'if', Nss, NewStack, T, Ts, Tzr);
yeccpars2_44(_S, new, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_44_new(Stack),
 yeccgoto_expr(hd(Nss), new, Nss, NewStack, T, Ts, Tzr);
yeccpars2_44(_S, 'or', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_44_\'or\''(Stack),
 yeccgoto_expr(hd(Nss), 'or', Nss, NewStack, T, Ts, Tzr);
yeccpars2_44(_S, put, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_44_put(Stack),
 yeccgoto_expr(hd(Nss), put, Nss, NewStack, T, Ts, Tzr);
yeccpars2_44(_S, retry, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_44_retry(Stack),
 yeccgoto_expr(hd(Nss), retry, Nss, NewStack, T, Ts, Tzr);
yeccpars2_44(_S, throw, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_44_throw(Stack),
 yeccgoto_expr(hd(Nss), throw, Nss, NewStack, T, Ts, Tzr);
yeccpars2_44(_S, 'try', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_44_\'try\''(Stack),
 yeccgoto_expr(hd(Nss), 'try', Nss, NewStack, T, Ts, Tzr);
yeccpars2_44(_S, while, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_44_while(Stack),
 yeccgoto_expr(hd(Nss), while, Nss, NewStack, T, Ts, Tzr);
yeccpars2_44(_S, '||', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_44_\'||\''(Stack),
 yeccgoto_expr(hd(Nss), '||', Nss, NewStack, T, Ts, Tzr);
yeccpars2_44(_S, '}', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_44_\'}\''(Stack),
 yeccgoto_expr(hd(Nss), '}', Nss, NewStack, T, Ts, Tzr);
yeccpars2_44(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_45(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_45(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_45(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_45(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_45(S, 'rem', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_45(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_45_\'$end\''(Stack),
 yeccgoto_expr(hd(Nss), '$end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_45(_S, '&&', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_45_\'&&\''(Stack),
 yeccgoto_expr(hd(Nss), '&&', Nss, NewStack, T, Ts, Tzr);
yeccpars2_45(_S, ')', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_45_\')\''(Stack),
 yeccgoto_expr(hd(Nss), ')', Nss, NewStack, T, Ts, Tzr);
yeccpars2_45(_S, '<>', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_45_\'<>\''(Stack),
 yeccgoto_expr(hd(Nss), '<>', Nss, NewStack, T, Ts, Tzr);
yeccpars2_45(_S, '=', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_45_\'=\''(Stack),
 yeccgoto_expr(hd(Nss), '=', Nss, NewStack, T, Ts, Tzr);
yeccpars2_45(_S, atom, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_45_atom(Stack),
 yeccgoto_expr(hd(Nss), atom, Nss, NewStack, T, Ts, Tzr);
yeccpars2_45(_S, 'catch', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_45_\'catch\''(Stack),
 yeccgoto_expr(hd(Nss), 'catch', Nss, NewStack, T, Ts, Tzr);
yeccpars2_45(_S, else, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_45_else(Stack),
 yeccgoto_expr(hd(Nss), else, Nss, NewStack, T, Ts, Tzr);
yeccpars2_45(_S, get, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_45_get(Stack),
 yeccgoto_expr(hd(Nss), get, Nss, NewStack, T, Ts, Tzr);
yeccpars2_45(_S, 'if', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_45_\'if\''(Stack),
 yeccgoto_expr(hd(Nss), 'if', Nss, NewStack, T, Ts, Tzr);
yeccpars2_45(_S, new, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_45_new(Stack),
 yeccgoto_expr(hd(Nss), new, Nss, NewStack, T, Ts, Tzr);
yeccpars2_45(_S, 'or', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_45_\'or\''(Stack),
 yeccgoto_expr(hd(Nss), 'or', Nss, NewStack, T, Ts, Tzr);
yeccpars2_45(_S, put, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_45_put(Stack),
 yeccgoto_expr(hd(Nss), put, Nss, NewStack, T, Ts, Tzr);
yeccpars2_45(_S, retry, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_45_retry(Stack),
 yeccgoto_expr(hd(Nss), retry, Nss, NewStack, T, Ts, Tzr);
yeccpars2_45(_S, throw, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_45_throw(Stack),
 yeccgoto_expr(hd(Nss), throw, Nss, NewStack, T, Ts, Tzr);
yeccpars2_45(_S, 'try', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_45_\'try\''(Stack),
 yeccgoto_expr(hd(Nss), 'try', Nss, NewStack, T, Ts, Tzr);
yeccpars2_45(_S, while, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_45_while(Stack),
 yeccgoto_expr(hd(Nss), while, Nss, NewStack, T, Ts, Tzr);
yeccpars2_45(_S, '||', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_45_\'||\''(Stack),
 yeccgoto_expr(hd(Nss), '||', Nss, NewStack, T, Ts, Tzr);
yeccpars2_45(_S, '}', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_45_\'}\''(Stack),
 yeccgoto_expr(hd(Nss), '}', Nss, NewStack, T, Ts, Tzr);
yeccpars2_45(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_46(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_46_(Stack),
 yeccgoto_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_47(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_47(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_47(S, 'rem', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_47(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_47_(Stack),
 yeccgoto_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_48(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_48(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_48(S, 'rem', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_48(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_48_(Stack),
 yeccgoto_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_49(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_49_(Stack),
 yeccgoto_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_50(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_50(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_50(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_50(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_50(S, '<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_50(S, '<=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_50(S, '<>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_50(S, '=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_50(S, '>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_50(S, '>=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_50(S, 'rem', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_50_(Stack),
 yeccgoto_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_51(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_51(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_16(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_52_(Stack),
 yeccgoto_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_53(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_53(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_53(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_53(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_53(S, '<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_53(S, '<=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_53(S, '<>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_53(S, '=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_53(S, '>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_53(S, '>=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_53(S, 'rem', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_53(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_53_(Stack),
 yeccgoto_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_54(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_54_(Stack),
 yeccgoto_guard(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_55(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_55_(Stack),
 yeccgoto_block(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_56(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_56_(Stack),
 yeccgoto_command(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_57(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_57(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_58/7}).
yeccpars2_58(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_58(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_59(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_59_(Stack),
 yeccgoto_block(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_60(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_60_(Stack),
 yeccgoto_block(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_61/7}).
yeccpars2_61(S, 'catch', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_61(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_62/7}).
yeccpars2_62(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_62(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_62(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_63(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_63_(Stack),
 yeccgoto_command(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_64(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_64_(Stack),
 yeccgoto_exception_block(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_65/7}).
yeccpars2_65(S, ':', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_65(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_66/7}).
yeccpars2_66(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_66(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_66(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_67/7}).
yeccpars2_67(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_67(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_68(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_68(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_68_(Stack),
 yeccgoto_exceptions(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_69(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_69_(Stack),
 yeccgoto_exception_block(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_70(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_70_(Stack),
 yeccgoto_exceptions(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_71(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_71_(Stack),
 yeccgoto_exception_block(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_72: see yeccpars2_7

yeccpars2_73(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_73_(Stack),
 yeccgoto_exception(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_74(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_74_(Stack),
 yeccgoto_command(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_75: see yeccpars2_14

yeccpars2_76(S, '&&', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_76(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_76(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_76(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_76(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_76(S, '<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_76(S, '<=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_76(S, '<>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_76(S, '=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_76(S, '>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_76(S, '>=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_76(S, 'rem', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_76(S, '||', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_76(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_76_(Stack),
 yeccgoto_command(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_77/7}).
yeccpars2_77(S, else, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_77(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_78: see yeccpars2_7

yeccpars2_79(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_79_(Stack),
 yeccgoto_command(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_80: see yeccpars2_14

yeccpars2_81(S, '&&', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_81(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_81(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_81(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_81(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_81(S, '<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_81(S, '<=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_81(S, '<>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_81(S, '=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_81(S, '>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_81(S, '>=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_81(S, 'rem', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_81(S, '||', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_81(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_81_(Stack),
 yeccgoto_command(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_82/7}).
yeccpars2_82(S, then, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 83, Ss, Stack, T, Ts, Tzr);
yeccpars2_82(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_83: see yeccpars2_7

-dialyzer({nowarn_function, yeccpars2_84/7}).
yeccpars2_84(S, else, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 85, Ss, Stack, T, Ts, Tzr);
yeccpars2_84(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_85: see yeccpars2_7

yeccpars2_86(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_86_(Stack),
 yeccgoto_command(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_87(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_87_(Stack),
 yeccgoto_command(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_88(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_88_(Stack),
 yeccgoto_commands(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_block/7}).
yeccgoto_block(7, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_77(77, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_block(11, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(61, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_block(13=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_56(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_block(72=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_73(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_block(78=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_79(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_block(83, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_84(84, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_block(85=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_86(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_command/7}).
yeccgoto_command(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(3, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_command(3, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(3, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_command(7=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_55(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_command(11=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_55(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_command(13=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_55(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_command(57, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(3, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_command(72=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_55(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_command(78=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_55(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_command(83=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_55(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_command(85=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_55(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_commands/7}).
yeccgoto_commands(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_commands(3=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_88(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_commands(57, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_58(58, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_exception/7}).
yeccgoto_exception(62=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_exception(66, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_68(68, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_exception(68, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_68(68, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_exception_block/7}).
yeccgoto_exception_block(62=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_exceptions/7}).
yeccgoto_exceptions(66, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_67(67, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_exceptions(68=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_70(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_expr/7}).
yeccgoto_expr(14, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(16, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(17, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_53(53, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(18, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_51(51, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(19=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_24(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(25, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(50, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(26=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(27, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(48, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(28, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(47, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(29=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(30, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(45, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(31, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(44, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(32, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(43, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(33, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(42, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(34, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(41, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(35, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(40, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(36=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(37, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_38(38, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(75, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_76(76, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(80, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_81(81, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_guard/7}).
yeccgoto_guard(5, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_82(82, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_guard(12, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(13, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_program/7}).
yeccgoto_program(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(1, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_uminus/7}).
yeccgoto_uminus(14=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_uminus(17=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_uminus(18=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_uminus(19=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_uminus(25=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_uminus(26=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_uminus(27=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_uminus(28=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_uminus(29=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_uminus(30=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_uminus(31=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_uminus(32=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_uminus(33=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_uminus(34=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_uminus(35=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_uminus(36=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_uminus(37=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_uminus(75=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_uminus(80=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr).

-compile({inline,yeccpars2_2_/1}).
-file("acid_parser.yrl", 52).
yeccpars2_2_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   to_value ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_3_/1}).
-file("acid_parser.yrl", 54).
yeccpars2_3_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   L = get_line ( __1 ) , { cons , L , __1 , { nil , L } }
  end | __Stack].

-compile({inline,yeccpars2_9_/1}).
-file("acid_parser.yrl", 65).
yeccpars2_9_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { tuple , get_line ( __1 ) , [ to_atom ( __1 ) ] }
  end | __Stack].

-compile({inline,yeccpars2_20_/1}).
-file("acid_parser.yrl", 119).
yeccpars2_20_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { atom , get_line ( __1 ) , false }
  end | __Stack].

-compile({inline,yeccpars2_22_/1}).
-file("acid_parser.yrl", 118).
yeccpars2_22_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { atom , get_line ( __1 ) , true }
  end | __Stack].

-compile({inline,yeccpars2_24_/1}).
-file("acid_parser.yrl", 95).
yeccpars2_24_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '-' , __2 }
  end | __Stack].

-compile({inline,yeccpars2_38_/1}).
-file("acid_parser.yrl", 112).
yeccpars2_38_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , 'orelse' , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_39_/1}).
-file("acid_parser.yrl", 103).
yeccpars2_39_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , 'rem' , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_40_\'$end\''/1}).
-file("acid_parser.yrl", 110).
'yeccpars2_40_\'$end\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '>=' , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_40_\'&&\''/1}).
-file("acid_parser.yrl", 110).
'yeccpars2_40_\'&&\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '>=' , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_40_\')\''/1}).
-file("acid_parser.yrl", 110).
'yeccpars2_40_\')\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '>=' , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_40_\'<>\''/1}).
-file("acid_parser.yrl", 110).
'yeccpars2_40_\'<>\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '>=' , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_40_\'=\''/1}).
-file("acid_parser.yrl", 110).
'yeccpars2_40_\'=\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '>=' , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_40_atom/1}).
-file("acid_parser.yrl", 110).
yeccpars2_40_atom(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '>=' , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_40_\'catch\''/1}).
-file("acid_parser.yrl", 110).
'yeccpars2_40_\'catch\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '>=' , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_40_else/1}).
-file("acid_parser.yrl", 110).
yeccpars2_40_else(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '>=' , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_40_get/1}).
-file("acid_parser.yrl", 110).
yeccpars2_40_get(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '>=' , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_40_\'if\''/1}).
-file("acid_parser.yrl", 110).
'yeccpars2_40_\'if\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '>=' , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_40_new/1}).
-file("acid_parser.yrl", 110).
yeccpars2_40_new(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '>=' , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_40_\'or\''/1}).
-file("acid_parser.yrl", 110).
'yeccpars2_40_\'or\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '>=' , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_40_put/1}).
-file("acid_parser.yrl", 110).
yeccpars2_40_put(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '>=' , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_40_retry/1}).
-file("acid_parser.yrl", 110).
yeccpars2_40_retry(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '>=' , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_40_throw/1}).
-file("acid_parser.yrl", 110).
yeccpars2_40_throw(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '>=' , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_40_\'try\''/1}).
-file("acid_parser.yrl", 110).
'yeccpars2_40_\'try\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '>=' , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_40_while/1}).
-file("acid_parser.yrl", 110).
yeccpars2_40_while(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '>=' , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_40_\'||\''/1}).
-file("acid_parser.yrl", 110).
'yeccpars2_40_\'||\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '>=' , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_40_\'}\''/1}).
-file("acid_parser.yrl", 110).
'yeccpars2_40_\'}\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '>=' , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_41_\'$end\''/1}).
-file("acid_parser.yrl", 109).
'yeccpars2_41_\'$end\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '>' , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_41_\'&&\''/1}).
-file("acid_parser.yrl", 109).
'yeccpars2_41_\'&&\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '>' , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_41_\')\''/1}).
-file("acid_parser.yrl", 109).
'yeccpars2_41_\')\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '>' , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_41_\'<>\''/1}).
-file("acid_parser.yrl", 109).
'yeccpars2_41_\'<>\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '>' , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_41_\'=\''/1}).
-file("acid_parser.yrl", 109).
'yeccpars2_41_\'=\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '>' , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_41_atom/1}).
-file("acid_parser.yrl", 109).
yeccpars2_41_atom(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '>' , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_41_\'catch\''/1}).
-file("acid_parser.yrl", 109).
'yeccpars2_41_\'catch\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '>' , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_41_else/1}).
-file("acid_parser.yrl", 109).
yeccpars2_41_else(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '>' , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_41_get/1}).
-file("acid_parser.yrl", 109).
yeccpars2_41_get(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '>' , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_41_\'if\''/1}).
-file("acid_parser.yrl", 109).
'yeccpars2_41_\'if\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '>' , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_41_new/1}).
-file("acid_parser.yrl", 109).
yeccpars2_41_new(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '>' , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_41_\'or\''/1}).
-file("acid_parser.yrl", 109).
'yeccpars2_41_\'or\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '>' , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_41_put/1}).
-file("acid_parser.yrl", 109).
yeccpars2_41_put(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '>' , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_41_retry/1}).
-file("acid_parser.yrl", 109).
yeccpars2_41_retry(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '>' , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_41_throw/1}).
-file("acid_parser.yrl", 109).
yeccpars2_41_throw(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '>' , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_41_\'try\''/1}).
-file("acid_parser.yrl", 109).
'yeccpars2_41_\'try\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '>' , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_41_while/1}).
-file("acid_parser.yrl", 109).
yeccpars2_41_while(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '>' , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_41_\'||\''/1}).
-file("acid_parser.yrl", 109).
'yeccpars2_41_\'||\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '>' , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_41_\'}\''/1}).
-file("acid_parser.yrl", 109).
'yeccpars2_41_\'}\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '>' , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_42_\'$end\''/1}).
-file("acid_parser.yrl", 105).
'yeccpars2_42_\'$end\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '=:=' , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_42_\'&&\''/1}).
-file("acid_parser.yrl", 105).
'yeccpars2_42_\'&&\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '=:=' , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_42_\')\''/1}).
-file("acid_parser.yrl", 105).
'yeccpars2_42_\')\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '=:=' , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_42_atom/1}).
-file("acid_parser.yrl", 105).
yeccpars2_42_atom(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '=:=' , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_42_\'catch\''/1}).
-file("acid_parser.yrl", 105).
'yeccpars2_42_\'catch\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '=:=' , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_42_else/1}).
-file("acid_parser.yrl", 105).
yeccpars2_42_else(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '=:=' , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_42_get/1}).
-file("acid_parser.yrl", 105).
yeccpars2_42_get(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '=:=' , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_42_\'if\''/1}).
-file("acid_parser.yrl", 105).
'yeccpars2_42_\'if\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '=:=' , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_42_new/1}).
-file("acid_parser.yrl", 105).
yeccpars2_42_new(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '=:=' , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_42_\'or\''/1}).
-file("acid_parser.yrl", 105).
'yeccpars2_42_\'or\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '=:=' , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_42_put/1}).
-file("acid_parser.yrl", 105).
yeccpars2_42_put(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '=:=' , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_42_retry/1}).
-file("acid_parser.yrl", 105).
yeccpars2_42_retry(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '=:=' , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_42_throw/1}).
-file("acid_parser.yrl", 105).
yeccpars2_42_throw(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '=:=' , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_42_\'try\''/1}).
-file("acid_parser.yrl", 105).
'yeccpars2_42_\'try\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '=:=' , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_42_while/1}).
-file("acid_parser.yrl", 105).
yeccpars2_42_while(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '=:=' , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_42_\'||\''/1}).
-file("acid_parser.yrl", 105).
'yeccpars2_42_\'||\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '=:=' , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_42_\'}\''/1}).
-file("acid_parser.yrl", 105).
'yeccpars2_42_\'}\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '=:=' , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_43_\'$end\''/1}).
-file("acid_parser.yrl", 106).
'yeccpars2_43_\'$end\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '=/=' , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_43_\'&&\''/1}).
-file("acid_parser.yrl", 106).
'yeccpars2_43_\'&&\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '=/=' , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_43_\')\''/1}).
-file("acid_parser.yrl", 106).
'yeccpars2_43_\')\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '=/=' , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_43_atom/1}).
-file("acid_parser.yrl", 106).
yeccpars2_43_atom(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '=/=' , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_43_\'catch\''/1}).
-file("acid_parser.yrl", 106).
'yeccpars2_43_\'catch\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '=/=' , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_43_else/1}).
-file("acid_parser.yrl", 106).
yeccpars2_43_else(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '=/=' , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_43_get/1}).
-file("acid_parser.yrl", 106).
yeccpars2_43_get(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '=/=' , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_43_\'if\''/1}).
-file("acid_parser.yrl", 106).
'yeccpars2_43_\'if\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '=/=' , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_43_new/1}).
-file("acid_parser.yrl", 106).
yeccpars2_43_new(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '=/=' , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_43_\'or\''/1}).
-file("acid_parser.yrl", 106).
'yeccpars2_43_\'or\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '=/=' , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_43_put/1}).
-file("acid_parser.yrl", 106).
yeccpars2_43_put(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '=/=' , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_43_retry/1}).
-file("acid_parser.yrl", 106).
yeccpars2_43_retry(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '=/=' , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_43_throw/1}).
-file("acid_parser.yrl", 106).
yeccpars2_43_throw(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '=/=' , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_43_\'try\''/1}).
-file("acid_parser.yrl", 106).
'yeccpars2_43_\'try\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '=/=' , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_43_while/1}).
-file("acid_parser.yrl", 106).
yeccpars2_43_while(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '=/=' , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_43_\'||\''/1}).
-file("acid_parser.yrl", 106).
'yeccpars2_43_\'||\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '=/=' , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_43_\'}\''/1}).
-file("acid_parser.yrl", 106).
'yeccpars2_43_\'}\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '=/=' , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_44_\'$end\''/1}).
-file("acid_parser.yrl", 108).
'yeccpars2_44_\'$end\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '=<' , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_44_\'&&\''/1}).
-file("acid_parser.yrl", 108).
'yeccpars2_44_\'&&\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '=<' , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_44_\')\''/1}).
-file("acid_parser.yrl", 108).
'yeccpars2_44_\')\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '=<' , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_44_\'<>\''/1}).
-file("acid_parser.yrl", 108).
'yeccpars2_44_\'<>\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '=<' , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_44_\'=\''/1}).
-file("acid_parser.yrl", 108).
'yeccpars2_44_\'=\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '=<' , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_44_atom/1}).
-file("acid_parser.yrl", 108).
yeccpars2_44_atom(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '=<' , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_44_\'catch\''/1}).
-file("acid_parser.yrl", 108).
'yeccpars2_44_\'catch\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '=<' , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_44_else/1}).
-file("acid_parser.yrl", 108).
yeccpars2_44_else(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '=<' , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_44_get/1}).
-file("acid_parser.yrl", 108).
yeccpars2_44_get(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '=<' , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_44_\'if\''/1}).
-file("acid_parser.yrl", 108).
'yeccpars2_44_\'if\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '=<' , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_44_new/1}).
-file("acid_parser.yrl", 108).
yeccpars2_44_new(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '=<' , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_44_\'or\''/1}).
-file("acid_parser.yrl", 108).
'yeccpars2_44_\'or\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '=<' , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_44_put/1}).
-file("acid_parser.yrl", 108).
yeccpars2_44_put(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '=<' , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_44_retry/1}).
-file("acid_parser.yrl", 108).
yeccpars2_44_retry(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '=<' , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_44_throw/1}).
-file("acid_parser.yrl", 108).
yeccpars2_44_throw(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '=<' , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_44_\'try\''/1}).
-file("acid_parser.yrl", 108).
'yeccpars2_44_\'try\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '=<' , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_44_while/1}).
-file("acid_parser.yrl", 108).
yeccpars2_44_while(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '=<' , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_44_\'||\''/1}).
-file("acid_parser.yrl", 108).
'yeccpars2_44_\'||\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '=<' , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_44_\'}\''/1}).
-file("acid_parser.yrl", 108).
'yeccpars2_44_\'}\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '=<' , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_45_\'$end\''/1}).
-file("acid_parser.yrl", 107).
'yeccpars2_45_\'$end\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '<' , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_45_\'&&\''/1}).
-file("acid_parser.yrl", 107).
'yeccpars2_45_\'&&\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '<' , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_45_\')\''/1}).
-file("acid_parser.yrl", 107).
'yeccpars2_45_\')\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '<' , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_45_\'<>\''/1}).
-file("acid_parser.yrl", 107).
'yeccpars2_45_\'<>\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '<' , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_45_\'=\''/1}).
-file("acid_parser.yrl", 107).
'yeccpars2_45_\'=\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '<' , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_45_atom/1}).
-file("acid_parser.yrl", 107).
yeccpars2_45_atom(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '<' , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_45_\'catch\''/1}).
-file("acid_parser.yrl", 107).
'yeccpars2_45_\'catch\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '<' , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_45_else/1}).
-file("acid_parser.yrl", 107).
yeccpars2_45_else(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '<' , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_45_get/1}).
-file("acid_parser.yrl", 107).
yeccpars2_45_get(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '<' , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_45_\'if\''/1}).
-file("acid_parser.yrl", 107).
'yeccpars2_45_\'if\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '<' , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_45_new/1}).
-file("acid_parser.yrl", 107).
yeccpars2_45_new(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '<' , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_45_\'or\''/1}).
-file("acid_parser.yrl", 107).
'yeccpars2_45_\'or\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '<' , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_45_put/1}).
-file("acid_parser.yrl", 107).
yeccpars2_45_put(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '<' , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_45_retry/1}).
-file("acid_parser.yrl", 107).
yeccpars2_45_retry(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '<' , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_45_throw/1}).
-file("acid_parser.yrl", 107).
yeccpars2_45_throw(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '<' , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_45_\'try\''/1}).
-file("acid_parser.yrl", 107).
'yeccpars2_45_\'try\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '<' , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_45_while/1}).
-file("acid_parser.yrl", 107).
yeccpars2_45_while(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '<' , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_45_\'||\''/1}).
-file("acid_parser.yrl", 107).
'yeccpars2_45_\'||\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '<' , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_45_\'}\''/1}).
-file("acid_parser.yrl", 107).
'yeccpars2_45_\'}\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '<' , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_46_/1}).
-file("acid_parser.yrl", 102).
yeccpars2_46_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , 'div' , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_47_/1}).
-file("acid_parser.yrl", 100).
yeccpars2_47_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '-' , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_48_/1}).
-file("acid_parser.yrl", 99).
yeccpars2_48_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '+' , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_49_/1}).
-file("acid_parser.yrl", 101).
yeccpars2_49_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , '*' , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_50_/1}).
-file("acid_parser.yrl", 113).
yeccpars2_50_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , 'andalso' , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_52_/1}).
-file("acid_parser.yrl", 115).
yeccpars2_52_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_53_/1}).
-file("acid_parser.yrl", 97).
yeccpars2_53_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { op , get_line ( __1 ) , 'not' , __2 }
  end | __Stack].

-compile({inline,yeccpars2_54_/1}).
-file("acid_parser.yrl", 83).
yeccpars2_54_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_55_/1}).
-file("acid_parser.yrl", 80).
yeccpars2_55_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   L = get_line ( __1 ) , { cons , L , __1 , { nil , L } }
  end | __Stack].

-compile({inline,yeccpars2_56_/1}).
-file("acid_parser.yrl", 71).
yeccpars2_56_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { Fun , Args } = unapply ( __2 ) ,
    { tuple , get_line ( __1 ) , [ to_atom ( __1 ) , Fun , Args , __3 ] }
  end | __Stack].

-compile({inline,yeccpars2_59_/1}).
-file("acid_parser.yrl", 79).
yeccpars2_59_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { nil , get_line ( __1 ) }
  end | __Stack].

-compile({inline,yeccpars2_60_/1}).
-file("acid_parser.yrl", 81).
yeccpars2_60_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_63_/1}).
-file("acid_parser.yrl", 69).
yeccpars2_63_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { tuple , get_line ( __1 ) , [ to_atom ( __1 , try_catch ) , __2 , __4 ] }
  end | __Stack].

-compile({inline,yeccpars2_64_/1}).
-file("acid_parser.yrl", 86).
yeccpars2_64_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   L = get_line ( __1 ) , { cons , L , __1 , { nil , L } }
  end | __Stack].

-compile({inline,yeccpars2_68_/1}).
-file("acid_parser.yrl", 89).
yeccpars2_68_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   L = get_line ( __1 ) , { cons , L , __1 , { nil , L } }
  end | __Stack].

-compile({inline,yeccpars2_69_/1}).
-file("acid_parser.yrl", 85).
yeccpars2_69_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { nil , get_line ( __1 ) }
  end | __Stack].

-compile({inline,yeccpars2_70_/1}).
-file("acid_parser.yrl", 90).
yeccpars2_70_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { cons , get_line ( __1 ) , __1 , __2 }
  end | __Stack].

-compile({inline,yeccpars2_71_/1}).
-file("acid_parser.yrl", 87).
yeccpars2_71_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_73_/1}).
-file("acid_parser.yrl", 92).
yeccpars2_73_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { tuple , get_line ( __1 ) , [ __1 , __3 ] }
  end | __Stack].

-compile({inline,yeccpars2_74_/1}).
-file("acid_parser.yrl", 67).
yeccpars2_74_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { tuple , get_line ( __1 ) , [ to_atom ( __1 , toss ) , __2 , { nil , get_line ( __1 ) } ] }
  end | __Stack].

-compile({inline,yeccpars2_76_/1}).
-file("acid_parser.yrl", 60).
yeccpars2_76_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { Fun , Args } = unapply ( __3 ) ,
    { tuple , get_line ( __1 ) , [ to_atom ( __1 ) , to_AST ( __2 ) , Fun , Args ] }
  end | __Stack].

-compile({inline,yeccpars2_79_/1}).
-file("acid_parser.yrl", 77).
yeccpars2_79_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { tuple , get_line ( __1 ) , [ to_atom ( __1 , or_else ) , __2 , __4 ] }
  end | __Stack].

-compile({inline,yeccpars2_81_/1}).
-file("acid_parser.yrl", 57).
yeccpars2_81_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { Fun , Args } = unapply ( __3 ) ,
    { tuple , get_line ( __1 ) , [ to_atom ( __1 ) , to_AST ( __2 ) , Fun , Args ] }
  end | __Stack].

-compile({inline,yeccpars2_86_/1}).
-file("acid_parser.yrl", 74).
yeccpars2_86_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { Fun , Args } = unapply ( __2 ) ,
    { tuple , get_line ( __1 ) , [ to_atom ( __1 , if_else ) , Fun , Args , __4 , __6 ] }
  end | __Stack].

-compile({inline,yeccpars2_87_/1}).
-file("acid_parser.yrl", 63).
yeccpars2_87_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { tuple , get_line ( __1 ) , [ to_atom ( __1 ) , to_AST ( __2 ) ] }
  end | __Stack].

-compile({inline,yeccpars2_88_/1}).
-file("acid_parser.yrl", 55).
yeccpars2_88_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { cons , get_line ( __1 ) , __1 , __2 }
  end | __Stack].


-file("acid_parser.yrl", 195).
