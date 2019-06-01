
Header
"%%"
"%% @authors:"
"%%      Luca Geatti <geatti.luca@spes.uniud.it>"
"%%      Federico Igne <igne.federico@spes.uniud.it>"
"%% "
"%% @supervisor:"
"%%      Marino Miculan <>"
"%%"
"%% Dipartimento di Scienze Matematiche, Informatiche e Fisiche (DMIF)"
"%% University of Udine - 33100 Udine (UD) - Italy"
"%%".

Nonterminals
	program
	% transactions transaction
	block exception_block
	commands command
	expr guard uminus
	exceptions exception.

Terminals
	% trans
	atom tvar integer true false 
	new put get retry throw try catch while if then else or % trans_call
	'(' ')' '{' '}' ':'
	'||' '&&' '=' '<>' '<' '>' '<=' '>=' '!' '+' '-' '*' '/' 'rem'.

Rootsymbol program.

% Endsymbol '$end'.


Left 100 '||'.
Left 200 '&&'.

Unary 300 '!'.

Nonassoc 400 '=' '<>'.
Nonassoc 500 '<' '>' '<=' '>='.

Left 600 '+' '-'.
Left 700 '*' '/' 'rem'.

Unary 800 uminus.


% program -> transactions.

% transactions -> transaction.
% transactions -> transaction transactions.

% transaction -> trans atom block.

program -> commands : to_value('$1').

commands -> command : L = get_line('$1'), {cons, L, '$1', {nil, L}}.
commands -> command commands : {cons, get_line('$1'), '$1', '$2'}.

command -> new tvar expr : {Fun,Args} = unapply('$3'),
                           {tuple, get_line('$1'), [to_atom('$1'), to_AST('$2'), Fun, Args]}.

command -> put tvar expr : {Fun,Args} = unapply('$3'),
                           {tuple, get_line('$1'), [to_atom('$1'), to_AST('$2'), Fun, Args]}.

command -> get tvar : {tuple, get_line('$1'), [to_atom('$1'), to_AST('$2')]}.

command -> retry : {tuple, get_line('$1'), [to_atom('$1')]}.

command -> throw atom : {tuple, get_line('$1'), [to_atom('$1','toss'), '$2', {nil, get_line('$1')}]}.

command -> try block catch exception_block : {tuple, get_line('$1'), [to_atom('$1','try_catch'), '$2', '$4']}.

command -> while guard block : {Fun,Args} = unapply('$2'),
                               {tuple, get_line('$1'), [to_atom('$1'), Fun, Args, '$3']}.

command -> if guard then block else block : {Fun,Args} = unapply('$2'),
                                            {tuple, get_line('$1'), [to_atom('$1','if_else'), Fun, Args, '$4', '$6']}.

command -> or block else block : {tuple, get_line('$1'), [to_atom('$1', 'or_else'), '$2', '$4']}.

block -> '{' '}' : {nil, get_line('$1')}.
block -> command : L = get_line('$1'), {cons, L, '$1', {nil, L}}.
block -> '{' commands '}' : '$2'.

guard -> '(' expr ')' : '$2'.

exception_block -> '{' '}' : {nil, get_line('$1')}.
exception_block -> exception : L = get_line('$1'), {cons, L, '$1', {nil, L}}.
exception_block -> '{' exceptions '}' : '$2'.

exceptions -> exception : L = get_line('$1'), {cons, L, '$1', {nil, L}}.
exceptions -> exception exceptions : {cons, get_line('$1'), '$1', '$2'}.

exception -> atom ':' block : {tuple, get_line('$1'), ['$1', '$3']}.

expr -> uminus : '$1'.
uminus -> '-' expr : {op, get_line('$1'), '-', '$2'}.

expr -> '!' expr : {op, get_line('$1'), 'not', '$2'}.

expr -> expr '+'   expr : {op, get_line('$1'), '+', '$1', '$3'}.
expr -> expr '-'   expr : {op, get_line('$1'), '-', '$1', '$3'}.
expr -> expr '*'   expr : {op, get_line('$1'), '*', '$1', '$3'}.
expr -> expr '/'   expr : {op, get_line('$1'), 'div', '$1', '$3'}.
expr -> expr 'rem' expr : {op, get_line('$1'), 'rem', '$1', '$3'}.

expr -> expr '='  expr : {op, get_line('$1'), '=:=', '$1', '$3'}.
expr -> expr '<>' expr : {op, get_line('$1'), '=/=', '$1', '$3'}.
expr -> expr '<'  expr : {op, get_line('$1'), '<', '$1', '$3'}.
expr -> expr '<=' expr : {op, get_line('$1'), '=<', '$1', '$3'}.
expr -> expr '>'  expr : {op, get_line('$1'), '>', '$1', '$3'}.
expr -> expr '>=' expr : {op, get_line('$1'), '>=', '$1', '$3'}.

expr -> expr '||' expr : {op, get_line('$1'), 'orelse', '$1', '$3'}.
expr -> expr '&&' expr : {op, get_line('$1'), 'andalso', '$1', '$3'}.

expr -> '(' expr ')' : '$2'.
expr -> tvar : '$1'.
expr -> integer : '$1'.
expr -> true : {atom, get_line('$1'), true}.
expr -> false : {atom, get_line('$1'), false}.

Erlang code.

get_line(T) -> erlang:element(2,T).


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