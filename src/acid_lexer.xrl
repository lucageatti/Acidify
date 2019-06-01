
%Header
%"%%"
%"%% @authors:"
%"%%      Luca Geatti <geatti.luca@spes.uniud.it>"
%"%%      Federico Igne <igne.federico@spes.uniud.it>"
%"%% "
%"%% @supervisor:"
%"%%      Marino Miculan <>"
%"%%"
%"%% Dipartimento di Scienze Matematiche, Informatiche e Fisiche (DMIF)"
%"%% University of Udine - 33100 Udine (UD) - Italy"
%"%%".

Definitions.

I  = [0-9]+
A  = [a-z_]+
O  = (\(|\)|\{|\}|;|:|\|\||&&|=|<>|<|>|<=|>=|!|\+|-|\*|/|rem)
V  = @[A-Za-z0-9\{\}"<>,]+
WS = ([\000-\s]|%.*)

Rules.

NEW		: {token,{new,TokenLine}}.
PUT		: {token,{put,TokenLine}}.
GET     : {token,{get,TokenLine}}.
RETRY   : {token,{retry,TokenLine}}.
THROW	: {token,{'throw',TokenLine}}.
TRY		: {token,{'try',TokenLine}}.
CATCH	: {token,{'catch',TokenLine}}.
WHILE	: {token,{while,TokenLine}}.
IF		: {token,{'if',TokenLine}}.
THEN	: {token,{then,TokenLine}}.
ELSE	: {token,{else,TokenLine}}.
OR		: {token,{'or',TokenLine}}.

true 	: {token,{'true',TokenLine}}.
false 	: {token,{'false',TokenLine}}.

{I}		: {token,{integer,TokenLine,list_to_integer(TokenChars)}}.
{O}		: {token,{list_to_atom(TokenChars),TokenLine}}.
{A}     : {token,{atom,TokenLine,list_to_atom(TokenChars)}}.
% '{A}'   : {token,{atom,TokenLine,list_to_atom(lists:droplast(tl(TokenChars)))}}.
{V}     : {token,{tvar,TokenLine,tl(TokenChars)++"."}}.
{WS}+   : skip_token.

Erlang code.
