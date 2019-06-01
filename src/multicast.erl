%%
%% @authors:
%%      Luca Geatti <geatti.luca@spes.uniud.it>
%%      Federico Igne <igne.federico@spes.uniud.it>
%% 
%% @supervisor:
%%      Marino Miculan <>
%% 
%% Dipartimento di scienze Matematiche, Informatiche e Fisiche (DMIF)
%% University of Udine - 33100 Udine (UD) - Italy
%%

-module(multicast).

-export([create_group/1, is_group/1, send_to_group/2, join_group/1, leave_group/1]).

-type name() :: any().



%% @doc Creates a group called 'Name'
-spec create_group(Name :: name()) -> ok.
create_group(Name) -> pg2:create(Name).



%% @doc Checks if 'Name' is a group. Returns 'true' is this is the case, 'false' otherwise
-spec is_group(Name :: name()) -> boolean().
is_group(Name) -> case pg2:get_members(Name) of
					{error, {no_such_group, Name}} -> false;
					_ -> true
				  end.



%% @doc Calls the function 'Fun' (which is a cast of the module acid) for all the members of the group 'Name'.
%% The function 'Fun' has to be of arity 1: its only parameter must be a Pid.
%% N.B.: in Erlang, if we want to carry the function f(a,b,c), we can write: fun(X) -> f(a,b,X) end.
-spec send_to_group(Fun :: fun((pid()) -> ok), Name :: any()) -> {ok, [pid()]} | {error, {no_such_group, name()}}.
send_to_group(Fun, Name) -> case pg2:get_members(Name) of % Members is a list of PIDs
								 {error, {no_such_group, Name}} -> {error, {no_such_group, Name}};
								 Members -> Members1 = lists:delete(self(), Members),
								 			[ Fun(Member) || Member <- Members1],
										    {ok, Members1}
							end.



%% @doc Joins the process 'Pid' to the group 'Name'
-spec join_group(Name :: name()) -> ok | {error, {no_such_group, name()}}.
join_group(Name) -> pg2:join(Name, self()).



%% @doc Makes the process 'Pid' leave the group 'Name'
-spec leave_group(Name :: name()) -> ok | {error, {no_such_group, name()}}.
leave_group(Name) -> pg2:leave(Name, self()).





