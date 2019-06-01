
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

-module(acid).
-behaviour(application).

-export([ start/2, stop/1 ]).

start(normal, _Args)
  -> {ok, Servers} = application:get_env(servers), 
     acid_manager:start(Servers,[]).

stop(_State)
  -> erlang:exit(acid, normal).