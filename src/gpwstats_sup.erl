-module(gpwstats_sup).
-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Args), {I, {I, start_link,[Args]}, permanent, 5000, Type, [I]}).
-ifdef(debug).
-define(TRACE(X),io:format("TRACE ~p:~p ~p~n",[?MODULE,?LINE,X])).
-else.
-define(TRACE(X),void).
-endif.
%% ===================================================================
%% API functions
%% ===================================================================

start_link(AppConfig) ->
	io:format("Starting supervisor ~p~n",[AppConfig]),
    supervisor:start_link({local, ?MODULE}, ?MODULE, [AppConfig]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(_AppConfig) ->
	io:format("Staring supervising child with config: ~p~n",[_AppConfig]),
	Server = {gpwstats_fetch_server,{gpwstats_fetch_server,start_link,[]},permanent,5000,worker,dynamic},
	Childs = [Server], 
	?TRACE(Server),
    {ok, { {one_for_one, 5, 10},Childs} }.
