-module(gpwstats_sup).
-behaviour(supervisor).

%% API
-export([start_link/1, start/0]).

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

start()->
			spawn(fun()-> supervisor:start_link({local,?MODULE}, ?MODULE, _Arg = []) end).
start_link(AppConfig) ->
	io:format("Starting supervisor ~p~n",[AppConfig]),
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	Server = {gpwstats_fetch_server,{gpwstats_fetch_server,start_link,[]},permanent,5000,worker,[gpwstats_fetch_server]},
	Childs = [Server], 
	?TRACE(Server),
    {ok, { {one_for_one,10,10},Childs} }.
