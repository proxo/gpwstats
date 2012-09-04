-module(gpwstats_sup).
-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Args), {I, {I, start_link,Args}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(AppConfig) ->
	io:format("Starting supervisor ~p~n",[AppConfig]),
    supervisor:start_link({local, ?MODULE}, ?MODULE, [AppConfig]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([_AppConfig]) ->
	io:format("Staring child~n"),
	Server = ?CHILD(gpwstats_fetch_server,worker,_AppConfig),
    {ok, { {one_for_one, 5, 10}, [Server]} }.

