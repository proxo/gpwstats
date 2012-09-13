-module(gpwstats_app).
-author("Piotr Maśko").
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).
-define(APP,gpwstats).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	AllEnv = application:get_all_env(?APP),
	%% io:format("Using env: ~p~n",[AllEnv]),
	R = gpwstats_sup:start_link(AllEnv),
	%% io:format("start app: ~p~n", [R]),
	R.

stop(_State) ->
    ok.
