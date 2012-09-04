-module(gpwstats_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).
-define(APP,gpwstats).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    gpwstats_sup:start_link(application:get_all_env(?APP)).

stop(_State) ->
    ok.
