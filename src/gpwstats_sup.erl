-module(gpwstats_sup).
-behaviour(supervisor).
-author("Piotr Maśko").
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
	%% io:format("Starting supervisor ~p~n",[AppConfig]),
    supervisor:start_link({local, ?MODULE}, ?MODULE, [AppConfig]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([AppConfig]) ->
	GpwServer = {gpwstats_fetch_server,{gpwstats_fetch_server,start_link,[[AppConfig]]},permanent,5000,worker,[gpwstats_fetch_server]},
	%% from app.config fetch web section
	WebConfig = proplists:get_value(web,AppConfig),
  	Web = {webmachine_mochiweb, {webmachine_mochiweb, start, [WebConfig]}, permanent, 5000, worker, dynamic},
  	Processes = [GpwServer, Web],
	%% maximum of 5 restarts per 20 seconds
    {ok, { {one_for_all,5,20},Processes} }.
