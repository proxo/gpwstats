-module(gpwstats).
-author("Piotr Maśko").
-export([start/0,stop/0]).

start()->
	io:format("Starting application ~n"),
	ensure_started(crypto),
	ensure_started(public_key),
	ensure_started(ssl),
	ensure_started(ibrowse),
	ensure_started(inets),
	ensure_started(mochiweb),
	application:set_env(webmachine, webmachine_logger_module, webmachine_logger),
	ensure_started(webmachine),
	ensure_started(gpwstats),
	ok.

stop()->
	application:stop(gpwstats),
	application:stop(webmachine),
	application:stop(mochiweb),
	application:stop(inets),
	application:stop(crypto),
	application:stop(public_key),
   	application:start(ssl),
	application:stop(ibrowse).

ensure_started(App)->
	case application:start(App) of
		ok-> ok;
		{error,{already_started,_}} -> ok;
		_Any -> io:format("Got strange start result: ~p ~p~n",[App,_Any])
	end.

