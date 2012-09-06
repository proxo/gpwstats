-module(gpwstats).
-author("Piotr Maśko").
-export([start/0,stop/0,fetch_stocks/0]).
-import(gpwstats_fetch_server,[fetch_stocks/1]).

start()->
	io:format("Starting application ~n"),
	ensure_started(crypto),
	ensure_started(public_key),
    	ensure_started(ssl),
	ensure_started(ibrowse),
	ensure_started(gpwstats).

stop()->
	application:stop(crypto),
	application:stop(public_key),
   	application:start(ssl),
	application:stop(ibrowse),
	application:stop(gpwstats).

fetch_stocks()->
	gpwstats_fetch_server:fetch_stocks(["WAS"]).

ensure_started(App)->
	case application:start(App) of
		ok-> ok;
		{error,{already_started,_}} -> ok
	end.

