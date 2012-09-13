-module(gpwstats_web_resource).
-author("Piotr Maśko").

%% webmachine exports
-export([
    init/1,
    to_html/2
  ]).

%% Helper macro for declaring children of supervisor
-ifdef(debug).
-define(TRACE(X),io:format("TRACE ~p:~p ~p~n",[?MODULE,?LINE,X])).
-else.
-define(TRACE(X),void).
-endif.

%% Includes
-include_lib("webmachine/include/webmachine.hrl").


init([])->{ok,undefined}.

to_html(ReqData,State)->
	StockData = gpwstats_fetch_server:fetch_stocks([]),
	StockDataList = make_pretty_line(StockData),
	{ok,Content} = index_dtl:render([{name,"Piotr"},{stocks,StockDataList}]),
	{Content,ReqData,State}.


make_pretty_line(StockData)->
	[string:join([ "<td>" ++ map_term_to_name(StatName,StatVal) ++ "</td>" || {StatName,StatVal} <- D ], "  ") || D <- StockData ].


map_term_to_name(stock,Val)-> Val;
map_term_to_name(avg,Val)-> mochinum:digits(Val);
map_term_to_name(open,Val)-> mochinum:digits(Val);
map_term_to_name(close,Val)-> mochinum:digits(Val);
map_term_to_name(diffPercent,V)-> mochinum:digits(V);
map_term_to_name(min,V)-> mochinum:digits(V);
map_term_to_name(max,V)-> mochinum:digits(V);
map_term_to_name(_Any,_V)-> io:format("any clause: ~p ~p~n",[_Any,_V]). 
