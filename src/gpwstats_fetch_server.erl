%% -*- tab-width: 4;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 ft=erlang et

-module(gpwstats_fetch_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).


%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% -----------------------------------------------------------------

-export([start_link/1,fetch_stock/2,fetch_stocks/1,stop/0,init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%% ------------------------------------------------------------------
%% Macros
%% ------------------------------------------------------------------
% trace calls
-ifdef(debug).
-define(TRACE(X,ARGS),io:format("TRACE ~p:~p " ++ X ++ "~n",[?MODULE,?LINE|ARGS])).
-else.
-define(TRACE(X,ARGS),void).
-endif.

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
-record(state, {config,link_per_day,last_fetch_time}).

start_link(Data) ->
    ?TRACE("Starting gen_server - fetch_server",[]),
    FetchConfig = application:get_all_env(gpwstats),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [FetchConfig],[]).

fetch_stock(StockName, Pid)->
	fetch_stocks([{StockName,1}]).

fetch_stocks(StocksList)->
	gen_server:call(?MODULE,{fetch_stocks,StocksList},infinity).

stop()->
    gen_server:cast(?MODULE,stop).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
%%
%%
init([FetchConfig]) ->
    ?TRACE("Initial config ~p",[FetchConfig]),
    {ok, #state{config=FetchConfig,last_fetch_time=undefined}}.

handle_call({fetch_stocks,StocksList},_From,State=#state{config=Config,last_fetch_time=undefined})->
	FileUrlPrefix = proplists:get_value(stock_url,Config),
	FileUrlSuffix =proplists:get_value(stock_suffix,Config),
	FileUrls = lists:map(fun(Day)-> FileUrlPrefix ++ "_" ++ Day ++ FileUrlSuffix end, proplists:get_value(stock_days,Config)),
	MyStocks = proplists:get_value(mystocks,Config),
	Ref = make_ref(),
	ChildPids = lists:map(fun(X)-> spawn_link(?MODULE,fetch_single_file,[self(),Ref,X,MyStocks]) end,FileUrls),
	NumToReceive=length(ChildPids),
	AllData = receive_responses(NumToReceive,Ref,[]),
	{reply,prepare_stats(AllData),#state{config=Config,last_fetch_time=erlang:localtime()}};
	
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

receive_responses(NumToReceive,Ref,ResponseList)->
	if NumToReceive < 0;NumToReceive =:= 0 -> ResponseList;
		true ->
		%% erlang else
		receive
			{ok,Ref,Data} -> receive_responses(NumToReceive-1,Ref,[Data|ResponseList]);
			_Any -> io:format("Unknown message: ~p~n",[_Any]), ResponseList
		end
	end.
		 

%%
%% Fetch single file from url 
fetch_single_file(ParentPid,Ref,FileUrl,MyStocks) when is_pid(ParentPid)->
	?TRACE("Fetching file from: ~p",[FileUrl]),
	{ok,_Status,_Headers,Data} = ibrowse:send_req(FileUrl,[], get),
	Lines = string:tokens(Data,"\r\n"),
	?TRACE("Processed ~p lines for file: ~p",[length(Lines), FileUrl]),
	StockSet = lists:foldl(fun(X,Set)-> {StockName,_} = X, gb_sets:add_element(StockName,Set) end, gb_sets:new(), MyStocks),
	LinesFiltered = lists:filter(fun(L)-> gb_sets:is_element(hd(string:tokens(L,",")),StockSet) end, Lines),
	?TRACE("Filtered lines ~p file:~p",[length(LinesFiltered), FileUrl]),
	ResultData = [string:tokens(L,",") || L <- LinesFiltered],	ResultData = [string:tokens(L,",") || L <- LinesFiltered],
	%% Give day in year as a first element in tupple
	ParentPid ! {ok, Ref, {hd(tl(hd(ResultData))), ResultData}},
	exit(normal).

%% Data has format {day,[StockName,OpenValue,CloseValue,_]}
prepare_stats([])->
	[];
prepare_stats(AllData)->
	SortedByDay = lists:sort(fun(A,B) -> element(1,A) < element(1,B) end, AllData),
	StockNames = [ StockName || [StockName|_Rest] <- element(2,hd(SortedByDay))],
	[[StockName,{avg,calculate_average(StockName,SortedByDay)}] || StockName <- StockNames]	
.

calculate_average(StockName,Data)->
	D = [hd(X) || X <- lists:map(fun({_Day, Vals})-> lists:filter(fun(V)-> hd(V) =:= StockName end,Vals) end, Data)],
	%% take third element from list - close price
	D2 = [hd(tl(tl(DX))) || DX <- D],
	lists:foldl(fun(X,Sum)-> my_to_float(X) + Sum end,0.0,D2) / length(D).
		
	
my_to_float(N)->
    case string:to_float(N) of
        {error,no_float} -> list_to_integer(N);
        {F,_Rest} -> F
    end.
	

handle_cast(stop,_State)->
    {stop,ok,_State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

