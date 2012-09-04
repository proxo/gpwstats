-module(gpwstats_fetch_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-record(state, {link_per_day,last_fetch_time}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

fetch_stock(StockName, Pid)->
	fetch_stocks([{StockName,1}]).

fetch_stocks(StocksList)->
	gen_server:call(?SERVER,{fetch_stocks,StocksList}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
%%
%%
init(Args) ->
    {ok, #state{link_per_day=Args, last_fetch_time=undefined}.

handle_call({fetch_stocks,StocksList},_From,State)->
	
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

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

