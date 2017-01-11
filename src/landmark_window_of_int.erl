-module(landmark_window_of_int).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% In the book "Knowledage Discovery from Data Streams" the landmark window approach was introduced in the section 2.2.5,
%% Fig. 2.4. This program implement a landmark window with elements of type integer.

-include("../include/data.hrl").
-include("../include/query.hrl").
-record(state, { next = 1 :: integer(),
	         elements = dict:new() :: dict:dict() %% a dictionary of {Int, #data{}}
	       }).

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

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
    {ok, #state{}}.

handle_call(#data{ type= int, value= Value }= Data, _from, State) when is_integer(Value) ->
    State1 = append(Data, State),
    {reply, ok, State1};
handle_call(#query{ term= count }, _from, #state{}= State) ->
    {reply, State#state.next - 1, State};
handle_call(#query{ term= inspect }, _from, State) ->
    {reply, State, State};
handle_call(Request, _From, State) ->
    {reply, {drop, Request}, State}.

handle_cast(#data{ type= int, value= Value }= Data, State) when is_integer(Value) ->
    State1 = append(Data, State),
    {noreply, State1};
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

append(Data, #state{}= State) ->
    #state{ next = State#state.next + 1,
	    elements = dict:append(State#state.next, Data, State#state.elements) }.
