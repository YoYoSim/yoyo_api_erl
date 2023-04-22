-module(simple_sim).

-behaviour(yoyo_api_erl).

% yoyo_api_erl API
-export([init/1]).
-export([step/3]).
-export([get_data/2]).

%API
-export([start_sim/1]).


-define(SID, <<"Sid_1">>).

-record(sim_state, {
    a = 0 :: number(),
    b = 1 :: number()
}).

%% yoyo_api_erl

init(_Args)->
    io:format("INFO: sim init ~n"),
    % create State
    State = #sim_state{},
    {ok, State}.

step(State, Time, _InputValue) ->
    #sim_state{a=A, b=B} = State,
    io:format("INFO: create a number ~p ~n", [A]),
    NewA = B,
    NewB = A + B,
    NewState = State#sim_state{a=NewA, b=NewB},
    {Time+1, NewState}.

get_data(State, _OutPut) ->
    {[], State}.

% API
start_sim(Port) ->
    yoyo_api_erl:start(Port, ?MODULE, ?SID).

