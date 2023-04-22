-module(yoyo_api_erl).

-export([start/3]).


%% simulator  API

-callback init(Args::list()) -> {ok, State::term()}| error.
-callback step(State::term(), Time::number(), InputValue::list()) -> {NextTime::number(), State::term()}.
-callback get_data(State::term(), OutPutValue::list()) -> {OutPut::list(), State::term()}.


%% API

start(Port, SimModule, Sid) ->
    yoyo = ets:new(yoyo, [set, public, named_table]),
    init_table(Sid),
    listener:start_link(Port, SimModule).

%% Internal.

init_table(Sid) ->
    ets:insert(yoyo, {<<"sid">>, Sid}),
    ets:insert(yoyo, {<<"ID">>, 0}).