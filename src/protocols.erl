-module(protocols).

-export([read_data/1]).
-export([send_data/4]).
-export([handle_data/3]).

%% API
read_data(Data) ->
    ErlData = from_bin_to_erl(Data),
    [_ID, Type, Content] = ErlData,
    if Type =:= 0 ->
        [Fun, Args] = Content,
        {Fun, Args};
    true ->
        throw(<<"msg_type_error">>)
    end.

send_data(Socket, Type, ID, Content) ->
    D = jiff:encode([ID, Type, Content]),
    gen_tcp:send(Socket, D).


from_bin_to_erl(Data) ->
    jiffy:decode(Data).

handle_data(Data, Sock, M) ->
    {Fun, Args} = read_data(Data),
    case Fun of
        <<"init">> ->
            sims_func(init, M, Args, Sock);
        <<"step">> ->
            sims_func(step, M, Args, Sock);
        <<"get_data">> ->
            sims_func(get_data, M, Args, Sock)
    end.

%% Internal

sims_func(init, M, Args, Sock) ->
    {ok, State} = apply(M, init, Args),
    save_state(State),
    ID = get_id(),
    send_data(Sock, 1, ID, []);
sims_func(step, M, Args, Sock) ->
    State = get_state(),
    {T, State1} = apply(M, step, [State | Args]),
    save_state(State1),
    ID = get_id(),
    send_data(Sock, 1, ID, [T]);
sims_func(get_data, M, Args, Sock) ->
    State = get_state(),
    {OutPut, State1} = apply(M, get_data, [State | Args]),
    save_state(State1),
    ID = get_id(),
    send_data(Sock, 1, ID, [OutPut]).



get_state() ->
    [{<<"State">>, State}] = ets:lookup(yoyo, <<"State">>),
    State.

save_state(State) ->
    ets:insert(yoyo, {<<"State">>, State}).

get_id() ->
    [{<<"ID">>, ID}] = ets:lookup(yoyo, <<"ID">>),
    ets:insert(yoyo, {<<"ID">>, ID+1}),
    ID.