-module(listener).

-behaviour(supervisor).

-export([start_link/2]).

-export([init/1]).


-define(WORKERS, 1).

start_link(Port, SimModule)->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Port, SimModule]).

init([Port, SimModule]) ->
    {ok, Listen} = gen_tcp:listen(Port, [{active, once},{packet, 4}]),
    io:format("INFO: tcp listener start ~n"),
    % start works
    ChildList = make_workers(?WORKERS, Listen, SimModule),
    Super = #{
            strategy => one_for_one,
            intensity => 5,
            period => 1
        },
    {ok, {Super,ChildList}}.


make_workers(Num, Listen, SM) ->
    make_workers(Num, [], Listen, SM).
make_workers(0, ChildList, _Listen, _) -> ChildList;
make_workers(Num, ChildList, Listen, SM) ->
    Child = #{
            id => Num,
            start => {transports, start_worker, [Listen, Num, SM]},
            type => worker
        },
    NewChildList = [Child | ChildList],
    make_workers(Num-1, NewChildList, Listen, SM).