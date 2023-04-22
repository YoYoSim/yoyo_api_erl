-module(transports).

-export([start_worker/3]).


start_worker(Socket, ID, Module) ->
    Pid = spawn(fun() -> accepter(Socket, ID, Module) end),
    {ok, Pid}.


accepter(Listen, ID, Module) ->
    io:format("INFO: listen ~p is listening ~n", [ID]),
    {ok, Socket} = gen_tcp:accept(Listen),
    Controller = spawn(fun() -> receiver(Socket, Module) end),
    ok = gen_tcp:controlling_process(Socket, Controller),
    accepter(Listen, ID, Module). 


receiver(Socket, M) ->
    receive
        {tcp, Socket, Data} ->
            protocols:handle_data(Data, Socket, M),
            inet:setopts(Socket, [{active, once}, 
                {packet, 4}]),
            receiver(Socket, M);
        {tcp_closed, Socket} ->
            io:format("INFO: socket closed  ~p ~n", [[Socket]]),
            io:format("INFO: receiver process end ~n")
    end.

