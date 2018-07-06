-module(httpc_bench).
-include("httpc_bench.hrl").

-export([         
         run/0, run/2, run/3, run_concurency/4
        ]).

-define(N, 30000).

-define(CLIENTS, [
                  httpc_bench_hackney,
                  httpc_bench_ibrowse
                 ]).

%% public
run() ->
    PoolSizes =  [ 1 bsl I  || I <- lists:seq(2,10)],
    run(?CLIENTS, PoolSizes).

run(Clients, PoolSizes) ->
    run(Clients, PoolSizes, ?N).

%% private
lookup(Key, List) ->
    case lists:keyfind(Key, 1, List) of
        false -> undefined;
        {_, Value} -> Value
    end.

name(Client, PoolSize, Concurency) ->
    list_to_atom(Client ++ "_" ++ integer_to_list(PoolSize) ++
                     "_" ++ integer_to_list(Concurency)).

run(Clients, PoolSizes, N) ->
    error_logger:tty(false),
    io:format("Running benchmark...~n~n" ++
                  "Client  PoolSize  Concurency  Requests/s  Error %~n" ++
                  [$= || _ <- lists:seq(1, 49)] ++ "~n", []),
    lists:foldl(
      fun(Client, _) ->
              lists:foldl(
                fun(PoolSize, _) ->
                          run_concurency(Client, PoolSize+1, PoolSize, N)                          
                end, ok, PoolSizes)
      end, ok, Clients).    



run_concurency(Client, PoolSize, Concurency, N) ->
    Client:start(PoolSize),
    {_Prefix, Client2} = lists:split(12, atom_to_list(Client)),
    Name = name(Client2, PoolSize, Concurency),
    Fun = fun() -> Client:get() end,
    Results = timing_hdr:run(Fun, [
                                   {name, Name},
                                   {concurrency, Concurency},
                                   {iterations, N},
                                   {output, "output/" ++ atom_to_list(Name)}
                                  ]),
    Qps = lookup(success, Results) / (lookup(total_time, Results) / 1000000),
    Errors = lookup(errors, Results) / lookup(iterations, Results) * 100,
    io:format("~-8s ~7B ~11B ~11B ~8.1f~n",
              [Client2, PoolSize, Concurency, trunc(Qps), Errors]),
    Client:stop().
