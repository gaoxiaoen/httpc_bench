-module(httpc_bench).
-include("httpc_bench.hrl").

-export([
         run_ibrowse/0,
         run/0, run/3, run/4, run_concurency/4
        ]).

-define(N, 4000).

-define(CLIENTS, [
                  httpc_bench_hackney,
                  httpc_bench_ibrowse
                 ]).

run_ibrowse() ->
    error_logger:tty(false),
    io:format("Running benchmark...~n~n" ++
                  "Client  PoolSize  Concurency  Requests/s  Error %~n" ++
                  [$= || _ <- lists:seq(1, 49)] ++ "~n", []),
    [run_concurency(httpc_bench_ibrowse, 1 bsl I, 1 bsl I, ?N) ||
        I <- lists:seq(1,10)],
    ok.

%% public
run() ->
    N = erlang:system_info(schedulers),
    PoolSizes = [N div 2, N, N * 2, N * 4],
    MaxPoolSizes = lists:max(PoolSizes),
    Concurencies = [MaxPoolSizes, MaxPoolSizes*2, MaxPoolSizes*4],
    run(?CLIENTS, PoolSizes, Concurencies, ?N).

run(Clients, PoolSizes, Concurencies) ->
    run(Clients, PoolSizes, Concurencies, ?N).

%% private
lookup(Key, List) ->
    case lists:keyfind(Key, 1, List) of
        false -> undefined;
        {_, Value} -> Value
    end.

name(Client, PoolSize, Concurency) ->
    list_to_atom(Client ++ "_" ++ integer_to_list(PoolSize) ++
                     "_" ++ integer_to_list(Concurency)).

run(Clients, PoolSizes, Concurencies, N) ->
    error_logger:tty(false),
    io:format("Running benchmark...~n~n" ++
                  "Client  PoolSize  Concurency  Requests/s  Error %~n" ++
                  [$= || _ <- lists:seq(1, 49)] ++ "~n", []),
    lists:foldl(
      fun(Client, _) ->
              lists:foldl(
                fun(PoolSize, _) ->
                        lists:foldl(
                          fun(Concurency, _) ->
                                  run_concurency(Client, PoolSize, Concurency, N)
                          end, ok, Concurencies)
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
