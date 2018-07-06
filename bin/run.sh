erl -pa _build/default/lib/*/ebin \
    +K true \
    -noshell \
    -eval 'httpc_bench:run(), init:stop().'
