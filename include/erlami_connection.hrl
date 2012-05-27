-record(erlami_connection, {
    send = fun(_) -> erlang:error("Not implemented") end,
    close = fun() -> erlang:error("Not implemented") end
}).
