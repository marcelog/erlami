-record(erlami_connection, {
    read_line = fun(_Timeout) -> erlang:error("Not implemented") end,
    send = fun(_) -> erlang:error("Not implemented") end,
    close = fun() -> erlang:error("Not implemented") end
}).
