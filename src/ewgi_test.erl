-module(ewgi_test).

-export([test/0]).

test() ->
    lists:map(fun test_file/1,
              [filename:rootname(filename:basename(F))
               || F <- filelib:wildcard("ebin/*.beam")]).

test_file("ewgi_test") ->
    skip;
test_file(F) ->
    case code:ensure_loaded(list_to_atom(F)) of
        {module, M} ->
            test_mod(M);
        _ ->
            skip
    end.

test_mod(M) ->
    case M:module_info() of
        L when is_list(L) ->
            Exports = proplists:get_value(exports, L, []),
            case lists:member({test,0}, Exports) of
                true ->
                    M:test();
                _ ->
                    ok
            end;
        _ ->
            ok
    end.
