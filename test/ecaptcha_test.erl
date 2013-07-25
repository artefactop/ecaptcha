-module(ecaptcha_test).
-include_lib("eunit/include/eunit.hrl").

-define(meck_lager(), begin
    meck:new(lager),
    meck:expect(lager, dispatch_log, fun(_Severity, _Metadata, _Format, _Args, _Size) ->
        %?debugFmt(_Format, _Args),
        ok
    end),
    meck:expect(lager, dispatch_log, fun(_Severity, _Module, _Function, _Line, _Pid, _Traces, _Format, _Args, _TruncSize) ->
        %?debugFmt(_Format, _Args),
        ok
    end)
end).

setup_test_() ->
    {setup,
        fun run_meck/0,
        fun() -> [
            new_captcha_images_test(),
            check_captcha_images_test()
        ] end
    }.

run_meck() ->
    ?meck_lager().

new_captcha_images_test() ->
    Test = case ecaptcha_image:new(2, <<"en">>) of 
        [{<<"token">>,_},{<<"text">>,_},{<<"images">>,[_A,_B]}] -> true;
        _ -> false 
    end,
    ?assertEqual(true, Test).

check_captcha_images_test() ->
    P = [{<<"valid">>, 0}],
    JWT = ejwt:encode(P, <<"e5b3ac76-e389-4f21-8fc7-5548cdee72fa">>), 
    Test = ecaptcha_image:check(JWT, 0), 
    ?assertEqual(true, Test).