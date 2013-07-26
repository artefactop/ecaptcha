-module(ecaptcha_test).
-include_lib("eunit/include/eunit.hrl").

-define(CryptKey, <<"e5b3ac76-e389-4f21-8fc7-5548cdee72fa">>).

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
        fun start/0,
        fun() -> [
            new_captcha_images_test(),
            check_captcha_images_test(),
            new_captcha_text_test(),
            check_captcha_text_test()
        ] end
    }.

start() ->
    ?meck_lager(),
    AppFile = code:where_is_file(atom_to_list(ecaptcha) ++ ".app"),
    {ok, [{application, _, List}]} = file:consult(AppFile),
    Apps = [erlsha2, jsx] ++ proplists:get_value(applications, List, []),
    lists:foreach(
        fun(X) ->
             ok = case application:start(X) of
                {error, {already_started, X}} -> ok;
                Ret -> Ret
             end
        end,
        Apps
    ),
    application:start(ecaptcha).

new_captcha_images_test() ->
    Test = case ecaptcha_image:new(2, <<"en">>) of 
        [{<<"token">>,_},{<<"text">>,_},{<<"images">>,[_A,_B]}] -> true;
        _ -> false 
    end,
    ?assertEqual(true, Test).

check_captcha_images_test() ->
    P = [{<<"valid">>, 0}, {<<"noise">>, random:uniform(1000000) }],
    JWT = ejwt:encode(P, ?CryptKey), 
    Test = ecaptcha_image:check(JWT, 0), 
    ?assertEqual(true, Test).

new_captcha_text_test() ->
    Test = case ecaptcha_text:new(5) of 
        {Token, _Base64Png} -> 
            case ejwt:decode(Token, ?CryptKey) of 
                error -> false;
                _ -> true
            end;
        _ -> false 
    end,
    ?assertEqual(true, Test).

check_captcha_text_test() ->
    P = [{<<"code">>, <<"abCd">>}, {<<"noise">>, random:uniform(1000000) }],
    JWT = ejwt:encode(P, ?CryptKey), 
    Test = ecaptcha_text:check(JWT, <<"abCd">>), 
    ?assertEqual(true, Test).