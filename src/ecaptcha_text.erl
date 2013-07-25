-module(ecaptcha_text).

-export([
    new/1,
    check/2
    ]).

-define(CryptKey, <<"e5b3ac76-e389-4f21-8fc7-5548cdee72fa">>). %FIXME configure key

%%TODO init

-spec new(Length::non_neg_integer()) -> {CodeHex::term(),BinPng::binary()}.

new(Length) when Length < 1 ->
    new(1);
new(Length) ->
    FileName = lists:flatmap(fun(Item) -> integer_to_list(Item) end, tuple_to_list(os:timestamp())),

    Code = generate_rand(Length),
    File = io_lib:format("/tmp/~s.png", [FileName]),

    Cmd = io_lib:format("convert -background 'none' -fill '#222222' -size 175 -gravity Center -wave 5x100 -swirl 50 -font DejaVu-Serif-Book -pointsize 28 label:~s -draw 'Bezier 10,40 50,35 100,35 150,35 200,50 250,35 300,35' ~s", [Code, File]),
    os:cmd(Cmd),

    {ok, BinPng} = file:read_file(File),
    file:delete(File),

    Sha = crypto:hmac(sha, ?CryptKey, integer_to_list(lists:sum(Code)) ++ Code), %%TODO use JWT
    CodeHex = mochihex:to_hex(Sha), %%FIXME dont user mochi
    {CodeHex, BinPng}.

check(CodeHex, Code) ->
    Sha = mochihex:to_bin(CodeHex), %%FIXME dont user mochi
    CryptKey = ets:first(captcha),

    case crypto:hmac(sha, CryptKey, integer_to_list(lists:sum(Code)) ++ Code) of
        Sha ->
            true;
        _ ->
            false
    end.

generate_rand(Length) ->
    {A1,A2,A3} = os:timestamp(),
    random:seed(A1, A2, A3),
    lists:foldl(fun(_I, Acc) -> [do_rand(0) | Acc] end, [], lists:seq(1, Length)).  

do_rand(R) when R > 46, R < 58; R > 64, R < 91; R > 96 ->
    R;
do_rand(_R) ->
    do_rand(47 + random:uniform(75)).