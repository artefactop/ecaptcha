-module(ecaptcha_image).

-export([
    new/2,
    check/2
    ]).

-define(CryptKey, <<"e5b3ac76-e389-4f21-8fc7-5548cdee72fa">>). %FIXME configure key, get_env

-define(GRAVITY, ["NorthWest", "North", "NorthEast", "West", "Center", "East", "SouthWest", "South", "SouthEast"]).

-spec new(NumberElements::non_neg_integer(), Lang::binary()) -> list().

new(NumberElements, Lang) when NumberElements < 2 ->
    new(2, Lang);

new(NumberElements, Lang) -> %%TODO return [{token,TOKEN},{text","select the cat"}, {"images",[<<bin1>>, <<bin2>>, <<bin3>>]}] 
    Base = code:priv_dir(ecaptcha),
    case file:consult(Base ++ "/captcha.config") of 
        {ok, [Config]} -> 
            lager:debug("Config captcha ~p",[Config]),
            WM = proplists:get_value(<<"watermark">>, Config),
            Images = proplists:get_value(<<"images">>, Config), 
            lager:debug("Images captcha ~p",[Images]),
            List = random(NumberElements, Images),
            lager:debug("List captcha ~p",[List]),
            BinImages = lists:foldl(fun(X, Acc) -> 
                lager:debug("X ~p",[X]),
                I = proplists:get_value(<<"img">>, X),
                BinPng = apply_watermark(<<(list_to_binary(Base))/binary,"/captcha_images/",WM/binary>>, <<(list_to_binary(Base))/binary,"/captcha_images/", I/binary>>),
                [ BinPng | Acc ] 
            end , [], List),
            Rand = random:uniform(length(List)),
            Text = get_text(Lang, proplists:get_value(<<"lang">>, lists:nth(Rand, List))), 
            Token = ejwt:encode([{<<"valid">>, Rand}], ?CryptKey),
            [{<<"token">>, Token}, {<<"text">>, Text}, {<<"images">>, BinImages}];
        Error -> Error 
    end.

get_text(Lang, List) ->
    case proplists:get_value(Lang, List) of 
        undefined ->
            proplists:get_value(<<"en">>, List, <<"unknown">>);
        T -> T 
    end.

-spec apply_watermark(Watermark::file:name_all(), Image::file:name_all()) -> binary().

apply_watermark(Watermark, Image) ->
    FileName = lists:flatmap(fun(Item) -> integer_to_list(Item) end, tuple_to_list(os:timestamp())),
    File = io_lib:format("/tmp/~s.png",[FileName]),
    lager:debug("Filename ~p",[FileName]),
    [Gv] = random(1, ?GRAVITY),
    lager:debug("composite -dissolve 50% -gravity ~s -quality 100 \\( ~s -resize 50% \\) ~s ~s", [Gv, Watermark, Image, File]),

    Cmd = io_lib:format("composite -dissolve 50% -gravity ~s -quality 100 \\( ~s -resize 50% \\) ~s ~s", [Gv, Watermark, Image, File]),
    os:cmd(Cmd),

    {ok, BinPng} = file:read_file(File),
    file:delete(File),
    base64:encode(BinPng).

-spec check(JWT::binary(), Position::non_neg_integer()) -> boolean().

check(JWT, Position) ->
    Payload = ejwt:decode(JWT, ?CryptKey), 
    case proplists:get_value(<<"valid">>, Payload) of 
        Position ->
            true;
        _ ->
            false 
    end. 


-spec random(N::non_neg_integer(), List::list()) -> list().

random(N, List) ->
    random(List, N, []).

-spec random(L::list(), N::non_neg_integer(), Acc::list()) -> list().

random([],_,Acc) ->
    Acc;
random(_,0,Acc) ->
    Acc;
random(L, N, Acc) ->
    {A1,A2,A3} = os:timestamp(),
    random:seed(A1, A2, A3),
    Rand = random:uniform(length(L)),
    E = lists:nth(Rand, L),
    random(delete_element(Rand, L), N-1, Acc ++ [E]).

-spec delete_element(N::non_neg_integer(), L::list()) -> list().

delete_element(N,L) -> %% change for erlang:delete_element
    Ele = lists:nth(N, L),
    lists:filter(fun(X) -> X =/= Ele end, L). %%FIXME remove all elements not only the first ocurrence 