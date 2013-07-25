ecaptcha
========

[![Build Status](https://api.travis-ci.org/artefactop/ecaptcha.png)](https://travis-ci.org/artefactop/ecaptcha)

Library for provide captchas


## Dependencies
`imagemagick`

## index ##

* [quickstart](#quickstart)
* [ecaptcha_text](#ecaptcha_text)
  - [`new/1`](#new)
  - [`check/2`](#check)
* [ecaptcha_image](#ecaptcha_image)
  - [`new/2`](#new)
  - [`check/2`](#check)

- - - 

## quickstart

To build the library and run tests

```bash
$ rebar compile
$ rebar eunit
```

## ecaptcha_image

### `new/2`

```-spec new(NumberElements::non_neg_integer(), Lang::binary()) -> list().```

```erlang
1> ecaptcha_image:new(2,<<"en">>).
[{<<"token">>, Token},{<<"text">>,<<"Where is Calamardo?">>},{<<"images">>,[Base64,Base64]}]
```

### `check/2`

When a human select the an image, you check if is correct sending Token and the position selected of the images list.

```-spec check(JWT::binary(), Position::non_neg_integer()) -> boolean().```

```erlang
1> ecaptcha_image:check(Token, 0).
true
2> ecaptcha_image:check(Token, 1).
false
```