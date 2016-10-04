-module(enexmo_client).

%% API
-export([get/2, get/3, get/4]).
-export([post/3, post/4]).

-define(DEFAULT_TIMEOUT, 5000).
-define(HEADERS, [
    {"User-Agent", "enexmo/1.0"},   %% TODO
    {"Accept", "application/json;charset=UTF-8"}
]).

%% API
get(Host, Path) ->
    get(Host, Path, []).

get(Host, Path, Params) ->
    get(Host, Path, Params, ?DEFAULT_TIMEOUT).

get(Host, Path, _Params, Timeout) ->
    HttpOpts = [{timeout, Timeout}],
    URI = unicode:characters_to_list(<<"https://", Host/binary, Path/binary>>),
    ReqParams = {URI, ?HEADERS},
    Resp = httpc:request(get, ReqParams, HttpOpts, [{body_format, binary}]),
    handle_resp(URI, Resp).

post(Host, Path, Params) ->
    post(Host, Path, Params, ?DEFAULT_TIMEOUT).

post(Host, Path, Params, Timeout) ->
    HttpOpts = [{timeout, Timeout}],
    URI = unicode:characters_to_list(<<"https://", Host/binary, Path/binary>>),
    ReqBody = jsx:encode(Params),
    ReqParams = {URI, ?HEADERS, "application/json;charset=UTF-8", ReqBody},
    Resp = httpc:request(post, ReqParams, HttpOpts, [{body_format, binary}]),
    handle_resp(URI, Resp).


%% internal
handle_resp(URI, Resp) ->
    case Resp of
        {ok, {{_, 401, _}, _Headers, _RespBody}} ->
            {error, {client_error, authentication_error}};
        {ok, {{_, 204, _}, _Headers, _RespBody}} ->
            {ok, []};
        {ok, {{_, Status, _}, _Headers, RespBody}}
                when (200 =< Status) and (Status < 300) ->
            try jsx:decode(RespBody) of
                Term -> {ok, Term}
            catch
                _:_ ->  {error, {client_error, invalid_response_format}}
            end;
        {ok, {{_, Status, _}, _Headers, _RespBody}}
                when (400 =< Status) and (Status < 500) ->
            ErrorDesc = <<(integer_to_binary(Status))/binary,
                " response from ", URI/binary>>,
            {error, {client_error, ErrorDesc}};
        {ok, {{_, Status, _}, _Headers, _RespBody}}
                when (500 =< Status) and (Status < 600) ->
            ErrorDesc = <<(integer_to_binary(Status))/binary,
                " response from ", URI/binary>>,
            {error, {server_error, ErrorDesc}};
        {error, Reason} ->
            {error, {client_error, Reason}}
    end.


