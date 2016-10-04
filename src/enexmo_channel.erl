-module(enexmo_channel).

%% API
-export([init/0]).
-export([add/2]).
-export([get/2]).

-record(channel, {
    id,
    api_key             = "",
    api_secret          = "",
    application_id      = "",
    private_key         = "",
    signature_secret    = ""
}).

%% API
init() ->
    Opts = [
        named_table,
        public,
        {read_concurrency, true},
        {keypos, #channel.id}
    ],
    _ = ets:new(?MODULE, Opts),
    ok.

add(ChanelId, Opts) ->
    APIKey          = proplists:get_value(api_key, Opts, ""),
    APISecret       = proplists:get_value(api_secret, Opts, ""),
    ApplicationId   = proplists:get_value(application_id, Opts, ""),
    PrivateKey      = proplists:get_value(private_key, Opts, ""),
    SignatureSecret = proplists:get_value(signature_secret, Opts, ""),
    Rec = #channel{
        id              = ChanelId,
        api_key         = unicode:characters_to_binary(APIKey),
        api_secret      = unicode:characters_to_binary(APISecret),
        application_id  = unicode:characters_to_binary(ApplicationId),
        private_key     = unicode:characters_to_binary(PrivateKey),
        signature_secret= unicode:characters_to_binary(SignatureSecret)
    },
    _ = ets:insert(?MODULE, Rec),
    ok.

get(ChanelId, ListOfOpts) ->
    case ets:lookup(?MODULE, ChanelId) of
        []      -> [];
        [Rec]   -> get_options(Rec, ListOfOpts)
    end.

%% internal
get_options(Rec, ListOfOpts) ->
    get_options(Rec, ListOfOpts, []).

get_options(Rec, [api_key|T], Acc) ->
    get_options(Rec, T, [{api_key, Rec#channel.api_key}|Acc]);
get_options(Rec, [api_secret|T], Acc) ->
    get_options(Rec, T, [{api_secret, Rec#channel.api_secret}|Acc]);
get_options(Rec, [application_id|T], Acc) ->
    get_options(Rec, T, [{application_id, Rec#channel.application_id}|Acc]);
get_options(Rec, [private_key|T], Acc) ->
    get_options(Rec, T, [{private_key, Rec#channel.private_key}|Acc]);
get_options(Rec, [signature_secret|T], Acc) ->
    get_options(Rec, T, [{signature_secret, Rec#channel.signature_secret}|Acc]);
get_options(Rec, [_|T], Acc) ->
    get_options(Rec, T, Acc);
get_options(_Rec, [], Acc) ->
    Acc.

