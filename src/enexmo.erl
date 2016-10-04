-module(enexmo).

%% API
-export([create_channel/2]).
-export([drop_channel/1]).
-export([send_message/4]).
-export([start_verification/3]).
-export([check_verification/3]).
-export([cancel_verification/2]).

%% API
create_channel(ChanelId, Opts) ->
    enexmo_channel:add(ChanelId, Opts).

drop_channel(ChanelId) ->
    enexmo_channel:drop(ChanelId).

send_message(ChanelId, From, To, Text) ->
    Host    = application:get_env(enexmo, host, "rest.nexmo.com"),
    HostBin = unicode:characters_to_binary(Host),
    BinFrom = unicode:characters_to_binary(From),
    BinTo   = unicode:characters_to_binary(To),
    BinText = unicode:characters_to_binary(Text),
    Auth    = enexmo_channel:get(ChanelId, [api_key, api_secret]),
    Params  = [{from, BinFrom}, {to, BinTo}, {text, BinText}|Auth],
    Result  = enexmo_client:post(HostBin, <<"/sms/json">>, Params),
    parse_response(send_message, Result).

%get_balance(ChanelId) ->
%    enexmo_client:request({get_balance, ChanelId}).

%get_country_pricing
%
%get_prefix_pricing
%
%get_sms_pricing
%
%get_voice_pricing
%
%update_settings
%
%topup
%
%get_account_numbers
%
%get_available_numbers
%
%buy_number
%
%cancel_number
%
%update_number
%
%get_message
%
%get_message_rejections
%
%search_messages
%
%send_ussd_push_message
%
%send_ussd_prompt_message
%
%send_2fa_message
%
%send_event_alert_message
%
%send_marketing_message
%
%get_event_alert_numbers
%
%resubscribe_event_alert_number
%
%initiate_call
%
%initiate_tts_call
%
%initiate_tts_prompt_call

start_verification(ChanelId, Number, Brand) ->
    Host        = application:get_env(enexmo, api_host, "api.nexmo.com"),
    HostBin     = unicode:characters_to_binary(Host),
    BinNumber   = unicode:characters_to_binary(Number),
    BinBrand    = unicode:characters_to_binary(Brand),
    Auth        = enexmo_channel:get(ChanelId, [api_key, api_secret]),
    Params      = [{number, BinNumber}, {brand, BinBrand}|Auth],
    Result      = enexmo_client:post(HostBin, <<"/verify/json">>, Params),
    parse_response(start_verification, Result).

check_verification(ChanelId, RequestId, Code) ->
    Host        = application:get_env(enexmo, api_host, "api.nexmo.com"),
    HostBin     = unicode:characters_to_binary(Host),
    BinRequestId= unicode:characters_to_binary(RequestId),
    BinCode     = unicode:characters_to_binary(Code),
    Auth        = enexmo_channel:get(ChanelId, [api_key, api_secret]),
    Params      = [{request_id, BinRequestId}, {code, BinCode}|Auth],
    Result      = enexmo_client:post(HostBin, <<"/verify/check/json">>, Params),
    parse_response(check_verification, Result).

%get_verification

cancel_verification(ChanelId, RequestId) ->
    enexmo_client:request({cancel_verification, ChanelId, RequestId}).

%trigger_next_verification_event
%
%get_basic_number_insight
%
%get_number_insight
%
%request_number_insight
%
%get_applications
%
%get_application
%
%create_application
%
%update_application
%
%delete_application
%
%create_call
%
%get_calls
%
%get_call
%
%update_call
%
%check_signature
%
%signature


%% internal
parse_response(send_message, {ok, Resp}) ->
    [Message] = proplists:get_value(<<"messages">>, Resp, [[]]),
    case proplists:get_value(<<"status">>, Message) of
        <<"0">> ->
            {ok, Message};
        Code ->
            ErrorDesc = proplists:get_value(<<"error-text">>, Message),
            {error, {server_error, Code, ErrorDesc}}
    end;
parse_response(start_verification, {ok, Resp}) ->
    case proplists:get_value(<<"status">>, Resp) of
        <<"0">> ->
            {ok, Resp};
        Code ->
            ErrorDesc = proplists:get_value(<<"error_text">>, Resp),
            {error, {server_error, Code, ErrorDesc}}
    end;
parse_response(check_verification, {ok, Resp}) ->
    case proplists:get_value(<<"status">>, Resp) of
        <<"0">> ->
            {ok, Resp};
        Code ->
            ErrorDesc = proplists:get_value(<<"error_text">>, Resp),
            {error, {server_error, Code, ErrorDesc}}
    end;
parse_response(_, Resp) ->
    Resp.
