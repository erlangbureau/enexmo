Nexmo Client Library for Erlang
===============================

Usage
-----

Begin by starting the nexmo app:

```erl
1> application:ensure_all_started(enexmo).
{ok,[crypto,asn1,public_key,ssl,inets,jsx,enexmo]}
```

Then register a chanel with your key and secret:

```erl
2> enexmo:create_channel(test, [{api_key, "api_key"}, {api_secret, "api_secret"}]).
ok
```

For newer endpoints that support JWT authentication such as the Voice API,
you can also specify the `application_id` and `private_key` arguments:

```erl
2> enexmo:create_channel(test, [{api_key, "api_key"}, {api_secret, "api_secret"}, {application_id, "application_id"}, {private_key, "private_key"}]).
ok
```


## SMS API

### Send a text message

```erl
3> enexmo:send_message(test, "Nexmo", "380920000911", "test msg").
{error,{server_error,<<"29">>,
               <<"Non White-listed Destination - rejected">>}}
```

Docs: [https://docs.nexmo.com/messaging/sms-api/api-reference#request](https://docs.nexmo.com/messaging/sms-api/api-reference#request?utm_source=DEV_REL&utm_medium=github&utm_campaign=erlang-client-library)

## Verify API

### Start a verification

```erl
4> enexmo:start_verification(test, "380920000911", "Nexmo").
{ok,[{<<"request_id">>,
      <<"37735891af52435292616476fdc7e4ae">>},
     {<<"status">>,<<"0">>}]}
```

Docs: [https://docs.nexmo.com/verify/api-reference/api-reference#vrequest](https://docs.nexmo.com/verify/api-reference/api-reference#vrequest?utm_source=DEV_REL&utm_medium=github&utm_campaign=erlang-client-library)

The response contains a verification request id which you will need to
store temporarily (in the session, database, url etc).

### Check a verification

```erl
5> enexmo:check_verification(test, "37735891af52435292616476fdc7e4ae", "3100").
{ok,[{<<"request_id">>,
      <<"37735891af52435292616476fdc7e4ae">>},
     {<<"status">>,<<"0">>},
     {<<"event_id">>,<<"070000000F65C3CB">>},
     {<<"price">>,<<"0.10000000">>},
     {<<"currency">>,<<"EUR">>}]}
```

Docs: [https://docs.nexmo.com/verify/api-reference/api-reference#check](https://docs.nexmo.com/verify/api-reference/api-reference#check?utm_source=DEV_REL&utm_medium=github&utm_campaign=erlang-client-library)

The verification request id comes from the call to the start_verification method.
The PIN code is entered into your application by the user.
