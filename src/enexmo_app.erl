-module(enexmo_app).
-behaviour(application).

%% application callbacks
-export([start/2]).
-export([stop/1]).

%% application callbacks
start(_Type, _Args) ->
    ok = enexmo_channel:init(),
	enexmo_sup:start_link().

stop(_State) ->
	ok.
