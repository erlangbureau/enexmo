-module(enexmo_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% application callbacks
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	Procs = [],
	{ok, {{one_for_one, 1, 5}, Procs}}.
