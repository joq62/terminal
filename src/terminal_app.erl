%%%-------------------------------------------------------------------
%% @doc terminal public API
%% @end
%%%-------------------------------------------------------------------

-module(terminal_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    terminal_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
