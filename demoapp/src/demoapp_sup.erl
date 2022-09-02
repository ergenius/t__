%%%-------------------------------------------------------------------
%% @doc testapp top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(demoapp_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() -> supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init(Args) ->

    %% We use legacy specs for now to remain compatible with older erlang versions we still want to support
    %% Add children specifications to the supervisor
    ChildSpecs = [{{local, demoapp_srv}, {demoapp_srv, start_link, [Args]}, permanent, 10000, worker, [demoapp_srv]}],

    %% Returns supervisor flags and child specifications
    {ok, {{one_for_one, 1, 5}, ChildSpecs}}.
