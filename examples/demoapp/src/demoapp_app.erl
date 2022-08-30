%%%-------------------------------------------------------------------
%% @doc testapp public API
%% @end
%%%-------------------------------------------------------------------

-module(demoapp_app).

-behaviour(application).

-include_lib("t__/include/t__.hrl").

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->

    %% Ensure t__ is started
    ok = application:ensure_started(t__),

    %% Example of configuring the t__ on runtime
    ok = t__:config_set(demoapp, #t__config{

        %% We setup the default application language to en
        %% Please check demoapp_srv.erl to see how you can overwrite this
        %% for a specific application process
        language = "en",

        %% we enable development mode - so every modification we make to
        %% the repositories PO files will be automatically loaded by t__
        %% (you can test it modifying PO files while running this application)
        dev = true,

        %% We setup a default repository for this application
        repositories = [
            %% You MUST always setup at least one "default" repository!
            #t__repository{
                name = "default",
                directory = filename:join([filename:dirname(filename:dirname(code:which(?MODULE))), "repositories/default"])
            },
            %% This is an invalid repository demonstrating how resilient t__ is.
            %% This is a nice feature for development phase.
            %% The repository directory is invalid, t__ will try to load it, fail and
            %% log some error but it will continue working with other valid repositories.
            %% If anytime later, the repository directory become available, t__ will load it
            %% and the repository will be available to use.
            #t__repository{
                name = "invalid",
                directory = "/t__/invalid-repository-that-does-not-exist-48945700dbguasczx"
            }
        ]
    }),

    %% Start application main supervisor
    case demoapp_sup:start_link() of
        {ok, Pid} -> {ok, Pid};
        Error ->
            ?T__LOG(debug, "t__sup:start_link failed!", [{error, Error}]),
            Error
    end.

stop(_State) ->
    ok.

%% internal functions
