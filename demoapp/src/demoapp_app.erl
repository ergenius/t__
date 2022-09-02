%% -*- coding: utf-8 -*-
%% Copyright (c) 2022, Madalin Grigore-Enescu <https://github.com/ergenius> <https://ergenius.com>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

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
                directory = filename:join([
                    filename:dirname(filename:dirname(code:which(?MODULE))),
                    "priv/repositories/default"])
            },
            %% This is an invalid repository demonstrating how resilient t__ is.
            %% This is a nice feature for development phase.
            %% The repository directory is invalid, t__ will try to load it, fail and
            %% log some error but it will continue working with other valid repositories.
            %% If anytime later, the repository directory become available, t__ will load it
            %% and the repository will be available to use.
            #t__repository{
                name = "invalid",
                directory = filename:join(["~/invalid-repository-that-does-not-exist"])
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
