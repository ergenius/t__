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
-module(t__sup).
-author("Madalin Grigore-Enescu").

-behaviour(supervisor).

%% supervisor exports
-export([start_link/1]).
-export([init/1]).
-export([upgrade/0]).

-spec start_link(Args :: proplists:proplist()) -> {'ok', pid()} | 'ignore' | {'error', term()}.
%% @doc Creates a supervisor process as part of a supervision tree.
start_link(Args) -> supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

-spec init(Args) -> Result when
    Args :: term(),
    Result :: {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}} | ignore.
%% @doc Returns supervisor flags and child specifications.
init(Args) ->

    %% We use legacy specs for now to remain compatible with older erlang versions we still want to support
    %% Add children specifications to the supervisor
    ChildSpecs = [{{local, t__srv}, {t__srv, start_link, [Args]}, permanent, 10000, worker, [t__srv]}],

    %% Returns supervisor flags and child specifications
    {ok, {{one_for_one, 1, 5}, ChildSpecs}}.

-spec upgrade() -> ok.
%% @doc Handle the upgrade process.
upgrade() -> ok.