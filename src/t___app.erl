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
-module(t___app).
-author("Madalin Grigore-Enescu").

-behaviour(application).

-include("../include/t__.hrl").

%% API.
-export([start/2]).
-export([stop/1]).

-spec start(StartType, StartArgs) -> {ok, Pid} | {ok, Pid, State} | {error, Reason} when
    StartType :: application:start_type(),
    StartArgs :: term(),
    Pid :: pid(),
    State :: term(),
    Reason :: term().
%% @doc This function is called whenever an application is started using start/1,2,
%% and is to start the processes of the application. If the application is structured
%% according to the OTP design principles as a supervision tree,
%% this means starting the top supervisor of the tree.
start(StartType, StartArgs) ->

    ?T__LOG(debug, "t__ start", [{type, StartType}, {args, StartArgs}]),

    %% Start application main supervisor
    case t__sup:start_link(StartArgs) of
        {ok, Pid} -> {ok, Pid};
        Error ->
            ?T__LOG(debug, "t__sup:start_link failed!", [{error, Error}]),
            Error
    end.

-spec stop(State) -> Ignored when
    State :: term(),
    Ignored :: term().
%% @doc This function is called whenever an application has stopped.
%% It is intended to be the opposite of Module:start/2 and is to do any necessary cleaning up.
%% The return value is ignored.
stop(_State) -> ok.