%% -*- coding: utf-8 -*-
%% Copyright (c) 2022, Madalin Grigore-Enescu <github@ergenius.com>
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
-module(t__repository).
-author("Madalin Grigore-Enescu").

-include_lib("kernel/include/file.hrl").
-include("../include/t__.hrl").

-type file_info() :: #file_info{}.

-export([
    update/2,
    update_check/1,
    update_force/2
]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% update
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec update(Application, Repository) -> {ok, NewRepository} | {error, Error} when
    Application :: atom(),
    Repository :: t__repository(),
    NewRepository :: t__repository(),
    Error :: term().

%% @doc Update the specified repository (only if file changes are detected).
update(Application, Repository = #t__repository{name = RepositoryName}) ->
    case update_check(Repository) of
        {true, NewFileList} ->
            ?T__LOG(debug, "Repository changed. Updating ETS tables.", [{application, Application}, {repository, Repository}]),
            update_force(Application, Repository, NewFileList);
        false -> {ok, Repository};
        Error ->
            %% Most probable this is because repository directory:
            %% - is invalid
            %% - was deleted/no longer exists
            %% Cleanup ETS tables
            EtsMsgs = t__utils:ets_table_msgs(Application, RepositoryName),
            EtsHeaders = t__utils:ets_table_headers(Application, RepositoryName),
            EtsPot  = t__utils:ets_table_pot(Application, RepositoryName),
            t__utils:ets_table_create_or_delete_all(EtsMsgs),
            t__utils:ets_table_create_or_delete_all(EtsHeaders),
            t__utils:ets_table_create_or_delete_all(EtsPot),
            Error
    end.

%%-------------------------------------------------------------
%% update_check
%%-------------------------------------------------------------

-spec update_check(Repository) -> false | {true, NewFilelist} | {error, Error} when
    Repository :: t__repository(),
    NewFilelist :: {string(), file_info()},
    Error :: term().

%% @doc Check repository files for updates
update_check(#t__repository{directory = Directory, files = Files}) ->
    case t__utils:dir_list_files(Directory, [regular], [".po"]) of
        {ok, NewFiles} ->
            case erlang:is_list(Files) of
                true -> update_check_files(Files, NewFiles);
                false -> {true, NewFiles}
            end;
        Error -> Error
    end.

update_check_files(L1, L2) when length(L1) =/= length(L2) -> 
    {true, lists:keysort(1,L2)};
update_check_files(L1, L2) ->
    %% L1 is already sorted
    SL2 = lists:keysort(1, L2),
    case update_check_compare(L1, SL2) of
        true -> {true, SL2};
        false -> false
    end.

update_check_compare(
    [{Filename1, #file_info{size = Size1, type = Type1, mtime = Mtime1}}|T1], 
    [{Filename2, #file_info{size = Size2, type = Type2, mtime = Mtime2}}|T2]) ->
        case {Size1, Filename1, Type1, Mtime1} of 
            {Size2, Filename2, Type2, Mtime2} -> update_check_compare(T1, T2);
            _ -> true
        end;
update_check_compare([], []) -> false.

%%-------------------------------------------------------------
%% update_force
%%-------------------------------------------------------------

-spec update_force(Application, Repository) -> {ok, NewRepository} | {error, Error} when
    Application :: atom(),
    Repository :: t__repository(),
    NewRepository :: t__repository(),
    Error :: term().
%% @doc Force update repository
update_force(Application, Repository) ->  update_force(Application, Repository, undefined).

-spec update_force(Application, Repository, NewFiles) -> {ok, NewRepository} | {error, Error} when
    Application :: atom(),
    Repository :: t__repository(),
    NewFiles :: undefined | list(),
    NewRepository :: t__repository(),
    Error :: term().
%% @doc Force update repository from the specified list of files.
%% If the file list is undefined, we get the file list from repository directory.
update_force(Application, Repository = #t__repository{
    directory = Directory
}, undefined) ->
    case t__utils:dir_list_files(Directory, [regular], [".po"]) of
        {ok, NewFiles} -> update_force(Application, Repository, NewFiles);
        Error -> Error
    end;
update_force(Application, Repository = #t__repository{
    name = RepositoryName
}, NewFiles) when erlang:is_list(NewFiles) ->

    EtsMsgs = t__utils:ets_table_msgs(Application, RepositoryName),
    EtsHeaders = t__utils:ets_table_headers(Application, RepositoryName),
    EtsPot  = t__utils:ets_table_pot(Application, RepositoryName),
    t__utils:ets_table_create_or_delete_all(EtsMsgs),
    t__utils:ets_table_create_or_delete_all(EtsHeaders),
    t__utils:ets_table_create_or_delete_all(EtsPot),
    update_po(NewFiles, EtsMsgs),

    {ok, Repository#t__repository{files = NewFiles}}.

%%-------------------------------------------------------------
%% update_po
%%-------------------------------------------------------------

%% @doc Iterate list of PO files and load them into the proper ETS tables
update_po([{Filename, _}|T], EtsMsgs) ->
    try
        case t__po:file_read(Filename) of 
            {ok, Header, Msgs} ->
                FilenameLanguage = filename:basename(Filename, ".po"),
                Language = proplists:get_value("Language", Header, FilenameLanguage),
                try
                    ok = update_po_messages(Msgs, Language, EtsMsgs)
                catch Exception:Reason ->
                    ?T__LOG(error, "Fail inserting messages into ETS table.",
                        [{exception, {Exception,Reason}}, {filename, Filename}, {ets, EtsMsgs}])
                end;
            Error -> 
                ?T__LOG(error, "Error parsing PO file.",
                    [{filename, Filename}, {error, Error}])
        end
    catch Exception1:Reason1 ->
        ?T__LOG(error, "Exception parsing file", [{exception, {Exception1, Reason1}}, {filename, Filename}])
    end,
    update_po(T, EtsMsgs);
update_po([], _EtsMsgs) -> ok.

%% @doc Insert messages for the language into the specified ETS table
update_po_messages([{Key, Value}|T], Language, EtsMsgs) ->
    true = ets:insert(EtsMsgs, {{Language, Key}, Value}),
    update_po_messages(T, Language, EtsMsgs);
update_po_messages([], _Language, _EtsMsg) -> ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% write_to_pot
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% -spec write_to_pot(Application, {RepositoryName, Directory}) -> ok | {error, Error} when
%     Application :: atom(),
%     RepositoryName :: string() | atom() | binary(),
%     Directory :: string(),
%     Error :: term().
% %% @doc Write the repository requested translations to repository POT file
% write_to_pot(Application, {RepositoryName, Directory}) -> write_to_pot(Application, RepositoryName, Directory).

% -spec write_to_pot(Application, RepositoryName, Directory) -> ok | {error, Error} when
%     Application :: atom(),
%     RepositoryName :: string() | atom() | binary(),
%     Directory :: string(),
%     Error :: term().
% %% @doc Write the repository requested translations to repository POT file
% write_to_pot(Application, RepositoryName, Directory) ->
%     EtsPot = t__utils:ets_table_repository_pot(Application, RepositoryName),


