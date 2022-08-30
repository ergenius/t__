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
-module(t__utils).
-author("Madalin Grigore-Enescu").

-include_lib("kernel/include/file.hrl").
-include("../include/t__.hrl").

-export([
    ets_table_msgs/2, ets_table_headers/2, ets_table_pot/2,
    ets_table_ensure/1,
    ets_table_delete/1,
	ets_table_create_or_delete_all/1,

	ets_lookup/2, ets_insert/2,

    dir_list_files/3,

	name_to_string/1
]).

%% @doc Prefix for applications ETS cache tables
-define(T__ETS_PREFIX, "t__").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ets_
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Return the ETS table holding messages for the specified application repository
ets_table_msgs(Application, RepositoryName) -> 
    ApplicationL = name_to_string(Application),
    RepositoryName = name_to_string(RepositoryName),
    erlang:list_to_atom(?T__ETS_PREFIX++ApplicationL++"_"++RepositoryName++"_msgs").

%% @doc Return the ETS table holding PO headers for the specified application repository
ets_table_headers(Application, RepositoryName) ->
    ApplicationL = name_to_string(Application),
    RepositoryName = name_to_string(RepositoryName),
    erlang:list_to_atom(?T__ETS_PREFIX++ApplicationL++"_"++RepositoryName++"_headers").

%% @doc Return the ETS table holding plural formula for the specified application repository
ets_table_pot(Application, RepositoryName) -> 
    ApplicationL = name_to_string(Application),
    RepositoryName = name_to_string(RepositoryName),
    erlang:list_to_atom(?T__ETS_PREFIX++ApplicationL++"_"++RepositoryName++"_pot").

%% @doc Ensure the specified table exists
%% This function should not be called before any ETS table operation but only on errors and exceptions
%% to avoid ets:info call cost.
ets_table_ensure(Table) when erlang:is_atom(Table) ->
    case ets:info(Table) of 
        undefined -> 
            ets:new(Table, [set, named_table, public]),
            new;
        _ -> exist
    end.

%% @doc Delete the specified table without throwing an error if the table does not exist
ets_table_delete(Table) ->
    try ets:delete(Table)
    catch _Exception:_Reason -> false
    end.

%% @doc Create the table if does not exist or delete all object if it does
ets_table_create_or_delete_all(Table) ->
	case ets:info(Table) of
		undefined ->
			ets:new(Table, [set, named_table, public]),
			ok;
		_ ->
			ets:delete_all_objects(Table),
			ok
	end.

%% @doc Returns a list of all objects with key Key in table Table without throwing exception
ets_lookup(Table, Key) ->
	try ets:lookup(Table, Key)
	catch Exception:Reason ->
		?T__LOG(error, "ets:lookup Exception!",	[
			{exception, {Exception, Reason}},
			{table, Table},
			{key, Key}
		]),
		[]
	end.

%% @doc Inserts the object or all of the objects in list into table Table without throwing exception
ets_insert(Table, ObjectOrObjects) ->
	try ets:insert(Table, ObjectOrObjects)
	catch Exception:Reason ->
		?T__LOG(error, "ets:insert Exception!",	[
			{exception, {Exception, Reason}},
			{table, Table},
			{object_or_objects, ObjectOrObjects}
		]),
		[]
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% dir_
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc List files into the directory with the specified extension
dir_list_files(Dir, Types, Extensions) ->
	case file:list_dir(Dir) of
		{ok, Filenames} -> dir_list_files_r(Dir, Filenames, Types, Extensions);
		Error -> Error
	end.
dir_list_files_r(Dir, Filenames, Types, Extensions) -> dir_list_files_r(Dir, Filenames, Types, Extensions, []).
dir_list_files_r(Dir, [Filename|T], Types, Extensions, Acum) ->
	FullFilename = filename:join(Dir, Filename),
	case file:read_file_info(FullFilename, [{time, posix}]) of
		{ok, FileInfo} ->
			case dir_list_files_match(FullFilename, FileInfo, Types, Extensions) of
				true ->
					dir_list_files_r(Dir, T, Types, Extensions, [{FullFilename, FileInfo}, Acum]);
				false -> dir_list_files_r(Dir, T, Types, Extensions, Acum)
			end;
		Error -> Error
	end;
dir_list_files_r(_Dir, [], _Types, _Extensions, Acum) -> {ok, lists:flatten(Acum)}.

dir_list_files_match(Filename, #file_info{type = Type}, Types, Extensions) when
	erlang:is_list(Types), erlang:is_list(Extensions) ->
	case lists:member(Type, Types) of
		true -> lists:member(filename:extension(Filename), Extensions);
		_ -> false
	end;
dir_list_files_match(_Filename, #file_info{type = Type}, Types, _Extensions) when
	erlang:is_list(Types) -> lists:member(Type, Types);
dir_list_files_match(Filename, _FileInfo, _Types, Extensions) when
	erlang:is_list(Extensions) -> lists:member(filename:extension(Filename), Extensions).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% name_to_string
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec name_to_string(Name) -> string() when
    Name :: string() | atom() | binary().
%% @doc Convert a name (list, atom, binary) to string.
name_to_string(Name) when erlang:is_list(Name) -> Name;
name_to_string(Name) when erlang:is_atom(Name) -> erlang:atom_to_list(Name);
name_to_string(Name) when erlang:is_binary(Name) -> erlang:binary_to_list(Name).

