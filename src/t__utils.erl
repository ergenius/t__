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
-module(t__utils).
-author("Madalin Grigore-Enescu").

-include_lib("kernel/include/file.hrl").
-include("../include/t__.hrl").

-export([
    dir_list_files/3, dir_del_r/1,
	name_to_string/1
]).

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

-spec dir_del_r(File) -> ok | {error, Reason} when
	File :: file:name_all(),
	Reason :: file:posix() | badarg.

%% @doc Deletes file or directory File. If File is a directory, its contents is first recursively deleted.
%% Same as new file:del_dir_r/1 introduced in OTP 23.0 for compatibility with older Erlang
dir_del_r(File) ->
	case file:read_link_info(File) of
		{ok, #file_info{type = directory}} ->
			case file:list_dir_all(File) of
				{ok, Names} ->
					lists:foreach(fun(Name) ->
						dir_del_r(filename:join(File, Name))
								  end, Names);
				{error, _Reason} -> ok
			end,
			file:del_dir(File);
		{ok, _FileInfo} -> file:delete(File);
		{error, _Reason} = Error -> Error
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% name_to_string
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec name_to_string(Name) -> string() when
    Name :: string() | atom() | binary().
%% @doc Convert a name (list, atom, binary) to string.
name_to_string(Name) when erlang:is_list(Name) -> Name;
name_to_string(Name) when erlang:is_atom(Name) -> erlang:atom_to_list(Name);
name_to_string(Name) when erlang:is_binary(Name) -> erlang:binary_to_list(Name).

