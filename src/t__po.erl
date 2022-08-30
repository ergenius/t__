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
-module(t__po).
-author("Madalin Grigore-Enescu").

-include("../include/t__.hrl").

-export([
	file_read/1, file_write/2
]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% file_
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

%% @doc Read and parse a PO file
file_read(Filename) ->
	case file:read_file(Filename) of
		{ok, Bin} -> parse(Bin);
		Error ->
			?T__LOG(error, "file:read_file(Filename) error!",
				[
					{filename, Filename},
					{error, Error}
				]),
			Error
	end.

%% @doc Write PO file
file_write(_Translation, _Filename) -> todo.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% parse_
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%-------------------------------------------------------------
%% parse
%%-------------------------------------------------------------

%% @doc Parse PO file
parse(Input) when erlang:is_list(Input) -> parse(Input);
parse(Input) when erlang:is_binary(Input) ->
	%% Ensure we know what a binary contains before converting it to a string.
	UnicodeList = case unicode:characters_to_list(Input) of
					  L when is_list(L) -> L; %% Unicode
					  _ -> erlang:binary_to_list(Input) %% The file was bytewise encoded
				  end,
	case parse_next(UnicodeList, 1, []) of
		{ok, Parsed} ->
			case Parsed of
				[{{undefined, [[]]}, [Header]} | T] ->
					%% Proper header found, parse it
					case parse_header(Header) of
						{ok, Header1} ->
							{ok, Header1, T};
						ParseHeaderError -> ParseHeaderError
					end;
				[{{undefined, [[]]}, _} | _] ->
					{error, po_invalid_header};
				_ ->
					%% PO is missing proper header
					{error, po_missing_header}
			end;
		ParseNextError -> ParseNextError
	end.

%%-------------------------------------------------------------
%% parse_header
%%-------------------------------------------------------------

parse_header(Header) -> parse_scan_line(Header, [], []).
parse_scan_line([$:|T], KeyAcum, Acum) ->
	KeyAcum1 = skip_spaces_tabs(lists:reverse(skip_spaces_tabs(KeyAcum))),
	case parse_scan_value(T) of
		{ok, T2, Value} -> parse_scan_line(T2, [], [{KeyAcum1, Value}|Acum]);
		Error -> Error
	end;
parse_scan_line([H|T], KeyAcum, Acum) ->
	parse_scan_line(T, [H|KeyAcum], Acum);
parse_scan_line([], _KeyAcum, Acum) ->
	{ok, lists:reverse(Acum)}.

parse_scan_value(I) ->
	parse_scan_value(I, []).
parse_scan_value([$\n|T], Acum) ->
	{ok, T, skip_spaces_tabs(lists:reverse(skip_spaces_tabs(Acum)))};
parse_scan_value([H|T], Acum) ->
	parse_scan_value(T, [H|Acum]);
parse_scan_value([], Acum) -> {error, po_invalid_header_missing_new_line, lists:reverse(Acum)}.

%%-------------------------------------------------------------
%% parse_next
%%-------------------------------------------------------------

%% @doc Parse next
parse_next(T, Line, Acum) ->
	case skip(T, Line) of
		{[], _} -> {ok, lists:reverse(Acum)};
		{T1, Line1} ->
			case parse_msgid(undefined, T1, Line1) of
				{T2, Line2, Msg} -> parse_next(T2, Line2, [Msg | Acum]);
				Error -> Error
			end
	end.

%%-------------------------------------------------------------
%% parse_msgid
%%-------------------------------------------------------------

%% @doc Parse msgid 
%% optional msgctxt
parse_msgid(undefined, [$m, $s, $g, $c, $t, $x, $t | T], Line) ->
	case parse_string(T, Line) of
		{T1, Line1, Context} -> parse_msgid(Context, T1, Line1);
		ParseStringError -> ParseStringError
	end;
%% msgid
parse_msgid(Context, [$m, $s, $g, $i, $d | T], Line) ->
	case parse_string(T, Line) of
		{T1, Line1, MsgId} ->
			%% Maybe msgstr next?
			case parse_msgstr(T1, Line1) of
				{T2, Line2, MsgStr} ->
					{T2, Line2, {{Context, [MsgId]}, [MsgStr]}};
				_ ->
					%% NO msgstr, plural MUST follow next otherwise the PO is malformed
					case parse_msgid_plural(T1, Line1) of
						{T3, Line3, MsgidPlural, Plural} -> {T3, Line3, {{Context, [MsgId, MsgidPlural]}, Plural}};
						_ -> {error, {msgstr_or_msgid_plural_expected, Line1}}
					end
			end;
		ParseStringError -> ParseStringError
	end;
%% unexpected line
parse_msgid(_, [_ | _], Line) -> {error, {unexpected_line, Line}}.

%%-------------------------------------------------------------
%% parse_msgstr
%%-------------------------------------------------------------

%% @doc Parse msgstr
parse_msgstr(T, Line) ->
	{T1, Line1} = skip(T, Line),
	case T1 of
		[$m, $s, $g, $s, $t, $r | T2] -> parse_string(T2, Line1);
		_ -> {error, {msgstr_expected, Line1}}
	end.

%%-------------------------------------------------------------
%% parse_msgid_plural
%%-------------------------------------------------------------

%% @doc Parse msgid plural
parse_msgid_plural(T, Line) ->
	{T1, Line1} = skip(T, Line),
	case T1 of
		%% msgid_plural
		[$m, $s, $g, $i, $d, $_, $p, $l, $u, $r, $a, $l | T2] ->
			case parse_string(T2, Line1) of
				{T3, Line3, MsgidPlural} ->
					%% Parse optional msgstr[n]
					case parse_msgstr_n(T3, Line3, []) of
						{T4, Line4, Plural} -> {T4, Line4, MsgidPlural, Plural};
						Error -> Error
					end;
				_ ->
					%% NO msgid_plural string! This is clearly malformed PO
					{error, {msgid_plural_string_expected, Line1}}
			end;
		_ -> {error, {msgid_plural_expected, Line1}}
	end.

%% @doc Parse plural msgstr[n]
%% n is always increasing from 0 to plural so there is no need to validate the index.
%% I tested scrambling array indexes with a few PO editors and they all reported errors
%% reading PO file when n is scrambled.
%% TODO: research more into this subject and add validation if necessary
parse_msgstr_n([$m, $s, $g, $s, $t, $r, $[, _I, $] | T], Line, Acum) ->
	case parse_string(T, Line) of
		{T1, Line1, Msgstr} ->
			{T2, Line2} = skip(T1, Line1),
			parse_msgstr_n(T2, Line2, [Msgstr | Acum]);
		ParseStringError -> ParseStringError
	end;
parse_msgstr_n(T, Line, Acum) -> {T, Line, lists:reverse(Acum)}.

%%-------------------------------------------------------------
%% parse_string
%%-------------------------------------------------------------

%% @doc Parse string (multiline must also be supported)
%% Example ("A multi-line string literal with comments ignored, \\, \", and \n"):
%%    ""  
%%      "A multi-line"      
%% ""
%%  " string literal "
%% "with comments "   
%% # this is a comment
%%
%%"ignored, \\, \", and \n"
parse_string(T, Line) ->
	{T1, Line1} = skip(T, Line),
	case T1 of
		[$" | T2] -> parse_string_in(T2, Line1, []);
		_ -> {error, {string_expected, Line1}}
	end.

parse_string_in([$\\, $\\ | T], Line, Acum) -> parse_string_in(T, Line, [$\\ | Acum]);
parse_string_in([$\\, $" | T], Line, Acum) -> parse_string_in(T, Line, [$" | Acum]);
parse_string_in([$\\, $n | T], Line, Acum) -> parse_string_in(T, Line, [$\n | Acum]);
parse_string_in([$" | T], Line, Acum) ->
	case skip(T, Line) of
		{[$" | T1], Line1} -> parse_string_in(T1, Line1, Acum);
		{T2, Line2} -> {T2, Line2, lists:reverse(Acum)}
	end;
parse_string_in([H | T], Line, Acum) -> parse_string_in(T, Line, [H | Acum]);
parse_string_in([], Line, _Acum) -> {error, {string_not_closed, Line}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% skip_
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Skip comments/spaces/tabs/carriage returns/new lines
skip([$\r | T], Line) -> skip(T, Line);
skip([$\n | T], Line) -> skip(T, Line + 1);
skip([$\s | T], Line) -> skip(T, Line);
skip([$\t | T], Line) -> skip(T, Line);
%% comment
skip([$# | T], Line) -> skip(skip_comment(T), Line + 1);
skip(T, Line) -> {T, Line}.

%% @doc Skip comments
skip_comment([$\n | T]) -> T;
skip_comment([_ | T]) -> skip_comment(T);
skip_comment([]) -> [].

% %% @doc Skip spaces/tabs
skip_spaces_tabs([$\s|T]) -> skip_spaces_tabs(T);
skip_spaces_tabs([$\t|T]) -> skip_spaces_tabs(T);
skip_spaces_tabs(T) -> T.

% %% @doc Skip spaces/tabs/carriage returns/new lines
% skip_spaces_tabs_cr_nl([$\r|T], Line) -> skip_spaces_tabs_cr_nl(T, Line);
% skip_spaces_tabs_cr_nl([$\n|T], Line) -> skip_spaces_tabs_cr_nl(T, Line+1);
% skip_spaces_tabs_cr_nl([$\s|T], Line) -> skip_spaces_tabs_cr_nl(T, Line);
% skip_spaces_tabs_cr_nl([$\t|T], Line) -> skip_spaces_tabs_cr_nl(T, Line);
% skip_spaces_tabs_cr_nl(T, Line) -> {T, Line}.





