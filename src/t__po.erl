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
-module(t__po).
-author("Madalin Grigore-Enescu").

-include("../include/t__.hrl").

-export([
	file_read/1, file_write/5
]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% file_
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%-------------------------------------------------------------
%% file_read
%%-------------------------------------------------------------

-spec file_read(Filename) -> {ok, Header, Msgs} | {error, Reason} when
	Filename :: file:name_all(),
	Header :: proplists:proplist(),
	Msgs :: proplists:proplist(),
	Reason :: file:posix() | badarg | terminated | system_limit | po_parse_error.
%% @doc Read and parse a PO file
file_read(Filename) ->
	case file:read_file(Filename) of
		{ok, Bin} ->
			case parse(Bin) of
				Ok = {ok, _Header, _Msgs} -> Ok;
				Error1 ->
					?T__LOG(error, "t__po:parse(Filename) error!",
						[
							{filename, Filename},
							{error, Error1}
						]),
					{ok, po_parse_error}
			end;
		Error2 ->
			?T__LOG(error, "file:read_file(Filename) error!",
				[
					{filename, Filename},
					{error, Error2}
				]),
			Error2
	end.

%%-------------------------------------------------------------
%% file_write
%%-------------------------------------------------------------

-spec file_write(Filename, Pot, Language, Header, Msgs) -> ok | {error, Reason} when
	Filename :: file:name_all(),
	Pot :: boolean(),
	Language :: string(),
	Header :: proplists:proplist(),
	Msgs :: proplists:proplist(),
	Reason :: file:posix() | badarg | system_limit | unknown_language.
%% @doc Write PO file
file_write(Filename, Pot, Language, Header, Msgs) ->
	Language = proplists:get_value("Language", Header, Language),
	{LanguageName, LanguageFormula, LanguageNplurals} = file_write_language_specs(Language),
	FileContent = file_write_build(Pot, LanguageName, LanguageFormula, LanguageNplurals, Header, Msgs),
	file:write_file(Filename, FileContent).

%% @doc Returns header with proper language information or error if language is invalid
file_write_language_specs(Language) when erlang:is_list(Language) ->
	case t__languages:get_specs(Language) of
		undefined -> {error, unknown_language};
		Specs ->
			Name = proplists:get_value("name", Specs),
			Formula = proplists:get_value("formula", Specs),
			Nplurals = proplists:get_value("nplurals", Specs),
			{Name, Formula, Nplurals}
	end.

%% @doc Build PO file content
file_write_build(Pot, LanguageName, LanguageFormula, Nplurals, Header, Msgs) ->
	Date = header_date(),
	Header1 = [
		{"Project-Id-Version", proplists:get_value("Project-Id-Version", Header, "t__ 0.1.0")},
		{"Report-Msgid-Bugs-To", proplists:get_value("Report-Msgid-Bugs-To", Header, "https://github.com/ergenius/t__")},
		{"POT-Creation-Date", proplists:get_value("POT-Creation-Date", Header, Date)},
		{"PO-Revision-Date", proplists:get_value("PO-Revision-Date", Header, Date)},
		{"Last-Translator", proplists:get_value("Last-Translator", Header, "unknown")},
		{"Content-Type", proplists:get_value("Content-Type", Header, "text/plain; charset=UTF-8")},
		{"MIME-Version", proplists:get_value("MIME-Version", Header, "1.0")},
		{"Content-Transfer-Encoding", proplists:get_value("Content-Transfer-Encoding", Header, "8bit")},
		{"Language", proplists:get_value("Language", Header)},
		{"Plural-Forms", lists:append(["nplurals=", erlang:integer_to_list(Nplurals), "; plural=(", LanguageFormula, ");"])},
		{"Language-Team", LanguageName}
	],
	file_write_build_msgs(Msgs, Pot, Nplurals, file_write_build_header(Pot, Header1)).

%% @doc Build PO file header
file_write_build_header(Pot, Header) -> file_write_build_header(Pot, Header, ["msgid \"\"\nmsgstr \"\"\n"]).
file_write_build_header(true, [{"PO-Revision-Date", _Value}|T], Acum) ->
	file_write_build_header(true, T, Acum);
file_write_build_header(Pot, [{Key, Value}|T], Acum) when erlang:is_list(Key), erlang:is_list(Value) ->
	file_write_build_header(Pot, T, [lists:append([Key, ": ", Value, "\\n\n"]) | Acum]);
file_write_build_header(_Pot, [], Acum) -> Acum.

%% @doc Build PO file messages
file_write_build_msgs([H|T], Pot, Nplurals, Acum) ->
	MsgComments = file_write_build_msgcomments(H),
	Msgctxt = file_write_build_msgctxt(H),
	Msgidstr = file_write_build_msgid(Pot, Nplurals, H),
	file_write_build_msgs(T, Pot, Nplurals, [lists:flatten(["\n", MsgComments, Msgctxt, Msgidstr]) | Acum]);
file_write_build_msgs([], _Pot, _Nplurals, Acum) -> lists:reverse(Acum).

%% @doc Build optional messages comments
file_write_build_msgcomments({{_Msgctxt, _Msgid}, {undefined,_Msgstr}}) -> [];
file_write_build_msgcomments({{_Msgctxt, _Msgid}, {MsgComments,_Msgstr}}) ->
	file_write_build_msgcomments(MsgComments, []).
file_write_build_msgcomments([H|T], Acum) ->
	file_write_build_msgcomments(T, [lists:append(["\n#", H]) | Acum]);
file_write_build_msgcomments([], Acum) -> lists:reverse(Acum).

%% @doc Build optional messages context
file_write_build_msgctxt({{undefined, _Msgid}, {_MsgComments,_Msgstr}}) -> [];
file_write_build_msgctxt({{Msgctxt, _Msgid}, {_MsgComments,_Msgstr}}) -> ["\nmsgctxt ", file_write_build_string(Msgctxt)].

%% @doc Build msgid/msgstr or msgid/msgid_plural/msgstr[x] for POT and PO
file_write_build_msgid(true, _Nplurals, {{_Msgctxt, [Msgid]}, {_MsgComments, _Msgstr}}) ->
	["\nmsgid ", file_write_build_string(Msgid), "\nmsgstr \"\""];
file_write_build_msgid(true, Nplurals, {{_Msgctxt, [Msgid, Msgidplural]}, {_MsgComments, _Msgstr}}) ->
	[
		"\nmsgid ", file_write_build_string(Msgid),
		"\nmsgid_plural ", file_write_build_string(Msgidplural),
		file_write_build_msgstr_p_pot(Nplurals)
	];
file_write_build_msgid(false, _Nplurals, {{_Msgctxt, [Msgid]}, {_MsgComments, Msgstr}}) ->
	["\nmsgid ", file_write_build_string(Msgid), "\nmsgstr ", file_write_build_string(Msgstr)];
file_write_build_msgid(false, Nplurals, {{_Msgctxt, [Msgid, Msgidplural]}, {_MsgComments, Msgstr}}) ->
	[
		"\nmsgid ", file_write_build_string(Msgid),
		"\nmsgid_plural ", file_write_build_string(Msgidplural),
		file_write_build_msgstr_p(Nplurals, Msgstr)
	].

%% @doc Build msgstr[x] for POT
file_write_build_msgstr_p_pot(Nplurals) -> file_write_build_msgstr_p_pot(0, Nplurals, []).
file_write_build_msgstr_p_pot(N, Nplurals, Acum) when N < Nplurals -> lists:flatten(Acum);
file_write_build_msgstr_p_pot(N, Nplurals, Acum) ->
	file_write_build_msgstr_p_pot(N+1, Nplurals, [Acum, "\nmsgstr[", erlang:integer_to_list(N), "] \"\""]).

%% @doc Build msgstr[x] for PO
file_write_build_msgstr_p(Nplurals, Msgstr) -> file_write_build_msgstr_p(0, Nplurals, Msgstr, []).
file_write_build_msgstr_p(N, Nplurals, _Msgstr, Acum) when N >= Nplurals -> lists:flatten(Acum);
file_write_build_msgstr_p(N, Nplurals, [H|T], Acum) ->
	file_write_build_msgstr_p(N+1, Nplurals, T,
		[Acum, "\nmsgstr[", erlang:integer_to_list(N), "] ", file_write_build_string(H)]);
% Allow untranslated/missing msgstr[x]
file_write_build_msgstr_p(N, Nplurals, [], Acum) ->
	file_write_build_msgstr_p(N+1, Nplurals, [],
		[Acum, "\nmsgstr[", erlang:integer_to_list(N), "] \"\""]).

%% @doc Build C style string (including quotes)
file_write_build_string(S) -> [$" | file_write_build_string(S, "\"")].
file_write_build_string([$\n|T], Acum) -> file_write_build_string(T, [Acum, "\\n"]);
file_write_build_string([$"|T], Acum) -> file_write_build_string(T, [Acum, "\\\""]);
file_write_build_string([$\\|T], Acum) -> file_write_build_string(T, [Acum, "\\\\"]);
file_write_build_string([H|T], Acum) -> file_write_build_string(T, [Acum, H]);
file_write_build_string([], Acum) -> lists:flatten(Acum).

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
				[{{undefined, [[]]}, {_Comments, [Header]}} | T] ->
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
	case parse_comments(T, Line) of
		% Ignore comments at the end of the file
		% (they are reported as invalid/errors by many PO editors parsers
		% but we should be as relaxed as possible)
		{[], _, _} -> {ok, lists:reverse(Acum)};
		{T1, Line1, Comments} ->
			case parse_msgid(undefined, T1, Line1, Comments) of
				{T2, Line2, Msg} -> parse_next(T2, Line2, [Msg | Acum]);
				Error -> Error
			end
	end.

%%-------------------------------------------------------------
%% parse_msgid
%%-------------------------------------------------------------

%% @doc Parse msgid
%% optional msgctxt
parse_msgid(undefined, [$m, $s, $g, $c, $t, $x, $t | T], Line, Comments) ->
	case parse_string(T, Line) of
		{T1, Line1, Context} -> parse_msgid(Context, T1, Line1, Comments);
		ParseStringError -> ParseStringError
	end;
%% msgid
parse_msgid(Context, [$m, $s, $g, $i, $d | T], Line, Comments) ->
	case parse_string(T, Line) of
		{T1, Line1, MsgId} ->
			%% Maybe msgstr next?
			case parse_msgstr(T1, Line1) of
				{T2, Line2, MsgStr} ->
					{T2, Line2, {{Context, [MsgId]}, {Comments, [MsgStr]}}};
				_ ->
					%% NO msgstr, plural MUST follow next otherwise the PO is malformed
					case parse_msgid_plural(T1, Line1) of
						{T3, Line3, MsgidPlural, Plural} ->
							{T3, Line3, {{Context, [MsgId, MsgidPlural]}, {Comments, Plural}}};
						_ -> {error, {msgstr_or_msgid_plural_expected, Line1}}
					end
			end;
		ParseStringError -> ParseStringError
	end;
%% unexpected line
parse_msgid(_, [_ | _], Line, _Comments) -> {error, {unexpected_line, Line}}.

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

%%-------------------------------------------------------------
%% parse_comments
%%-------------------------------------------------------------

%% @doc Parse multiple comments and return them
parse_comments(I, Line) -> parse_comments(I, Line, []).

% Ignore 'space' characters and empty lines
parse_comments([$\n | T], Line, Acum) -> parse_comments(T, Line + 1, Acum);
parse_comments([$\r | T], Line, Acum) -> parse_comments(T, Line, Acum);
parse_comments([$\s | T], Line, Acum) -> parse_comments(T, Line, Acum);
parse_comments([$\t | T], Line, Acum) -> parse_comments(T, Line, Acum);
%% Parse comment
parse_comments([$# | T], Line, Acum) ->
	{NewT, Comment} = parse_comment(T),
	parse_comments(NewT, Line + 1, [Comment | Acum]);
parse_comments(T, Line, Acum) -> {T, Line, lists:reverse(Acum)}.

%% @doc Parse comment
parse_comment(I) -> parse_comment(I, []).
parse_comment([$\n | T], Acum) -> {T, lists:reverse(Acum)};
parse_comment([H | T], Acum) -> parse_comment(T, [H|Acum]);
parse_comment([], Acum) -> {[], lists:reverse(Acum)}.

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

%% @doc Generate PO Date
header_date() ->
	TS = os:timestamp(),
	{{Year,Month,Day},{Hour,Minute,_Second}} = calendar:now_to_universal_time(TS),
	lists:flatten(io_lib:format("\"PO-Revision-Date: ~4..0w-~2..0w-~2..0w ~2..0w:~2..0w+0000\\n\"\n",[Year, Month, Day, Hour, Minute])).


