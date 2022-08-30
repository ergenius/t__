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
%%
%% =========================================================================
%% THIS MODULE IS USED TO GENERATE AUTOMATICALLY PARTIAL TRANSPILED
%% t__plural.erl MODULE. IF YOU CALL t__plural_gen:generate/0
%% t__plural.erl MODULE WILL BE REPLACED WITH A NEW GENERATED UNFINISHED
%% FILE!
%% =========================================================================
-module(t__plural_gen).
-author("Madalin Grigore-Enescu").

%% API
-export([generate/0]).

%% Data is coming from this project: https://github.com/ergenius/gettext-po-samples
%% (Project maintained by the same author)
-define(T__PLURAL_FORMS, [
	[{"nplurals", 1},
		{"formula", "0"},
		{"cases", ["other"]},
		{"languages",
			["bm", "bo", "dz", "hnj", "id", "ig", "ii", "ja", "jbo", "jv", "jw", "kde", "kea", "km",
				"ko", "lkt", "lo", "ms", "my", "nqo", "osa", "sah", "ses", "sg", "su", "th", "to",
				"tpi", "vi", "wo", "yo", "yue", "zh", "zh_Hans", "zh_Hant"]}],
	[{"nplurals", 2},
		{"formula", "n != 1"},
		{"cases", ["one", "other"]},
		{"languages",
			["af", "an", "asa", "ast", "az", "bal", "bem", "bez", "bg", "brx", "ca", "ce", "cgg",
				"chr", "ckb", "da", "de", "de_AT", "de_CH", "dv", "ee", "el", "en", "en_AU", "en_CA",
				"en_GB", "en_US", "eo", "et", "eu", "fi", "fo", "fur", "fy", "gl", "gsw", "ha", "haw",
				"hu", "ia", "io", "jgo", "jmc", "ka", "kaj", "kcg", "kk", "kkj", "kl", "ks", "ksb",
				"ku", "ky", "lb", "lg", "lij", "mas", "mgo", "ml", "mn", "mr", "nah", "nb", "nd", "ne",
				"nl", "nl_BE", "nn", "nnh", "no", "nr", "ny", "nyn", "om", "or", "os", "pap", "ps",
				"rm", "rof", "rwk", "saq", "sc", "scn", "sd", "sdh", "seh", "sn", "so", "sq", "ss",
				"ssy", "st", "sv", "sw", "sw_CD", "syr", "ta", "te", "teo", "tig", "tk", "tn", "tr",
				"ts", "ug", "ur", "uz", "ve", "vo", "vun", "wae", "xh", "xog", "yi"]}],
	[{"nplurals", 2},
		{"formula",
			"n != 1 && n != 2 && n != 3 && (n % 10 == 4 || n % 10 == 6 || n % 10 == 9)"},
		{"cases", ["one", "other"]},
		{"languages", ["ceb", "fil", "tl"]}],
	[{"nplurals", 2},
		{"formula", "n % 10 != 1 || n % 100 == 11"},
		{"cases", ["one", "other"]},
		{"languages", ["is", "mk"]}],
	[{"nplurals", 2},
		{"formula", "n > 1"},
		{"cases", ["one", "other"]},
		{"languages",
			["ak", "am", "as", "bho", "bn", "doi", "fa", "fa_AF", "ff", "gu", "guw", "hi", "hy",
				"kab", "kn", "ln", "mg", "nso", "pa", "pcm", "si", "ti", "wa", "zu"]}],
	[{"nplurals", 2},
		{"formula", "n >= 2 && (n < 11 || n > 99)"},
		{"cases", ["one", "other"]},
		{"languages", ["tzm"]}],
	[{"nplurals", 3},
		{"formula",
			"(n % 10 == 1 && (n % 100 < 11 || n % 100 > 19)) ? 0 : ((n % 10 >= 2 && n % 10 <= 9 && (n % 100 < 11 || n % 100 > 19)) ? 1 : 2)"},
		{"cases", ["one", "few", "other"]},
		{"languages", ["lt"]}],
	[{"nplurals", 3},
		{"formula",
			"(n % 10 == 1 && n % 100 != 11) ? 0 : ((n % 10 >= 2 && n % 10 <= 4 && (n % 100 < 12 || n % 100 > 14)) ? 1 : 2)"},
		{"cases", ["one", "few", "other"]},
		{"languages", ["be", "bs", "hr", "ru", "sh", "sr", "sr_ME", "uk"]}],
	[{"nplurals", 3},
		{"formula", "(n == 0 || n == 1) ? 0 : ((n >= 2 && n <= 10) ? 1 : 2)"},
		{"cases", ["one", "few", "other"]},
		{"languages", ["shi"]}],
	[{"nplurals", 3},
		{"formula",
			"(n == 1) ? 0 : ((n % 10 >= 2 && n % 10 <= 4 && (n % 100 < 12 || n % 100 > 14)) ? 1 : 2)"},
		{"cases", ["one", "few", "other"]},
		{"languages", ["pl"]}],
	[{"nplurals", 3},
		{"formula",
			"(n == 1) ? 0 : ((n == 0 || n % 100 >= 2 && n % 100 <= 19) ? 1 : 2)"},
		{"cases", ["one", "few", "other"]},
		{"languages", ["mo", "ro", "ro_MD"]}],
	[{"nplurals", 3},
		{"formula", "(n == 1) ? 0 : ((n >= 2 && n <= 4) ? 1 : 2)"},
		{"cases", ["one", "few", "other"]},
		{"languages", ["cs", "sk"]}],
	[{"nplurals", 3},
		{"formula",
			"(n == 0 || n == 1) ? 0 : ((n != 0 && n % 1000000 == 0) ? 1 : 2)"},
		{"cases", ["one", "many", "other"]},
		{"languages", ["fr", "fr_CA", "fr_CH", "pt", "pt_BR"]}],
	[{"nplurals", 3},
		{"formula", "(n == 1) ? 0 : ((n != 0 && n % 1000000 == 0) ? 1 : 2)"},
		{"cases", ["one", "many", "other"]},
		{"languages", ["es", "es_419", "es_ES", "es_MX", "it", "pt_PT"]}],
	[{"nplurals", 3},
		{"formula", "(n == 1) ? 0 : ((n == 2) ? 1 : 2)"},
		{"cases", ["one", "two", "other"]},
		{"languages", ["iu", "naq", "sat", "se", "sma", "smi", "smj", "smn", "sms"]}],
	[{"nplurals", 3},
		{"formula",
			"(n % 10 == 0 || n % 100 >= 11 && n % 100 <= 19) ? 0 : ((n % 10 == 1 && n % 100 != 11) ? 1 : 2)"},
		{"cases", ["zero", "one", "other"]},
		{"languages", ["lv", "prg"]}],
	[{"nplurals", 3},
		{"formula", "(n == 0) ? 0 : ((n == 1) ? 1 : 2)"},
		{"cases", ["zero", "one", "other"]},
		{"languages", ["ksh", "lag"]}],
	[{"nplurals", 4},
		{"formula",
			"(n == 1) ? 0 : ((n == 0 || n % 100 >= 2 && n % 100 <= 10) ? 1 : ((n % 100 >= 11 && n % 100 <= 19) ? 2 : 3))"},
		{"cases", ["one", "few", "many", "other"]},
		{"languages", ["mt"]}],
	[{"nplurals", 4},
		{"formula",
			"(n % 10 == 1) ? 0 : ((n % 10 == 2) ? 1 : ((n % 100 == 0 || n % 100 == 20 || n % 100 == 40 || n % 100 == 60 || n % 100 == 80) ? 2 : 3))"},
		{"cases", ["one", "two", "few", "other"]},
		{"languages", ["gv"]}],
	[{"nplurals", 4},
		{"formula",
			"(n % 100 == 1) ? 0 : ((n % 100 == 2) ? 1 : ((n % 100 == 3 || n % 100 == 4) ? 2 : 3))"},
		{"cases", ["one", "two", "few", "other"]},
		{"languages", ["dsb", "hsb", "sl"]}],
	[{"nplurals", 4},
		{"formula",
			"(n == 1 || n == 11) ? 0 : ((n == 2 || n == 12) ? 1 : ((n >= 3 && n <= 10 || n >= 13 && n <= 19) ? 2 : 3))"},
		{"cases", ["one", "two", "few", "other"]},
		{"languages", ["gd"]}],
	[{"nplurals", 4},
		{"formula",
			"(n == 1) ? 0 : ((n == 2) ? 1 : ((n > 10 && n % 10 == 0) ? 2 : 3))"},
		{"cases", ["one", "two", "many", "other"]},
		{"languages", ["he"]}],
	[{"nplurals", 5},
		{"formula",
			"(n % 10 == 1 && n % 100 != 11 && n % 100 != 71 && n % 100 != 91) ? 0 : ((n % 10 == 2 && n % 100 != 12 && n % 100 != 72 && n % 100 != 92) ? 1 : ((((n % 10 == 3 || n % 10 == 4) || n % 10 == 9) && (n % 100 < 10 || n % 100 > 19) && (n % 100 < 70 || n % 100 > 79) && (n % 100 < 90 || n % 100 > 99)) ? 2 : ((n != 0 && n % 1000000 == 0) ? 3 : 4)))"},
		{"cases", ["one", "two", "few", "many", "other"]},
		{"languages", ["br"]}],
	[{"nplurals", 5},
		{"formula",
			"(n == 1) ? 0 : ((n == 2) ? 1 : ((n >= 3 && n <= 6) ? 2 : ((n >= 7 && n <= 10) ? 3 : 4)))"},
		{"cases", ["one", "two", "few", "many", "other"]},
		{"languages", ["ga"]}],
	[{"nplurals", 6},
		{"formula",
			"(n == 0) ? 0 : ((n == 1) ? 1 : (((n % 100 == 2 || n % 100 == 22 || n % 100 == 42 || n % 100 == 62 || n % 100 == 82) || n % 1000 == 0 && (n % 100000 >= 1000 && n % 100000 <= 20000 || n % 100000 == 40000 || n % 100000 == 60000 || n % 100000 == 80000) || n != 0 && n % 1000000 == 100000) ? 2 : ((n % 100 == 3 || n % 100 == 23 || n % 100 == 43 || n % 100 == 63 || n % 100 == 83) ? 3 : ((n != 1 && (n % 100 == 1 || n % 100 == 21 || n % 100 == 41 || n % 100 == 61 || n % 100 == 81)) ? 4 : 5))))"},
		{"cases", ["zero", "one", "two", "few", "many", "other"]},
		{"languages", ["kw"]}],
	[{"nplurals", 6},
		{"formula",
			"(n == 0) ? 0 : ((n == 1) ? 1 : ((n == 2) ? 2 : ((n % 100 >= 3 && n % 100 <= 10) ? 3 : ((n % 100 >= 11 && n % 100 <= 99) ? 4 : 5))))"},
		{"cases", ["zero", "one", "two", "few", "many", "other"]},
		{"languages", ["ar", "ar_001", "ars"]}],
	[{"nplurals", 6},
		{"formula",
			"(n == 0) ? 0 : ((n == 1) ? 1 : ((n == 2) ? 2 : ((n == 3) ? 3 : ((n == 6) ? 4 : 5))))"},
		{"cases", ["zero", "one", "two", "few", "many", "other"]},
		{"languages", ["cy"]}]]).

%% @doc We use this list to replace C operators with Erlang operators in PO plural formulas
-define(T__PLURAL_REPLACE, [
	%% Arithmetic Expressions
	%%{"++", " + 1 "}, %% Increment operator increases the integer value by one
	%%{"--", " - 1 "}, %% Decrement operator decreases the integer value by one.
	%%{"+", " + "},
	%%{"-", " - "},
	%%{"*", " * "},
	%%{6, "/", " div "}, %% Divides numerator by de-numerator/Integer division
	{<<"%">>, <<" rem ">>}, %% Modulus Operator and remainder of after an integer division/Integer remainder
	%% Comparisons
	{<<"==">>, <<" == ">>}, %% equal to (allow float)
	{<<"!=">>, <<" /= ">>}, %% not equal to (allow float)
	{<<"<=">>, <<" =< ">>}, %% Less than or equal to
	{<<">=">>, <<" >= ">>}, %% Greater than or equal to
	%%{<<"<">>, <<" < ">>}, %% Less than
	%%{<<">">>, <<" > ">>}, %% Greater than
	%% Logical Operators/Boolean Expressions
	{<<"!">>, <<" qot ">>}, %% Unary logical NOT
	{<<"&&">>, <<" aqd ">>}, %% Logical AND
	{<<"||">>, <<" or ">>}, %% Logical OR
	{<<"\t">>, <<" ">>}, %% Replace tab with space
	{<<"n">>, <<"N">>}, %% We used q in qot & aqd to be able to easily replace n
	{<<"q">>, <<"n">>}, %% We replace q back to n
	{<<"  ">>, <<" ">>}
]).

-define(T__PLURAL_FILE_TEMPLATE, "%% -*- coding: utf-8 -*-"
"\n%% Copyright (c) 2022, Madalin Grigore-Enescu <github@ergenius.com> <www.ergenius.com>"
"\n%%"
"\n%% Permission to use, copy, modify, and/or distribute this software for any"
"\n%% purpose with or without fee is hereby granted, provided that the above"
"\n%% copyright notice and this permission notice appear in all copies."
"\n%%"
"\n%% THE SOFTWARE IS PROVIDED \"AS IS\" AND THE AUTHOR DISCLAIMS ALL WARRANTIES"
"\n%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF"
"\n%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR"
"\n%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES"
"\n%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN"
"\n%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF"
"\n%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE."
"\n-module(t__plural)."
"\n-author(\"Madalin Grigore-Enescu\")."
"\n"
"\n-include(\"../include/t__.hrl\")."
"\n"
"\n-export([select/3])."
"\n"
"\n%% @doc Select and return the proper translation based on N and plural-forms"
"\n%% If the proper translation can not be determined, the method returns first element of Msg parameter."
"\nselect(Language, N, Msg) ->"
"\n    Index = n_to_index(Language, N) + 1,"
"\n    case Index > erlang:length(Msg) of"
"\n		true ->"
"\n			?T__LOG(warning, \"Invalid plural term translation: language=~p, msg=~p, n=~p\", [Language, Msg, N]),"
"\n			[Msg1|_] = Msg,"
"\n			Msg1;"
"\n		false -> lists:nth(Index, Msg)"
"\n	end."
"{{AF}}"
"\n"
"\n-spec n_to_index(Language, N) -> Index | undefined when"
"\n    Language :: string(),"
"\n    N :: integer(),"
"\n    Index :: integer()."
"\n%% @doc Returns plural form index for N or 'undefined' if language is unknown to us"
"{{AL}}"
"\nn_to_index(_, N) -> 0."
).

%% @doc Generate t__plural.erl file from the database
generate() -> generate(?T__PLURAL_FORMS, 0, [], []).

generate([H | T], Index, AL, AF) ->
	ListIndex = erlang:integer_to_list(Index),
	GL = generate_l(H, ListIndex),
	GF = generate_f(H, ListIndex),
	generate(T, Index + 1, [GL | AL], [GF | AF]);
generate([], _Index, AL, AF) ->

	RAL = erlang:iolist_to_binary(lists:reverse(AL)),
	RAF = erlang:iolist_to_binary(lists:reverse(AF)),
	TPluralFile = binary:replace(erlang:list_to_binary(?T__PLURAL_FILE_TEMPLATE), <<"{{AL}}">>, RAL),
	TPluralFile1 = binary:replace(TPluralFile, <<"{{AF}}">>, RAF),
	file:write_file(filename:join([filename:dirname(code:which(?MODULE)), "t__plural.erl"]),
		erlang:iolist_to_binary([unicode:encoding_to_bom(utf8), TPluralFile1])).

%% @doc Generate language functions
generate_l(H, Index) ->
	Languages = proplists:get_value("languages", H),
	generate_l(Languages, Index, []).
generate_l([H | T], Index, Acum) ->
	generate_l(T, Index, [Acum, "\nn_to_index(\"", H, "\", N) -> formula_", Index, "(N);"]);
generate_l([], _Index, Acum) -> unicode:characters_to_binary(lists:flatten(Acum), utf8).

%% @doc Generate formula function (partial transpiled, must be manually checked!)
generate_f(H, Index) ->
	Nplurals = io_lib:format("~p", [proplists:get_value("nplurals", H)]),
	Formula = proplists:get_value("formula", H),
	Cases = io_lib:format("~p", [proplists:get_value("cases", H)]),
	["\n\n%% nplurals: ", Nplurals, "\n%% C formula: ", Formula, "\n%% cases: ", Cases, "\nformula_", Index, "(N) -> ", generate_f_if(Formula)].

generate_f_if("0") -> "0.";
generate_f_if(F) -> ["\nif\n", generate_f_if(F, 0, [], [], []), "\nend."].
%% End of guard
generate_f_if([$? | T], Index, Prev, FAcum, Acum) ->
	FAcum1 = lists:flatten(FAcum),
	FAcum2 = trim_formula(generate_f_if_guard(FAcum1)),
	generate_f_if(T, Index, ";\n", [], [Acum, Prev, "    (", FAcum2, ") -> "]);
%% End of value
generate_f_if([$: | T], Index, Prev, FAcum, Acum) ->
	FAcum1 = trim_formula(lists:flatten(FAcum)),
	generate_f_if(T, Index+1, Prev, [], [Acum, FAcum1]);
%% Any
generate_f_if([H | T], Index, Prev, FAcum, Acum) ->
	generate_f_if(T, Index, Prev, [FAcum, [H]], Acum);
%% End of formula
generate_f_if([], Index, Prev, [], Acum) ->
	lists:flatten([Acum, Prev, "    true -> ", integer_to_list(Index)]);
generate_f_if([], Index, Prev, FAcum, Acum) ->
	FAcum1 = trim_formula(lists:flatten(FAcum)),
	case parse_is_string_integer(FAcum1) of
		true -> lists:flatten([Acum, Prev, "    true -> ", FAcum1]);
		false ->
			if
				Index > 0 ->
					lists:flatten([Acum, Prev, "    (", generate_f_if_guard(FAcum1), ") -> ", integer_to_list(Index)]);
				true ->
					lists:flatten([Acum, Prev, "    (", generate_f_if_guard(FAcum1), ") -> 1;\n    true -> 0"])
			end
	end.

%% Replace C with Erlang
generate_f_if_guard(G) -> generate_f_if_guard(?T__PLURAL_REPLACE, erlang:list_to_binary(G)).
generate_f_if_guard([{S, R} | T], G) ->
	generate_f_if_guard(T, binary:replace(G, S, R, [global]));
generate_f_if_guard([], G) -> erlang:binary_to_list(G).

%% @doc trim spaces and '(' from the beginning and spaces and ')' from the end
trim_formula(S) -> lists:reverse(trim_right(lists:reverse(trim_left(S)))).

%% @doc trim spaces and '('
trim_left([$(|T]) -> trim_left(T);
trim_left([$\s|T]) -> trim_left(T);
trim_left([$\t|T]) -> trim_left(T);
trim_left(T) -> T.

%% @doc trim spaces and '('
trim_right([$)|T]) -> trim_right(T);
trim_right([$\s|T]) -> trim_right(T);
trim_right([$\t|T]) -> trim_right(T);
trim_right(T) -> T.

%% @doc Check if the specified term holds a string holding an integer number (and nothing else)
parse_is_string_integer(S) when not erlang:is_list(S) -> false;
parse_is_string_integer([]) -> false;
parse_is_string_integer(S) -> parse_is_string_integer_i(S).
parse_is_string_integer_i([H|T]) ->
	case ((H < $0) or (H > $9)) of
		true -> false;
		false -> parse_is_string_integer_i(T)
	end;
parse_is_string_integer_i([]) -> true.