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
-module(t__languages).
-author("Madalin Grigore-Enescu").

-export([get_specs/1, is_gettext_language/1]).

%% Source: https://cldr.unicode.org/
-define(T__LANGUAGES_SPECS, [
	{"af",
		[{"name", "Afrikaans"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"ak",
		[{"name", "Akan"},
			{"formula", "n > 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"am",
		[{"name", "Amharic"},
			{"formula", "n > 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"an",
		[{"name", "Aragonese"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"ar",
		[{"name", "Arabic"},
			{"formula",
				"(n == 0) ? 0 : ((n == 1) ? 1 : ((n == 2) ? 2 : ((n % 100 >= 3 && n % 100 <= 10) ? 3 : ((n % 100 >= 11 && n % 100 <= 99) ? 4 : 5))))"},
			{"nplurals", 6},
			{"cases",
				["zero", "one", "two", "few", "many", "other"]}]},
	{"ar_001",
		[{"name", "Modern Standard Arabic"},
			{"baseLanguage", "Arabic"},
			{"formula",
				"(n == 0) ? 0 : ((n == 1) ? 1 : ((n == 2) ? 2 : ((n % 100 >= 3 && n % 100 <= 10) ? 3 : ((n % 100 >= 11 && n % 100 <= 99) ? 4 : 5))))"},
			{"nplurals", 6},
			{"cases",
				["zero", "one", "two", "few", "many", "other"]}]},
	{"ars",
		[{"name", "Najdi Arabic"},
			{"formula",
				"(n == 0) ? 0 : ((n == 1) ? 1 : ((n == 2) ? 2 : ((n % 100 >= 3 && n % 100 <= 10) ? 3 : ((n % 100 >= 11 && n % 100 <= 99) ? 4 : 5))))"},
			{"nplurals", 6},
			{"cases",
				["zero", "one", "two", "few", "many", "other"]}]},
	{"as",
		[{"name", "Assamese"},
			{"formula", "n > 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"asa",
		[{"name", "Asu"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"ast",
		[{"name", "Asturian"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"az",
		[{"name", "Azerbaijani"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"bal",
		[{"name", "Baluchi"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"be",
		[{"name", "Belarusian"},
			{"formula",
				"(n % 10 == 1 && n % 100 != 11) ? 0 : ((n % 10 >= 2 && n % 10 <= 4 && (n % 100 < 12 || n % 100 > 14)) ? 1 : 2)"},
			{"nplurals", 3},
			{"cases", ["one", "few", "other"]}]},
	{"bem",
		[{"name", "Bemba"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"bez",
		[{"name", "Bena"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"bg",
		[{"name", "Bulgarian"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"bho",
		[{"name", "Bhojpuri"},
			{"formula", "n > 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"bm",
		[{"name", "Bambara"},
			{"formula", "0"},
			{"nplurals", 1},
			{"cases", ["other"]}]},
	{"bn",
		[{"name", "Bangla"},
			{"formula", "n > 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"bo",
		[{"name", "Tibetan"},
			{"formula", "0"},
			{"nplurals", 1},
			{"cases", ["other"]}]},
	{"br",
		[{"name", "Breton"},
			{"formula",
				"(n % 10 == 1 && n % 100 != 11 && n % 100 != 71 && n % 100 != 91) ? 0 : ((n % 10 == 2 && n % 100 != 12 && n % 100 != 72 && n % 100 != 92) ? 1 : ((((n % 10 == 3 || n % 10 == 4) || n % 10 == 9) && (n % 100 < 10 || n % 100 > 19) && (n % 100 < 70 || n % 100 > 79) && (n % 100 < 90 || n % 100 > 99)) ? 2 : ((n != 0 && n % 1000000 == 0) ? 3 : 4)))"},
			{"nplurals", 5},
			{"cases", ["one", "two", "few", "many", "other"]}]},
	{"brx",
		[{"name", "Bodo"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"bs",
		[{"name", "Bosnian"},
			{"formula",
				"(n % 10 == 1 && n % 100 != 11) ? 0 : ((n % 10 >= 2 && n % 10 <= 4 && (n % 100 < 12 || n % 100 > 14)) ? 1 : 2)"},
			{"nplurals", 3},
			{"cases", ["one", "few", "other"]}]},
	{"ca",
		[{"name", "Catalan"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"ce",
		[{"name", "Chechen"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"ceb",
		[{"name", "Cebuano"},
			{"formula",
				"n != 1 && n != 2 && n != 3 && (n % 10 == 4 || n % 10 == 6 || n % 10 == 9)"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"cgg",
		[{"name", "Chiga"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"chr",
		[{"name", "Cherokee"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"ckb",
		[{"name", "Central Kurdish"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"cs",
		[{"name", "Czech"},
			{"formula", "(n == 1) ? 0 : ((n >= 2 && n <= 4) ? 1 : 2)"},
			{"nplurals", 3},
			{"cases", ["one", "few", "other"]}]},
	{"cy",
		[{"name", "Welsh"},
			{"formula",
				"(n == 0) ? 0 : ((n == 1) ? 1 : ((n == 2) ? 2 : ((n == 3) ? 3 : ((n == 6) ? 4 : 5))))"},
			{"nplurals", 6},
			{"cases",
				["zero", "one", "two", "few", "many", "other"]}]},
	{"da",
		[{"name", "Danish"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"de",
		[{"name", "German"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"de_AT",
		[{"name", "Austrian German"},
			{"territory", "Austria"},
			{"baseLanguage", "German"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"de_CH",
		[{"name", "Swiss High German"},
			{"territory", "Switzerland"},
			{"baseLanguage", "German"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"doi",
		[{"name", "Dogri"},
			{"formula", "n > 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"dsb",
		[{"name", "Lower Sorbian"},
			{"formula",
				"(n % 100 == 1) ? 0 : ((n % 100 == 2) ? 1 : ((n % 100 == 3 || n % 100 == 4) ? 2 : 3))"},
			{"nplurals", 4},
			{"cases", ["one", "two", "few", "other"]}]},
	{"dv",
		[{"name", "Divehi"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"dz",
		[{"name", "Dzongkha"},
			{"formula", "0"},
			{"nplurals", 1},
			{"cases", ["other"]}]},
	{"ee",
		[{"name", "Ewe"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"el",
		[{"name", "Greek"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"en",
		[{"name", "English"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"en_AU",
		[{"name", "Australian English"},
			{"territory", "Australia"},
			{"baseLanguage", "English"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"en_CA",
		[{"name", "Canadian English"},
			{"territory", "Canada"},
			{"baseLanguage", "English"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"en_GB",
		[{"name", "British English"},
			{"territory", "United Kingdom"},
			{"baseLanguage", "English"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"en_US",
		[{"name", "American English"},
			{"territory", "United States"},
			{"baseLanguage", "English"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"eo",
		[{"name", "Esperanto"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"es",
		[{"name", "Spanish"},
			{"formula", "(n == 1) ? 0 : ((n != 0 && n % 1000000 == 0) ? 1 : 2)"},
			{"nplurals", 3},
			{"cases", ["one", "many", "other"]}]},
	{"es_419",
		[{"name", "Latin American Spanish"},
			{"territory", "Latin America"},
			{"baseLanguage", "Spanish"},
			{"formula", "(n == 1) ? 0 : ((n != 0 && n % 1000000 == 0) ? 1 : 2)"},
			{"nplurals", 3},
			{"cases", ["one", "many", "other"]}]},
	{"es_ES",
		[{"name", "European Spanish"},
			{"territory", "Spain"},
			{"baseLanguage", "Spanish"},
			{"formula", "(n == 1) ? 0 : ((n != 0 && n % 1000000 == 0) ? 1 : 2)"},
			{"nplurals", 3},
			{"cases", ["one", "many", "other"]}]},
	{"es_MX",
		[{"name", "Mexican Spanish"},
			{"territory", "Mexico"},
			{"baseLanguage", "Spanish"},
			{"formula", "(n == 1) ? 0 : ((n != 0 && n % 1000000 == 0) ? 1 : 2)"},
			{"nplurals", 3},
			{"cases", ["one", "many", "other"]}]},
	{"et",
		[{"name", "Estonian"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"eu",
		[{"name", "Basque"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"fa",
		[{"name", "Persian"},
			{"formula", "n > 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"fa_AF",
		[{"name", "Dari"},
			{"territory", "Afghanistan"},
			{"baseLanguage", "Persian"},
			{"formula", "n > 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"ff",
		[{"name", "Fulah"},
			{"formula", "n > 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"fi",
		[{"name", "Finnish"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"fil",
		[{"name", "Filipino"},
			{"formula",
				"n != 1 && n != 2 && n != 3 && (n % 10 == 4 || n % 10 == 6 || n % 10 == 9)"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"fo",
		[{"name", "Faroese"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"fr",
		[{"name", "French"},
			{"formula",
				"(n == 0 || n == 1) ? 0 : ((n != 0 && n % 1000000 == 0) ? 1 : 2)"},
			{"nplurals", 3},
			{"cases", ["one", "many", "other"]}]},
	{"fr_CA",
		[{"name", "Canadian French"},
			{"territory", "Canada"},
			{"baseLanguage", "French"},
			{"formula",
				"(n == 0 || n == 1) ? 0 : ((n != 0 && n % 1000000 == 0) ? 1 : 2)"},
			{"nplurals", 3},
			{"cases", ["one", "many", "other"]}]},
	{"fr_CH",
		[{"name", "Swiss French"},
			{"territory", "Switzerland"},
			{"baseLanguage", "French"},
			{"formula",
				"(n == 0 || n == 1) ? 0 : ((n != 0 && n % 1000000 == 0) ? 1 : 2)"},
			{"nplurals", 3},
			{"cases", ["one", "many", "other"]}]},
	{"fur",
		[{"name", "Friulian"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"fy",
		[{"name", "Western Frisian"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"ga",
		[{"name", "Irish"},
			{"formula",
				"(n == 1) ? 0 : ((n == 2) ? 1 : ((n >= 3 && n <= 6) ? 2 : ((n >= 7 && n <= 10) ? 3 : 4)))"},
			{"nplurals", 5},
			{"cases", ["one", "two", "few", "many", "other"]}]},
	{"gd",
		[{"name", "Scottish Gaelic"},
			{"formula",
				"(n == 1 || n == 11) ? 0 : ((n == 2 || n == 12) ? 1 : ((n >= 3 && n <= 10 || n >= 13 && n <= 19) ? 2 : 3))"},
			{"nplurals", 4},
			{"cases", ["one", "two", "few", "other"]}]},
	{"gl",
		[{"name", "Galician"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"gsw",
		[{"name", "Swiss German"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"gu",
		[{"name", "Gujarati"},
			{"formula", "n > 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"guw",
		[{"name", "Gun"},
			{"formula", "n > 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"gv",
		[{"name", "Manx"},
			{"formula",
				"(n % 10 == 1) ? 0 : ((n % 10 == 2) ? 1 : ((n % 100 == 0 || n % 100 == 20 || n % 100 == 40 || n % 100 == 60 || n % 100 == 80) ? 2 : 3))"},
			{"nplurals", 4},
			{"cases", ["one", "two", "few", "other"]}]},
	{"ha",
		[{"name", "Hausa"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"haw",
		[{"name", "Hawaiian"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"he",
		[{"name", "Hebrew"},
			{"formula",
				"(n == 1) ? 0 : ((n == 2) ? 1 : ((n > 10 && n % 10 == 0) ? 2 : 3))"},
			{"nplurals", 4},
			{"cases", ["one", "two", "many", "other"]}]},
	{"hi",
		[{"name", "Hindi"},
			{"formula", "n > 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"hnj",
		[{"name", "Mong Njua"},
			{"formula", "0"},
			{"nplurals", 1},
			{"cases", ["other"]}]},
	{"hr",
		[{"name", "Croatian"},
			{"formula",
				"(n % 10 == 1 && n % 100 != 11) ? 0 : ((n % 10 >= 2 && n % 10 <= 4 && (n % 100 < 12 || n % 100 > 14)) ? 1 : 2)"},
			{"nplurals", 3},
			{"cases", ["one", "few", "other"]}]},
	{"hsb",
		[{"name", "Upper Sorbian"},
			{"formula",
				"(n % 100 == 1) ? 0 : ((n % 100 == 2) ? 1 : ((n % 100 == 3 || n % 100 == 4) ? 2 : 3))"},
			{"nplurals", 4},
			{"cases", ["one", "two", "few", "other"]}]},
	{"hu",
		[{"name", "Hungarian"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"hy",
		[{"name", "Armenian"},
			{"formula", "n > 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"ia",
		[{"name", "Interlingua"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"id",
		[{"name", "Indonesian"},
			{"formula", "0"},
			{"nplurals", 1},
			{"cases", ["other"]}]},
	{"ig",
		[{"name", "Igbo"},
			{"formula", "0"},
			{"nplurals", 1},
			{"cases", ["other"]}]},
	{"ii",
		[{"name", "Sichuan Yi"},
			{"formula", "0"},
			{"nplurals", 1},
			{"cases", ["other"]}]},
	{"io",
		[{"name", "Ido"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"is",
		[{"name", "Icelandic"},
			{"formula", "n % 10 != 1 || n % 100 == 11"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"it",
		[{"name", "Italian"},
			{"formula", "(n == 1) ? 0 : ((n != 0 && n % 1000000 == 0) ? 1 : 2)"},
			{"nplurals", 3},
			{"cases", ["one", "many", "other"]}]},
	{"iu",
		[{"name", "Inuktitut"},
			{"formula", "(n == 1) ? 0 : ((n == 2) ? 1 : 2)"},
			{"nplurals", 3},
			{"cases", ["one", "two", "other"]}]},
	{"ja",
		[{"name", "Japanese"},
			{"formula", "0"},
			{"nplurals", 1},
			{"cases", ["other"]}]},
	{"jbo",
		[{"name", "Lojban"},
			{"formula", "0"},
			{"nplurals", 1},
			{"cases", ["other"]}]},
	{"jgo",
		[{"name", "Ngomba"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"jmc",
		[{"name", "Machame"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"jv",
		[{"name", "Javanese"},
			{"formula", "0"},
			{"nplurals", 1},
			{"cases", ["other"]}]},
	{"jw",
		[{"name", "Javanese"},
			{"supersededBy", "jv"},
			{"formula", "0"},
			{"nplurals", 1},
			{"cases", ["other"]}]},
	{"ka",
		[{"name", "Georgian"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"kab",
		[{"name", "Kabyle"},
			{"formula", "n > 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"kaj",
		[{"name", "Jju"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"kcg",
		[{"name", "Tyap"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"kde",
		[{"name", "Makonde"},
			{"formula", "0"},
			{"nplurals", 1},
			{"cases", ["other"]}]},
	{"kea",
		[{"name", "Kabuverdianu"},
			{"formula", "0"},
			{"nplurals", 1},
			{"cases", ["other"]}]},
	{"kk",
		[{"name", "Kazakh"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"kkj",
		[{"name", "Kako"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"kl",
		[{"name", "Kalaallisut"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"km",
		[{"name", "Khmer"},
			{"formula", "0"},
			{"nplurals", 1},
			{"cases", ["other"]}]},
	{"kn",
		[{"name", "Kannada"},
			{"formula", "n > 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"ko",
		[{"name", "Korean"},
			{"formula", "0"},
			{"nplurals", 1},
			{"cases", ["other"]}]},
	{"ks",
		[{"name", "Kashmiri"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"ksb",
		[{"name", "Shambala"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"ksh",
		[{"name", "Colognian"},
			{"formula", "(n == 0) ? 0 : ((n == 1) ? 1 : 2)"},
			{"nplurals", 3},
			{"cases", ["zero", "one", "other"]}]},
	{"ku",
		[{"name", "Kurdish"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"kw",
		[{"name", "Cornish"},
			{"formula",
				"(n == 0) ? 0 : ((n == 1) ? 1 : (((n % 100 == 2 || n % 100 == 22 || n % 100 == 42 || n % 100 == 62 || n % 100 == 82) || n % 1000 == 0 && (n % 100000 >= 1000 && n % 100000 <= 20000 || n % 100000 == 40000 || n % 100000 == 60000 || n % 100000 == 80000) || n != 0 && n % 1000000 == 100000) ? 2 : ((n % 100 == 3 || n % 100 == 23 || n % 100 == 43 || n % 100 == 63 || n % 100 == 83) ? 3 : ((n != 1 && (n % 100 == 1 || n % 100 == 21 || n % 100 == 41 || n % 100 == 61 || n % 100 == 81)) ? 4 : 5))))"},
			{"nplurals", 6},
			{"cases",
				["zero", "one", "two", "few", "many", "other"]}]},
	{"ky",
		[{"name", "Kyrgyz"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"lag",
		[{"name", "Langi"},
			{"formula", "(n == 0) ? 0 : ((n == 1) ? 1 : 2)"},
			{"nplurals", 3},
			{"cases", ["zero", "one", "other"]}]},
	{"lb",
		[{"name", "Luxembourgish"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"lg",
		[{"name", "Ganda"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"lij",
		[{"name", "Ligurian"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"lkt",
		[{"name", "Lakota"},
			{"formula", "0"},
			{"nplurals", 1},
			{"cases", ["other"]}]},
	{"ln",
		[{"name", "Lingala"},
			{"formula", "n > 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"lo",
		[{"name", "Lao"},
			{"formula", "0"},
			{"nplurals", 1},
			{"cases", ["other"]}]},
	{"lt",
		[{"name", "Lithuanian"},
			{"formula",
				"(n % 10 == 1 && (n % 100 < 11 || n % 100 > 19)) ? 0 : ((n % 10 >= 2 && n % 10 <= 9 && (n % 100 < 11 || n % 100 > 19)) ? 1 : 2)"},
			{"nplurals", 3},
			{"cases", ["one", "few", "other"]}]},
	{"lv",
		[{"name", "Latvian"},
			{"formula",
				"(n % 10 == 0 || n % 100 >= 11 && n % 100 <= 19) ? 0 : ((n % 10 == 1 && n % 100 != 11) ? 1 : 2)"},
			{"nplurals", 3},
			{"cases", ["zero", "one", "other"]}]},
	{"mas",
		[{"name", "Masai"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"mg",
		[{"name", "Malagasy"},
			{"formula", "n > 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"mgo",
		[{"name", "Meta'"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"mk",
		[{"name", "Macedonian"},
			{"formula", "n % 10 != 1 || n % 100 == 11"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"ml",
		[{"name", "Malayalam"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"mn",
		[{"name", "Mongolian"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"mo",
		[{"name", "Moldavian"},
			{"supersededBy", "ro_MD"},
			{"formula",
				"(n == 1) ? 0 : ((n == 0 || n % 100 >= 2 && n % 100 <= 19) ? 1 : 2)"},
			{"nplurals", 3},
			{"cases", ["one", "few", "other"]}]},
	{"mr",
		[{"name", "Marathi"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"ms",
		[{"name", "Malay"},
			{"formula", "0"},
			{"nplurals", 1},
			{"cases", ["other"]}]},
	{"mt",
		[{"name", "Maltese"},
			{"formula",
				"(n == 1) ? 0 : ((n == 0 || n % 100 >= 2 && n % 100 <= 10) ? 1 : ((n % 100 >= 11 && n % 100 <= 19) ? 2 : 3))"},
			{"nplurals", 4},
			{"cases", ["one", "few", "many", "other"]}]},
	{"my",
		[{"name", "Burmese"},
			{"formula", "0"},
			{"nplurals", 1},
			{"cases", ["other"]}]},
	{"nah",
		[{"name", "Nahuatl"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"naq",
		[{"name", "Nama"},
			{"formula", "(n == 1) ? 0 : ((n == 2) ? 1 : 2)"},
			{"nplurals", 3},
			{"cases", ["one", "two", "other"]}]},
	{"nb",
		[{"name", "Norwegian Bokmål"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"nd",
		[{"name", "North Ndebele"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"ne",
		[{"name", "Nepali"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"nl",
		[{"name", "Dutch"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"nl_BE",
		[{"name", "Flemish"},
			{"territory", "Belgium"},
			{"baseLanguage", "Dutch"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"nn",
		[{"name", "Norwegian Nynorsk"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"nnh",
		[{"name", "Ngiemboon"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"no",
		[{"name", "Norwegian"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"nqo",
		[{"name", "N'Ko"},
			{"formula", "0"},
			{"nplurals", 1},
			{"cases", ["other"]}]},
	{"nr",
		[{"name", "South Ndebele"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"nso",
		[{"name", "Northern Sotho"},
			{"formula", "n > 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"ny",
		[{"name", "Nyanja"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"nyn",
		[{"name", "Nyankole"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"om",
		[{"name", "Oromo"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"or",
		[{"name", "Odia"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"os",
		[{"name", "Ossetic"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"osa",
		[{"name", "Osage"},
			{"formula", "0"},
			{"nplurals", 1},
			{"cases", ["other"]}]},
	{"pa",
		[{"name", "Punjabi"},
			{"formula", "n > 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"pap",
		[{"name", "Papiamento"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"pcm",
		[{"name", "Nigerian Pidgin"},
			{"formula", "n > 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"pl",
		[{"name", "Polish"},
			{"formula",
				"(n == 1) ? 0 : ((n % 10 >= 2 && n % 10 <= 4 && (n % 100 < 12 || n % 100 > 14)) ? 1 : 2)"},
			{"nplurals", 3},
			{"cases", ["one", "few", "other"]}]},
	{"prg",
		[{"name", "Prussian"},
			{"formula",
				"(n % 10 == 0 || n % 100 >= 11 && n % 100 <= 19) ? 0 : ((n % 10 == 1 && n % 100 != 11) ? 1 : 2)"},
			{"nplurals", 3},
			{"cases", ["zero", "one", "other"]}]},
	{"ps",
		[{"name", "Pashto"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"pt",
		[{"name", "Portuguese"},
			{"formula",
				"(n == 0 || n == 1) ? 0 : ((n != 0 && n % 1000000 == 0) ? 1 : 2)"},
			{"nplurals", 3},
			{"cases", ["one", "many", "other"]}]},
	{"pt_BR",
		[{"name", "Brazilian Portuguese"},
			{"territory", "Brazil"},
			{"baseLanguage", "Portuguese"},
			{"formula",
				"(n == 0 || n == 1) ? 0 : ((n != 0 && n % 1000000 == 0) ? 1 : 2)"},
			{"nplurals", 3},
			{"cases", ["one", "many", "other"]}]},
	{"pt_PT",
		[{"name", "European Portuguese"},
			{"territory", "Portugal"},
			{"baseLanguage", "Portuguese"},
			{"formula", "(n == 1) ? 0 : ((n != 0 && n % 1000000 == 0) ? 1 : 2)"},
			{"nplurals", 3},
			{"cases", ["one", "many", "other"]}]},
	{"rm",
		[{"name", "Romansh"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"ro",
		[{"name", "Romanian"},
			{"formula",
				"(n == 1) ? 0 : ((n == 0 || n % 100 >= 2 && n % 100 <= 19) ? 1 : 2)"},
			{"nplurals", 3},
			{"cases", ["one", "few", "other"]}]},
	{"ro_MD",
		[{"name", "Moldavian"},
			{"territory", "Moldova"},
			{"baseLanguage", "Romanian"},
			{"formula",
				"(n == 1) ? 0 : ((n == 0 || n % 100 >= 2 && n % 100 <= 19) ? 1 : 2)"},
			{"nplurals", 3},
			{"cases", ["one", "few", "other"]}]},
	{"rof",
		[{"name", "Rombo"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"ru",
		[{"name", "Russian"},
			{"formula",
				"(n % 10 == 1 && n % 100 != 11) ? 0 : ((n % 10 >= 2 && n % 10 <= 4 && (n % 100 < 12 || n % 100 > 14)) ? 1 : 2)"},
			{"nplurals", 3},
			{"cases", ["one", "few", "other"]}]},
	{"rwk",
		[{"name", "Rwa"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"sah",
		[{"name", "Sakha"},
			{"formula", "0"},
			{"nplurals", 1},
			{"cases", ["other"]}]},
	{"saq",
		[{"name", "Samburu"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"sat",
		[{"name", "Santali"},
			{"formula", "(n == 1) ? 0 : ((n == 2) ? 1 : 2)"},
			{"nplurals", 3},
			{"cases", ["one", "two", "other"]}]},
	{"sc",
		[{"name", "Sardinian"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"scn",
		[{"name", "Sicilian"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"sd",
		[{"name", "Sindhi"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"sdh",
		[{"name", "Southern Kurdish"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"se",
		[{"name", "Northern Sami"},
			{"formula", "(n == 1) ? 0 : ((n == 2) ? 1 : 2)"},
			{"nplurals", 3},
			{"cases", ["one", "two", "other"]}]},
	{"seh",
		[{"name", "Sena"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"ses",
		[{"name", "Koyraboro Senni"},
			{"formula", "0"},
			{"nplurals", 1},
			{"cases", ["other"]}]},
	{"sg",
		[{"name", "Sango"},
			{"formula", "0"},
			{"nplurals", 1},
			{"cases", ["other"]}]},
	{"sh",
		[{"name", "Serbo-Croatian"},
			{"formula",
				"(n % 10 == 1 && n % 100 != 11) ? 0 : ((n % 10 >= 2 && n % 10 <= 4 && (n % 100 < 12 || n % 100 > 14)) ? 1 : 2)"},
			{"nplurals", 3},
			{"cases", ["one", "few", "other"]}]},
	{"shi",
		[{"name", "Tachelhit"},
			{"formula",
				"(n == 0 || n == 1) ? 0 : ((n >= 2 && n <= 10) ? 1 : 2)"},
			{"nplurals", 3},
			{"cases", ["one", "few", "other"]}]},
	{"si",
		[{"name", "Sinhala"},
			{"formula", "n > 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"sk",
		[{"name", "Slovak"},
			{"formula", "(n == 1) ? 0 : ((n >= 2 && n <= 4) ? 1 : 2)"},
			{"nplurals", 3},
			{"cases", ["one", "few", "other"]}]},
	{"sl",
		[{"name", "Slovenian"},
			{"formula",
				"(n % 100 == 1) ? 0 : ((n % 100 == 2) ? 1 : ((n % 100 == 3 || n % 100 == 4) ? 2 : 3))"},
			{"nplurals", 4},
			{"cases", ["one", "two", "few", "other"]}]},
	{"sma",
		[{"name", "Southern Sami"},
			{"formula", "(n == 1) ? 0 : ((n == 2) ? 1 : 2)"},
			{"nplurals", 3},
			{"cases", ["one", "two", "other"]}]},
	{"smi",
		[{"name", "Sami"},
			{"formula", "(n == 1) ? 0 : ((n == 2) ? 1 : 2)"},
			{"nplurals", 3},
			{"cases", ["one", "two", "other"]}]},
	{"smj",
		[{"name", "Lule Sami"},
			{"formula", "(n == 1) ? 0 : ((n == 2) ? 1 : 2)"},
			{"nplurals", 3},
			{"cases", ["one", "two", "other"]}]},
	{"smn",
		[{"name", "Inari Sami"},
			{"formula", "(n == 1) ? 0 : ((n == 2) ? 1 : 2)"},
			{"nplurals", 3},
			{"cases", ["one", "two", "other"]}]},
	{"sms",
		[{"name", "Skolt Sami"},
			{"formula", "(n == 1) ? 0 : ((n == 2) ? 1 : 2)"},
			{"nplurals", 3},
			{"cases", ["one", "two", "other"]}]},
	{"sn",
		[{"name", "Shona"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"so",
		[{"name", "Somali"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"sq",
		[{"name", "Albanian"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"sr",
		[{"name", "Serbian"},
			{"formula",
				"(n % 10 == 1 && n % 100 != 11) ? 0 : ((n % 10 >= 2 && n % 10 <= 4 && (n % 100 < 12 || n % 100 > 14)) ? 1 : 2)"},
			{"nplurals", 3},
			{"cases", ["one", "few", "other"]}]},
	{"sr_ME",
		[{"name", "Montenegrin"},
			{"territory", "Montenegro"},
			{"baseLanguage", "Serbian"},
			{"formula",
				"(n % 10 == 1 && n % 100 != 11) ? 0 : ((n % 10 >= 2 && n % 10 <= 4 && (n % 100 < 12 || n % 100 > 14)) ? 1 : 2)"},
			{"nplurals", 3},
			{"cases", ["one", "few", "other"]}]},
	{"ss",
		[{"name", "Swati"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"ssy",
		[{"name", "Saho"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"st",
		[{"name", "Southern Sotho"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"su",
		[{"name", "Sundanese"},
			{"formula", "0"},
			{"nplurals", 1},
			{"cases", ["other"]}]},
	{"sv",
		[{"name", "Swedish"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"sw",
		[{"name", "Swahili"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"sw_CD",
		[{"name", "Congo Swahili"},
			{"territory", "Congo - Kinshasa"},
			{"baseLanguage", "Swahili"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"syr",
		[{"name", "Syriac"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"ta",
		[{"name", "Tamil"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"te",
		[{"name", "Telugu"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"teo",
		[{"name", "Teso"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"th",
		[{"name", "Thai"},
			{"formula", "0"},
			{"nplurals", 1},
			{"cases", ["other"]}]},
	{"ti",
		[{"name", "Tigrinya"},
			{"formula", "n > 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"tig",
		[{"name", "Tigre"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"tk",
		[{"name", "Turkmen"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"tl",
		[{"name", "Tagalog"},
			{"formula",
				"n != 1 && n != 2 && n != 3 && (n % 10 == 4 || n % 10 == 6 || n % 10 == 9)"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"tn",
		[{"name", "Tswana"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"to",
		[{"name", "Tongan"},
			{"formula", "0"},
			{"nplurals", 1},
			{"cases", ["other"]}]},
	{"tpi",
		[{"name", "Tok Pisin"},
			{"formula", "0"},
			{"nplurals", 1},
			{"cases", ["other"]}]},
	{"tr",
		[{"name", "Turkish"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"ts",
		[{"name", "Tsonga"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"tzm",
		[{"name", "Central Atlas Tamazight"},
			{"formula", "n >= 2 && (n < 11 || n > 99)"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"ug",
		[{"name", "Uyghur"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"uk",
		[{"name", "Ukrainian"},
			{"formula",
				"(n % 10 == 1 && n % 100 != 11) ? 0 : ((n % 10 >= 2 && n % 10 <= 4 && (n % 100 < 12 || n % 100 > 14)) ? 1 : 2)"},
			{"nplurals", 3},
			{"cases", ["one", "few", "other"]}]},
	{"ur",
		[{"name", "Urdu"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"uz",
		[{"name", "Uzbek"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"ve",
		[{"name", "Venda"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"vi",
		[{"name", "Vietnamese"},
			{"formula", "0"},
			{"nplurals", 1},
			{"cases", ["other"]}]},
	{"vo",
		[{"name", "Volapük"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"vun",
		[{"name", "Vunjo"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"wa",
		[{"name", "Walloon"},
			{"formula", "n > 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"wae",
		[{"name", "Walser"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"wo",
		[{"name", "Wolof"},
			{"formula", "0"},
			{"nplurals", 1},
			{"cases", ["other"]}]},
	{"xh",
		[{"name", "Xhosa"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"xog",
		[{"name", "Soga"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"yi",
		[{"name", "Yiddish"},
			{"formula", "n != 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]},
	{"yo",
		[{"name", "Yoruba"},
			{"formula", "0"},
			{"nplurals", 1},
			{"cases", ["other"]}]},
	{"yue",
		[{"name", "Cantonese"},
			{"formula", "0"},
			{"nplurals", 1},
			{"cases", ["other"]}]},
	{"zh",
		[{"name", "Chinese"},
			{"formula", "0"},
			{"nplurals", 1},
			{"cases", ["other"]}]},
	{"zh_Hans",
		[{"name", "Simplified Chinese"},
			{"script", "Simplified Han"},
			{"baseLanguage", "Chinese"},
			{"formula", "0"},
			{"nplurals", 1},
			{"cases", ["other"]}]},
	{"zh_Hant",
		[{"name", "Traditional Chinese"},
			{"script", "Traditional Han"},
			{"baseLanguage", "Chinese"},
			{"formula", "0"},
			{"nplurals", 1},
			{"cases", ["other"]}]},
	{"zu",
		[{"name", "Zulu"},
			{"formula", "n > 1"},
			{"nplurals", 2},
			{"cases", ["one", "other"]}]}]).

-spec get_specs(Language) -> Specs when
	Language :: term(),
	Specs :: undefined | proplists:proplist().
%% @doc Select and return a proplist containing language specifications.
%% If the language is not in our database, returns undefined.
get_specs(Language) ->
	case is_gettext_language(Language) of
		true ->
			case proplists:get_value(Language, ?T__LANGUAGES_SPECS, undefined) of
				undefined ->
					%% Try again with base language
					case base_language(Language) of
						Language -> undefined;
						BaseLanguage ->
							proplists:get_value(BaseLanguage, ?T__LANGUAGES_SPECS, undefined)
					end;
				Value -> Value
			end;
		false -> undefined
	end.

%% @doc Returns base language from a language string
base_language([C1, C2, $_ | _T]) -> [C1, C2];
base_language([C1, C2, C3, $_ | _T]) -> [C1, C2, C3];
base_language(L) -> L.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% is_
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec is_gettext_language(term()) -> true | false.
%% @doc Return true if the specified language is a valid gettext language string or false otherwise.
%% We don't actually validate all possible languages and locales/teritories/area codes, but we check the general format.
%% As a result the implementation remain compatible with all possible iso_639 past and future codes.
%%
%% Many libraries out there miss some rare languages codes (including the academic or dead languages)
%% because they try to validate based on a incomplete database.
%%
%% Please also notice that we are only interested in ISO 639 codes because
%% if we don't recognize the ISO 639/area code combination we fall back on ISO 639 (either revision 1 or 2)
%% code for determining the formula.
is_gettext_language(L=[_,_]) -> is_iso_639(L);
is_gettext_language([C1,C2,$_|_]) -> is_iso_639([C1, C2]);
is_gettext_language(L=[_,_,_]) -> is_iso_639(L);
is_gettext_language([C1,C2,C3,$_|_]) -> is_iso_639([C1, C2, C3]);
is_gettext_language(_) -> false.

is_iso_639([H|_]) when H < 97, H > 122 -> false;
is_iso_639([_|T]) -> is_iso_639(T);
is_iso_639([]) -> true.
