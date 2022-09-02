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
-module(t__eunit).
-author("Madalin Grigore-Enescu").

-define(DEBUG, true).

-include_lib("eunit/include/eunit.hrl").
-include_lib("t__/include/t__.hrl").

-define(T__APP_PATH, filename:join([filename:dirname(filename:dirname(code:which(?MODULE)))])).

%% Writing test generating functions
%%
%% A drawback of simple test functions is that you must write a separate function (with a separate name) for each test case.
%% A more compact way of writing tests (and much more flexible, as we shall see), is to write functions that return tests, instead of being tests.
%%
%% A function with a name ending in ..._test_() (note the final underscore) is recognized by EUnit as a test generator function.
%% Test generators return a representation of a set of tests to be executed by EUnit.
%%
%% Representing a test as data The most basic representation of a test is a single fun-expression that takes no arguments.
%% For example, the following test generator:
%%
%% basic_test_() ->
%% fun () -> ?assert(1 + 1 =:= 2) end.
%% will have the same effect as the following simple test:
%% simple_test() ->
%% ?assert(1 + 1 =:= 2).
%% (in fact, EUnit will handle all simple tests just like it handles fun-expressions: it will put them in a list, and run them one by one).

%% Test application
application_test_() ->

	{setup,

		%% Setup function
		fun() ->

			%% Application config
			Config = #t__config{
				language = "ro",
				dev = true,
				repositories = [
					#t__repository{
						name = "default",
						directory = filename:join([?T__APP_PATH, "repositories/default"])
					},
					#t__repository{
						name = "template1",
						directory = filename:join([?T__APP_PATH, "repositories/template1"])
					}
				]
			},

			%% Set the application environment
			ok = application:set_env(t__, t__, Config),

			%% Start application for the test
			ok = application:ensure_started(sasl),
			ok = application:ensure_started(t__),

			?T__LOG(debug, "Test cleanup function called."),

			timer:sleep(5000)

		end,

		%% Cleanup function
		fun(_) ->
			?T__LOG(debug, "Test cleanup function called.")
		end,

		%% Run all tests
		test_all()

	}.

test_all() ->

	[

		fun () -> ?T__LOG(debug, ?T__("~4.2f", [3.56])), ok end,
		fun () -> ?T__LOG(debug, ?T__("Privacy Policy")), ok end,
		fun () -> ?T__LOG(debug, ?T__({"web-app/navbar", "About us"})), ok end,
		fun () -> ?T__LOG(debug, ?T__("Project name: ~s", [t__])), ok end,
		fun () -> ?T__LOG(debug, ?T__({"android-app", "Version: ~s"}, ["1.0.0"])), ok end,
		fun () -> ?T__LOG(debug, ?T__({"female", "Her/his name is ~s"}, ["Irina"])), ok end,
		fun () -> ?T__LOG(debug, ?T__({"male", "Her/his name is ~s"}, ["Madalin"])), ok end,
		fun () -> ?T__LOG(debug, ?T__(["~B file", "~B files"], [0])), ok end,
		fun () -> ?T__LOG(debug, ?T__(["~B file", "~B files"], [1])), ok end,
		fun () -> ?T__LOG(debug, ?T__(["~B file", "~B files"], [234])), ok end,
		fun () -> ?T__LOG(debug, ?T__({"android-app/search", ["~B result", "~B results"]}, [0])), ok end,
		fun () -> ?T__LOG(debug, ?T__({"android-app/search", ["~B result", "~B results"]}, [1])), ok end,
		fun () -> ?T__LOG(debug, ?T__({"android-app/search", ["~B result", "~B results"]}, [234])), ok end,
		fun () -> ?T__LOG(debug, ?T__({"web-app/search", ["~B result", "~B results"]}, [0])), ok end,
		fun () -> ?T__LOG(debug, ?T__({"web-app/search", ["~B result", "~B results"]}, [1])), ok end,
		fun () -> ?T__LOG(debug, ?T__({"web-app/search", ["~B result", "~B results"]}, [234])), ok end,
		fun () -> ?T__LOG(debug, ?T__({"female", ["~B file belongs to her/him", "~B files belong to her/him"]}, [0])), ok end,
		fun () -> ?T__LOG(debug, ?T__({"female", ["~B file belongs to her/him", "~B files belong to her/him"]}, [1])), ok end,
		fun () -> ?T__LOG(debug, ?T__({"female", ["~B file belongs to her/him", "~B files belong to her/him"]}, [4675])), ok end,
		fun () -> ?T__LOG(debug, ?T__({"male", ["~B file belongs to her/him", "~B files belong to her/him"]}, [0])), ok end,
		fun () -> ?T__LOG(debug, ?T__({"male", ["~B file belongs to her/him", "~B files belong to her/him"]}, [1])), ok end,
		fun () -> ?T__LOG(debug, ?T__({"male", ["~B file belongs to her/him", "~B files belong to her/him"]}, [586])), ok end,
		fun () -> ?T__LOG(debug, ?T__("Multiline\nstring")), ok end,
		fun () -> ?T__LOG(debug, ?T__("\\a \\b \\f \n \\r \t \\v \\' \" \\? \\")), ok end,

		%% Singular
		?_assert("Politica de confidentialitate" =:= ?T__("Privacy Policy")),
		?_assert("Politica de confidentialitate" =:= ?T__(<<"Privacy Policy">>)),
		?_assert("Politica de confidentialitate" =:= ?T__('Privacy Policy')),

		%% Singular entry with context
		?_assert("Despre noi" =:= ?T__({"web-app/navbar", "About us"})),

		%% Singular entry with interpolation
		?_assert("Nume proiect: t__" =:= ?T__("Project name: ~s", [t__])),

		%% Singular entry with context and interpolation
		?_assert("Versiune android: 1.0.0" =:= ?T__({"android-app", "Version: ~s"}, ["1.0.0"])),

		%% Grammatical gender based on context combined with interpolation
		?_assert("Numele ei este Irina" =:= ?T__({"female", "Her/his name is ~s"}, ["Irina"])),
		?_assert("Numele lui este Madalin" =:= ?T__({"male", "Her/his name is ~s"}, ["Madalin"])),

		%% Plural entry with interpolation
		?_assert("0 fisiere" =:= ?T__(["~B file", "~B files"], [0])),
		?_assert("1 fisier" =:= ?T__(["~B file", "~B files"], [1])),
		?_assert("234 fisiere" =:= ?T__(["~B file", "~B files"], [234])),

		%% Plural entry with context and interpolation
		?_assert("Android: 0 rezultate" =:= ?T__({"android-app/search", ["~B result", "~B results"]}, [0])),
		?_assert("Android: 1 rezultat" =:= ?T__({"android-app/search", ["~B result", "~B results"]}, [1])),
		?_assert("Android: 234 rezultate" =:= ?T__({"android-app/search", ["~B result", "~B results"]}, [234])),
		?_assert("Web: 0 rezultate" =:= ?T__({"web-app/search", ["~B result", "~B results"]}, [0])),
		?_assert("Web: 1 rezultat" =:= ?T__({"web-app/search", ["~B result", "~B results"]}, [1])),
		?_assert("Web: 234 rezultate" =:= ?T__({"web-app/search", ["~B result", "~B results"]}, [234])),

		%% Plural entry with context used for grammatical gender and interpolation
		?_assert("0 fisiere apartin ei" =:= ?T__({"female", ["~B file belongs to her/him", "~B files belong to her/him"]}, [0])),
		?_assert("1 fisier apartine ei" =:= ?T__({"female", ["~B file belongs to her/him", "~B files belong to her/him"]}, [1])),
		?_assert("4675 fisiere apartin ei" =:= ?T__({"female", ["~B file belongs to her/him", "~B files belong to her/him"]}, [4675])),
		?_assert("0 fisiere apartin lui" =:= ?T__({"male", ["~B file belongs to her/him", "~B files belong to her/him"]}, [0])),
		?_assert("1 fisier apartine lui" =:= ?T__({"male", ["~B file belongs to her/him", "~B files belong to her/him"]}, [1])),
		?_assert("586 fisiere apartin lui" =:= ?T__({"male", ["~B file belongs to her/him", "~B files belong to her/him"]}, [586])),

		%% Singular entry demonstrating multiline strings.
		?_assert("Sir de caractere\nmulti-line" =:= ?T__("Multiline\nstring")),

		%% Singular entry demonstrating multiline strings and escape sequences
		?_assert("\\a \\b \\f \n \\r \t \\v \\' \" \\? \\" =:= ?T__("\\a \\b \\f \n \\r \t \\v \\' \" \\? \\"))

	].



