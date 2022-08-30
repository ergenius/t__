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
-author("Madalin Grigore-Enescu").

%% @doc Default language code string.
%% 
%% Don't edit this! Overwrite it using the following ways:
%% - call TL__("language_code") in your process to setup the language per process. Best performance/higly recommended.
%% - call t__:config/1 or t__:config/2 to configure the language at the application level
%% - set the t__config environment variable for your application
%% - set the t__config environment variable for the t__ application. NOT recommended because 
%% it may affect any other applications on your node using the same method to setup the default language.
%% However if you want to modify the default behaviour of all applications running on your node, you can do this.
-define(T__DEFAULT_LANGUAGE, "en").

%% @doc Holds repository information
-record(t__repository, {

	%% Repository name
	%% This must be a unique non empty string (per application) and it is used to identify the repository
	%% when calling translate functions. You can use whatever format fits your application
	%% requirements, any UTF8 character is allowed in any order.
	name = undefined :: undefined : string(),

	%% Repository directory
	%% Path to the directory where PO files are located.
	%% Files must be placed directly into the directory, one file per language.
	%% It is recommended (but not required) that the name of the file to be the
	%% same with the PO file header Language.
	directory = undefined :: undefined : string(),

	%% This is internally used by our translation server to keep track
	%% of the repository files changes. If you set it, it will be replaced.
	%% It contains a list of {Filename, FileInfo} tuples when it's automatically
	%% populated by the PO monitor server or it's undefined when
	%% the application config development mode is disabled (false).
	files = undefined :: undefined | [{string(), term()}]

}).

-type t__repository() :: #t__repository{}.

%% @doc Record for holding t__ configuration options that you can use with t__:config to configure
%% t__ behaviour.
%%
%% This record is set as an environment variable per each application using t__
%% The record is used to determine how t__ behave individually for each application.
%% We use environment variables and we keep this out of the t__:t__srv because we want to 
%% avoid t__:t__srv calls as much as possible.
-record(t__config, {

	%% Application default language
	%% If it's undefined default t__ language will be used.
	language = ?T__DEFAULT_LANGUAGE :: undefined | string(),

	%% True for enabling developer mode
	%% Defaults to: false
	%%
	%% It is obviously highly recommended to DISABLE developer mode in production!
	%%
	%% When in developer mode the t__srv monitor monitor PO file changes and
	%% collect all translated strings into a POT file.
	%% The cache ETS tables are still used by the t__ but they will be evicted
	%% everytime the PO files are modified.
	dev = false :: true | false,

	%% Application PO & POT files repositories
	%% If you enable generation of the POT file at runtime, please make sure
	%% the repositories path is writable by the erlang node process because
	%% t__ will create a POT file there.
	repositories = [] :: [t__repository()]

}).

-type t__config() :: #t__config{}.

%% @doc Record for holding all parameters that you can use with T__ macro.
-record(t__p, {

	%% Application
	%% Defaults to: current calling process application or t__
	application = undefined :: atom(),

	%% Application repository
	%% Defaults to: "default"
	repository = "default" :: string() | binary() | atom(),

	%% Language
	%% Defaults to: undefined
	%% If it's undefined default calling process language will be used.
	language = undefined :: undefined | string() | binary() | atom(),

	%% Context
	%% Defaults to: undefined
	context = undefined :: undefined | string() | binary() | atom(),

	%% Message or messages (for plural forms) to translate
	%% Defaults to: undefined
	msg = undefined :: undefined | string() | binary() | atom() | [string()] | [binary()] | [atom()],

	%% Data to use for interpolation
	%% Defaults to: undefined
	%% If the translated string contains Erlang format sequences, data will be used to replace them
	%% as Data parameter to io_lib:format/2
	data = undefined :: undefined | [term()],

	%% Reference information is added as a comment line starting with #:
	%% into the generated POT file just above the msgid.
	%%
	%% It should contain a reference to the program’s source code related to the msgid
	%% you want to translate. This information can be used by the translator to
	%% locate and understand the context from where the message is comming.
	%% If you use the T__ macro this information is automatically set using
	%% ?FILE:?LINE ?MODULE:?FUNCTION_NAME/?FUNCTION_ARITY format.
	reference = undefined :: undefined | string()

}).

-type t__p() :: #t__p{}.

%% @doc Returns the current language for the calling process.
%% 
%% When you properly setup the language for the process, the expected time complexity for the current implementation 
%% of this macro is O(1) and the worst case time complexity is O(N), where N is the number of items in the process dictionary.
%% 
%% The function will perform the following steps in order to determine the default language:
%% - call erlang:get(t__language)
%% - call application:get_env(t__language)
%% - call application:get_env(t__, t__language)
%% - if no t__language environment key was set for both the current application or the t__ application
%% we return t__ default language defined in this hrl file
-define(T__LANGUAGE(), t__:language()).

%% @doc Sets the language for the calling process
%% The specified language will be used by all T__ macros and functions for the current process 
%% if no explicit language was specified.
%% Usage:
%% T__LANGUAGE("en").
%% T__LANGUAGE("es").
%% T__LANGUAGE("en_GB").
%% T__LANGUAGE("ro_RO").
%% T__LANGUAGE("gsw").
%% T__LANGUAGE("gsw_FR").
-define(T__LANGUAGE(Language), t__:language(Language)).

%% @doc Macro for creating a gettext reference
%% Refrence information is added as a comment line starting with #: 
%% into the generated POT file above the msgid.
%%
%% It should contain a reference to the program’s source code related to the msgid 
%% you want to translate. This information can be used by the translator to
%% locate and understand the context from where the message is comming.
%% If you use the T__ macro this information is automatically collected using 
%% ?FILE:?LINE ?MODULE:?FUNCTION_NAME/?FUNCTION_ARITY format
-define(T__REFERENCE(), io_lib:format("~p:~p - ~p:~p/~p", [?FILE, ?LINE, ?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY])).

%% @doc Translate singular or plural term, with or without context and repository, 
%% for the application to which the calling process belongs to.
%%
%% Singular terms:
%% ?T__("I have a joke about Erlang, but it requires a prologue.")
%% You can use binaries and atoms, however it is not recommended.
%% ?T__(<<"Erlang is user-friendly, it’s just picky about its friends!">>)
%% ?T__('Why can't you trust atoms? Because they make up everything!')
%%
%% Singular with context:
%% Context is useful for the translators to distinguish in between identical strings.
%% ?T__({"menu", "Save"})
%% ?T__({"menu", "Quit"})
%% ?T__({"button", "Save"})
%% ?T__({"button", "Cancel"})
%% Context can also be used to create proper translations based on grammatical gender.
%% ?T__({"female", "The cat belong to him/her"})
%% ?T__({"male", "The cat belong to him/her"})
%%
%% Singular with repository:
%% Repositories are used to have different translations sources directories for the same application.
%% For example let's assume your application has many different HTML templates,
%% each with his own translation directory.
%% ?T__({"template1", {"Simple term from repository template1"}})
%% ?T__({"template2", {"Simple term from repository template2"}})
%% Repository can be combined with context also.
%% ?T__({"template1", {"menu", "Save"}})
%% 
%% Singular terms with interpolation:
%% ?T__("$~2f", [3.56])
%%
%% Context can also be used to create the proper translation based on grammatical gender combined with interpolation:
%% ?T__({"female", "Her/his name is ~s"}, ["Marry"])
%% ?T__({"male", "Her/his name is ~s"}, ["John"])
%%
%% Plural terms with interpolation:
%% ?T__(["~B user", "~B users"], [3])
%%
%% Plural terms with context and interpolation:
%% ?T__({"female", ["~B file belongs to her/him", "~B files belong to her/him"]}, [3])
%% ?T__({"male", ["~B file belongs to her/him", "~B files belong to her/him"]}, [3])
%%
%% Plural terms with context, interpolation and repositories:
%% ?T__({"template1", {"female", ["~B file belongs to her/him", "~B files belong to her/him"]}}, [3])
%% ?T__({"template1", {"male", ["~B file belongs to her/him", "~B files belong to her/him"]}}, [3])
%%
%% You can also specify everything using the #t__p{} record as a single parameter to the T__ macro.
%% This is actually the performance wise way of doing it. You will save some extra functions calls necessary
%% to understand your tuples. All #t__p{} fields except msg are optional.
%% ?T__(#t__p{msg = "Hello world"}).
%% ?T__(#t__p{msg = "Her name is ~s", data = ["Marry"]}).
%% ?T__(#t__p{language = "ro", context = "female", msg = "Her/his name is ~s", data = ["Marry"]}).
%% ?T__(#t__p{repository = "module1", language = "ro", context = "male", msg = "Her/his name is ~s", data = ["John"]}).
%% ?T__(#t__p{application=myapp, repository = "module1", language = "ro", context = "female", msg = "Her/his name is ~s", data = ["Marry"]}).
%%
%% For your convenience a macro also exists with all possible parameters:
%% ?T__(Application, Repository, Language, Context, Msg, Data).
%%
%% As you probably noticed in the examples above, interpolation is using Erlang format control sequences 
%% (io:format style parameter).
%%
%% Only one parameter should be used for plural forms! This is a gettext recommendation, not a t__ limitation.
%% Actually t__ allows you to use as many variables you like at your own risk. But only the first variable
%% will be used when interpreting the gettext plural formula.
%% 
%% Why they did this? This is because there are languages that have more than 2 plural forms (6 for example).
%% Now imagine you mix 2 variables or more, each with his own 6 plural forms. 
%% You will force the translators team to create so many combinations that their 
%% job will become unpractical. Many programmers are not aware of this characteristic 
%% of various languages and mix more than one parameter with text in their implementations, 
%% making the translation for such languages, an impossible task.
%%
%% Gettext found an elegant and simple solution to this problem by recommending to use only
%% one variable per string that needs to be translated. If you have more than one
%% variables into a text that need to be translated simply split the phrase in separate strings 
%% containing each variable!
%%
%% What is gettext plural formula?
%%
%% The information about the plural form selection is stored in the header entry of the PO file 
%% (the one with the empty msgid string). The plural form information for english language looks like this:
%% Plural-Forms: nplurals=2; plural=n == 1 ? 0 : 1;
%% Now let's take a look on Polish language formula:
%% "Plural-Forms: nplurals=4; plural=(n==1 ? 0 : (n%10>=2 && n%10<=4) && (n%100<12 || n%100>14) ? 1 : n!=1 && (n%10>=0 && n%10<=1) || (n%10>=5 && n%10<=9) || (n%100>=12 && n%100<=14) ? 2 : 3);\n"
%% The nplurals value must be a decimal number which specifies how many different plural forms exist for this language. 
%% The string following plural is an expression which is using the C language syntax. 
%% Exceptions are that no negative numbers are allowed, numbers must be decimal, and the only variable allowed is n. 
%% This expression will be evaluated whenever T__ is called. The numeric value passed to T__ is then substituted for all 
%% uses of the variable n in the expression. The resulting value then must be greater or equal to zero and smaller 
%% than the value given as the value of nplurals.
-define(T__(P), t__:translate(P, undefined, ?T__REFERENCE())).
-define(T__(P, Data), t__:translate(P, Data, ?T__REFERENCE())).
-define(T__(Application, Repository, Language, Context, Msg, Data), t__:translate(#t__p{
	application = Application,
	repository = Repository,
	language = Language,
	context = Context,
	msg = Msg,
	data = Data,
	reference = ?T__REFERENCE()})).

%% @doc Log a message
-define(T__LOG(Level, Msg), io:format(user, "~nLOG:~p: ~p~n~n", [Level, Msg])).
-define(T__LOG(Level, Msg, Args), io:format(user, "~nLOG:~p: ~p~n~p~n~n", [Level, Msg, Args])).