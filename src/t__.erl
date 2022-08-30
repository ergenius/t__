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
-module(t__).
-author("Madalin Grigore-Enescu").

-include("../include/t__.hrl").

-export([
	application/0,
	language/0,

	config_get/0, config_get/1, config_get_environment/1,
	config_set/1, config_set/2, config_set/3, config_set_cast/2,
	config_delete/0, config_delete/1, config_delete/2, config_delete_cast/1,

	translate/1, translate/2, translate/3, translate/7
]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% application/language
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

-spec application() -> 't__' | Application when
	Application :: atom().
%% @doc Returns the name of the application to which the calling process belongs.
%% If the specified process does not belong to any application the function returns 't__'.
application() ->
	case application:get_application() of
		{ok, Application} -> Application;
		_ -> t__
	end.

-spec language() -> Language when
	Language :: string().
%% @doc Returns the current language for the calling process.
%% 
%% When you properly setup the language for the process, the expected time complexity for the current implementation 
%% of this macro is O(1) and the worst case time complexity is O(N), where N is the number of items in the process dictionary.
%% 
%% The function will perform the following steps in order to determine the default language:
%% - call erlang:get(t__language)
%% - call application:get_env(t__config)
%% - call application:get_env(t__, t__config)
%% - if no t__config environment key was set for both the current application or the t__ application
%% we return t__ default language.
language() ->
	case erlang:get(t__language) of
		undefined ->
			#t__config{
				language = Language
			} = config_get(),
			Language;
		PDLanguage -> PDLanguage
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% config
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

%%-------------------------------------------------------------
%% config_get
%%-------------------------------------------------------------

-spec config_get() -> Config when
	Config :: string().
%% @doc Returns the current configuration for the calling process.
%% 
%% Please notice that the language config record field may not be the same
%% to the calling process application config because language can also be configured 
%% at the process level. (The process level language overwrites the application
%% configured language).
%%
%% If you want to get the config holding application language, call t__:config/1 function.
%%
%% The function will perform the following steps:
%% - call erlang:get(t__language) to get any calling process specific language that will overwrite 
%% the language returned from the next calls
%% - call application:get_env(t__config)
%% - call application:get_env(t__, t__config)
%% - if no t__config environment key was set for both the current application or the t__ application
%% we return t__ default config
config_get() ->
	PDLanguage = erlang:get(t__language),
	Application = application(),
	Config = config_get(Application),
	%% Process dictionary (may) overwrite application language
	case erlang:is_list(PDLanguage) of
		true -> Config#t__config{language = PDLanguage};
		_ -> Config
	end.

-spec config_get(Application) -> Config when
	Application :: atom(),
	Config :: string().
%% @doc Returns the current configuration for the specified application.
%%
%% The function will perform the following steps:
%% - call application:get_env(t__config)
%% - call application:get_env(t__, t__config)
%% - if no t__config environment key was set for both the current application or the t__ application
%% we return t__ default config
config_get(Application) ->
	case config_get_environment(Application) of
		{ok, Config} -> Config;
		_ ->
			case config_get_environment(t__) of
				{ok, TConfig} -> TConfig;
				_ -> #t__config{}
			end
	end.

%% @doc Returns the value of configuration t__config for Application
config_get_environment(Application) ->
	case application:get_env(Application, t__) of
		{ok, Config = #t__config{}} -> {ok, Config};
		{ok, _InvalidConfig} ->
			?T__LOG(emergency, "The value of configuration t__config for application is invalid!",
				[{application, Application}]),
			erlang:error(invalid_configuration);
		_ -> undefined
	end.

%%-------------------------------------------------------------
%% config_set
%%-------------------------------------------------------------

-spec config_set(Config) -> ok | {error, Error} when
	Config :: t__config(),
	Error :: term().
%% @doc Sets a new configuration for the calling process application with infinity timeout.
%% @see t__:config_set/3
config_set(Config) -> config_set(application(), Config, infinity).

-spec config_set(Application, Config) -> ok | {error, Error} when
	Application :: atom(),
	Config :: t__config(),
	Error :: term().
%% @doc Sets a new configuration for the specified application with infinity timeout.
%% @see t__:config_set/3
config_set(Application, Config) -> config_set(Application, Config, infinity).

-spec config_set(Application, Config, Timeout) -> ok | {error, Error} when
	Application :: atom(),
	Config :: t__config(),
	Timeout :: timeout(),
	Error :: term().
%% @doc Sets a new configuration for the specified application with the specified timeout.
%%
%% There is a blocking gen_server call behind this!
%% That's why there is a config_set_cast version of this function for convenience.
%% However I don't recommend using that because you will not know if the operation failed.
%%
%% Changing applications config is possible but should not be abused.
%% The system was designed for you to be able to change it at the application start or
%% from an administration panel, from time to time.
config_set(Application, Config = #t__config{}, Timeout) -> t__srv:config_set_call(Application, Config, Timeout).

-spec config_set_cast(Application, Config) -> ok | {error, Error} when
	Application :: atom(),
	Config :: t__config(),
	Error :: term().
%% @doc Sets a new configuration for the specified application asynchronous
%% Sends an asynchronous request to the process handling the implementation 
%% and returns ok immediately.
%%
%% @see t__:config_set/3
config_set_cast(Application, Config = #t__config{}) -> t__srv:config_set_cast(Application, Config).

%%-------------------------------------------------------------
%% config_delete
%%-------------------------------------------------------------

-spec config_delete() -> ok | {error, Error} when
	Error :: term().
%% @doc Deletes configuration for the calling process application with infinity timeout.
%% @see t__:config_delete/2
config_delete() -> config_set(application(), infinity).

-spec config_delete(Application) -> ok | {error, Error} when
	Application :: atom(),
	Error :: term().
%% @doc Deletes configuration for the specified application with infinity timeout.
%% @see t__:config_delete/2
config_delete(Application) -> config_delete(Application, infinity).

-spec config_delete(Application, Timeout) -> ok | {error, Error} when
	Application :: atom(),
	Timeout :: timeout(),
	Error :: term().
%% @doc Deletes configuration for the specified application with the specified timeout.
%%
%% There is a blocking gen_server call behind this!
%% That's why there is a config_delete_cast version of this function for convenience.
%%
%% Deleting applications config is possible but should not be abused.
%% The system was designed for you to be able to change it at the application start or
%% from an administration panel, from time to time.
config_delete(Application, Timeout) -> t__srv:config_delete_call(Application, Timeout).

-spec config_delete_cast(Application) -> ok | {error, Error} when
	Application :: atom(),
	Error :: term().
%% @doc Deletes configuration for the specified application asynchronous
%% Sends an asynchronous request to the process handling the implementation 
%% and returns ok immediately.
%%
%% @see t__:config_delete/2
config_delete_cast(Application) -> t__srv:config_delete_cast(Application).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% translate
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec translate(Param) -> Msg when
	Param :: term(),
	Msg :: string().
%% @doc Translate a singular or plural term, with or without repository, language and context
%% This is the main translate function that does everything.
%% using #t__p
translate(#t__p{
	application = Application,
	repository = Repository,
	language = Language,
	context = Context,
	msg = Msg,
	data = Data,
	reference = Reference
}) ->
	Application1 = case Application of
					   undefined -> erlang:atom_to_list(application());
					   _ -> erlang:atom_to_list(Application)
				   end,
	Repository1 = case Repository of
					  undefined -> "default";
					  _ -> translate_param_cast_term(Repository)
				  end,
	Language1 = case Language of
					undefined -> language();
					_ -> translate_param_cast_term(Language)
				end,
	Context1 = case Context of
				   undefined -> undefined;
				   _ -> translate_param_cast_term(Context)
			   end,
	Msg1 = translate_param_cast_msg(Msg),
	true = ((Data =:= undefined) or erlang:is_list(Data)),
	true = ((Reference =:= undefined) or erlang:is_list(Reference)),
	translate_private(Application1, Repository1, Language1, Context1, Msg1, Data, Reference);
translate(P) -> translate(P, undefined, undefined).

-spec translate(Param, Data) -> Msg when
	Param :: term(),
	Data :: undefined | list(),
	Msg :: string().
%% @doc Translate with data
%% @see t__:translate/3
translate(P, Data) -> translate(P, Data, undefined).

-spec translate(Param, Data, Reference) -> Msg when
	Param :: term(),
	Data :: undefined | list(),
	Reference :: undefined | string(),
	Msg :: string().
%% @doc Translate with data and reference as separate parameters
%% using #t__p
translate(P = #t__p{}, Data, Reference) ->
	translate(P#t__p{data = Data, reference = Reference});
%% With proplist
translate(P = [{_K, _V} | _T], Data, Reference) ->
	translate(translate_param_proplists_to_p(P, Data, Reference));
%% With repository and term
translate({Repository, {Term}}, Data, Reference) ->
	translate(translate_param_term_to_p(Term, Repository, undefined, Data, Reference));
%% With repository, language and term
translate({Repository, Language, {Term}}, Data, Reference) ->
	translate(translate_param_term_to_p(Term, Repository, Language, Data, Reference));
%% Without repository and language
translate(Term, Data, Reference) ->
	translate(translate_param_term_to_p(Term, undefined, undefined, Data, Reference)).

-spec translate(Application, Repository, Language, Context, Msg, Data, Reference) -> TranslatedMsg when
	Application :: undefined | atom(),
	Repository :: undefined | string() | atom() | binary(),
	Language :: undefined | string() | atom() | binary(),
	Context :: undefined | string() | atom() | binary(),
	Msg :: string() | atom() | binary(),
	Data :: undefined | list(),
	Reference :: undefined | string(),
	TranslatedMsg :: string().
%% @doc Translate with all separate parameters.
translate(Application, Repository, Language, Context, Msg, Data, Reference) ->
	translate(#t__p{
		application = Application,
		repository = Repository,
		language = Language,
		context = Context,
		msg = Msg,
		data = Data,
		reference = Reference
	}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% translate_param_
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%-------------------------------------------------------------
%% translate_param_proplists_to_p
%%-------------------------------------------------------------

%% @doc Convert proplists translate parameter to #t__p{}
translate_param_proplists_to_p(P, Data, Reference) ->
	#t__p{
		application = proplists:get_value(application, P, undefined),
		repository = proplists:get_value(repository, P, undefined),
		language = proplists:get_value(language, P, undefined),
		context = proplists:get_value(context, P, undefined),
		msg = proplists:get_value(msg, P, undefined),
		data = proplists:get_value(data, P, Data),
		reference = proplists:get_value(reference, P, Reference)
	}.

%%-------------------------------------------------------------
%% translate_param_term_to_p
%%-------------------------------------------------------------

% -spec translate_param_term_to_p(Term, Repository, Language, Data, Reference) -> t__p() when 
%     Repository :: undefined | string() | binary() | atom(),
%     Language :: undefined | string() | binary() | atom(),
%     Data :: undefined | [term()],
%     Reference :: undefined | string() | binary() | atom(),
%     Term :: string() | binary() | atom(),

%% @doc Convert term to #t__p{}
%% Singular terms:
%% "I have a joke about Erlang, but it requires a prologue."
%% <<"Erlang is user-friendly, it’s just picky about its friends!">>
%% 'Why can't you trust atoms? Because they make up everything!'
%% Plural terms:
%% ["user", "users"]
%% Plural terms with interpolation:
%% ["~B user", "~B users"]
translate_param_term_to_p(Msg, Repository, Language, Data, Reference) when
	erlang:is_list(Msg); erlang:is_binary(Msg); erlang:is_atom(Msg) -> #t__p{
	repository = Repository,
	language = Language,
	msg = Msg,
	data = Data,
	reference = Reference};
%% Singular with context:
%% {"menu", "Save"}
%% Plural terms with context:
%% {"female", ["File belongs to her", "Files belong to her"]}
%% {"male", ["File belongs to him", "Files belong to him"]}
%% Plural terms with context and interpolation:
%% {"female", ["~B file belongs to her", "~B files belong to her"]}
%% {"male", ["~B file belongs to him", "~B files belong to him"]}
translate_param_term_to_p({Context, Msg}, Repository, Language, Data, Reference) ->
	#t__p{
		repository = Repository,
		language = Language,
		context = Context,
		msg = Msg,
		data = Data,
		reference = Reference}.

%%-------------------------------------------------------------
%% translate_param_cast_term
%%-------------------------------------------------------------

%% @doc Convert string, atom, binary to string
translate_param_cast_term(Term) when is_list(Term) ->
	true = io_lib:char_list(Term),
	Term;
translate_param_cast_term(Term) when is_binary(Term) ->
	case unicode:characters_to_list(Term) of
		L when is_list(L) -> translate_param_cast_term(L); %% Unicode
		_ -> erlang:binary_to_list(Term) %% bytewise encoded
	end;
translate_param_cast_term(Term) when is_atom(Term) -> erlang:atom_to_list(Term).

%%-------------------------------------------------------------
%% translate_param_cast_msg
%%-------------------------------------------------------------

%% @doc Convert list of strings, atoms, binaries to list of strings
translate_param_cast_msg([Msg1]) when erlang:is_list(Msg1); erlang:is_binary(Msg1); erlang:is_atom(Msg1)
	-> [translate_param_cast_term(Msg1)];
translate_param_cast_msg([Msg1, Msg2]) when erlang:is_list(Msg1); erlang:is_binary(Msg1); erlang:is_atom(Msg1)
	-> [translate_param_cast_term(Msg1), translate_param_cast_term(Msg2)];
translate_param_cast_msg(M) when erlang:is_list(M) -> [M];
translate_param_cast_msg(M) when erlang:is_binary(M) ->
	case unicode:characters_to_list(M) of
		L when is_list(L) -> [L]; %% Unicode
		_ -> [erlang:binary_to_list(M)] %% bytewise encoded
	end;
translate_param_cast_msg(M) when erlang:is_atom(M) -> [erlang:atom_to_list(M)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% translate_private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Private translate function that expects proper parameters
%% handle singular messages
translate_private(Application, Repository, Language, Context, Msg, Data, Reference) ->

	TableMsgs = t__utils:ets_table_msgs(Application, Repository),
	Key = {Language, {Context, Msg}},
	try
		case ets:lookup(TableMsgs, Key) of
			[] ->
				TablePot = t__utils:ets_table_pot(Application, Repository),
				try
					ets:insert(TablePot, {Key, Msg, Reference}),
					translate_private_sp_missing(Msg, Data, Application, Repository, Language)
				catch
					Exception:Reason ->
						?T__LOG(error, "ets:insert Exception!",
							[
								{exception, {Exception, Reason}},
								{table_pot, TablePot},
								{application, Application},
								{repository, Repository},
								{language, Language},
								{context, Context},
								{msg, Msg},
								{data, Data},
								{reference, Reference}
							]),
						translate_private_sp_missing(Msg, Data, Application, Repository, Language)
				end;
			[{_Key, Value}] ->
				translate_private_sp(Value, Data, Application, Repository, Language)
		end
	catch
		Exception1:Reason1 ->
			?T__LOG(error, "ets:lookup Exception!",
				[
					{exception, {Exception1, Reason1}},
					{table_msg, TableMsgs},
					{application, Application},
					{repository, Repository},
					{language, Language},
					{context, Context},
					{msg, Msg},
					{data, Data},
					{reference, Reference}
				]),
			translate_private_sp_missing(Msg, Data, Application, Repository, Language)
	end.

%% @doc Detect and translate single or plural terms when the translation is missing
%% Single terms
translate_private_sp_missing([Msg], Data, _Application, _Repository, _Language) ->
	translate_private_format(Msg, Data);
%% Plural terms
translate_private_sp_missing([M1|_T], Data = [N | _], _Application, _Repository, _Language) when N == 1 ->
	translate_private_format(M1, Data);
translate_private_sp_missing([_M1,M2|_T], Data, _Application, _Repository, _Language) ->
	translate_private_format(M2, Data).

%% @doc Detect and translate single or plural terms
%% Single terms
translate_private_sp([Msg], Data, _Application, _Repository, _Language) ->
	translate_private_format(Msg, Data);
%% Plural terms
translate_private_sp(Msg, Data = [N | _], _Application, _Repository, Language) ->
	SelectedMsg = t__plural:select(Language, N, Msg),
	translate_private_format(SelectedMsg, Data).

%% @doc Returns a character list that represents Data formatted in accordance with Format
%% If any exception is raised, returns original Msg unaltered.
translate_private_format(Msg, Data) when erlang:is_list(Data); erlang:length(Data) > 0 ->
	try
		lists:flatten(io_lib:format(Msg, Data))
	catch
		Exception:Reason ->
			?T__LOG(error, "Exception for io_lib:format(Msg, Data)",
				[
					{exception, {Exception, Reason}},
					{msg, Msg},
					{data, Data}
				]),
			Msg
	end;
translate_private_format(Msg, _Data) when erlang:is_list(Msg) -> Msg.
