%% -*- coding: utf-8 -*-
%% Copyright (c) 2022, Madalin Grigore-Enescu <github@ergenius.com> <www.ergenius.com>
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
-module(t__srv).
-author("Madalin Grigore-Enescu").

-behaviour(gen_server).

-include("../include/t__.hrl").

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([config_set_call/3, config_set_cast/2]).
-export([config_delete_call/2, config_delete_cast/1]).

-record(t__srv_state, {
	configs = [] :: proplists:proplist(),
	dev_count = 0,
	timer_ref = undefined,
	timer_interval = 10000
}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec start_link(Args :: proplists:proplist()) -> {'ok', pid()} | 'ignore' | {'error', term()}.
%% @doc Creates a gen_server process as part of a supervision tree.
%% The function should be called, directly or indirectly, by the supervisor.
%% It will, among other things, ensure that the gen_server is linked to the supervisor.
start_link(Args) -> gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

%% @doc Config set
%% See t__:config_set/3 for more details
config_set_call(Application, Config, Timeout) -> gen_server:call(?MODULE, {config_set, Application, Config}, Timeout).

%% @doc Config set
%% See t__:config_set_cast/3 for more details
config_set_cast(Application, Config) -> gen_server:cast(?MODULE, {config_set, Application, Config}).

%% @doc Config delete
%% See t__:config_delete/2 for more details
config_delete_call(Application, Timeout) -> gen_server:call(?MODULE, {config_delete, Application}, Timeout).

%% @doc Config delete
%% See t__:config_delete_cast/2 for more details
config_delete_cast(Application) -> gen_server:cast(?MODULE, {config_delete, Application}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% INIT
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Init gen_server
%% Whenever a gen_server process is started using start/3,4 or start_link/3,4, this function is called by the new process to initialize.
%%
%% Args is the Args argument provided to the start function.
%% If the initialization is successful, the function is to return {ok,State}, {ok,State,Timeout}, or {ok,State,hibernate},
%% where State is the internal state of the gen_server process.
%%
%% If an integer time-out value is provided, a time-out occurs unless a request or a message is received within Timeout milliseconds.
%% A time-out is represented by the atom timeout, which is to be handled by the Module:handle_info/2 callback function.
%% The atom infinity can be used to wait indefinitely, this is the default value.
%%
%% If hibernate is specified instead of a time-out value, the process goes into hibernation when waiting for the next message to arrive
%% (by calling proc_lib:hibernate/3).
%%
%% If the initialization fails, the function is to return {stop, Reason}, where Reason is any term, or ignore.
init(Args) -> private_handle_init(Args).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% handle_call
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Handle gen_server calls
%% Whenever a gen_server process receives a request sent using call/2,3 or multi_call/2,3,4, this function is called to handle the request.
%%
%% Request is the Request argument provided to call or multi_call.
%% From is a tuple {Pid,Tag}, where Pid is the pid of the client and Tag is a unique tag.
%% State is the internal state of the gen_server process.
%%
%% If {reply,Reply,NewState} is returned, {reply,Reply,NewState,Timeout} or {reply,Reply,NewState,hibernate},
%% Reply is given back to From as the return value of call/2,3 or included in the return value of multi_call/2,3,4.
%% The gen_server process then continues executing with the possibly updated internal state NewState.
%%
%% For a description of Timeout and hibernate, see Module:init/1.
%%
%% If {noreply,NewState} is returned, {noreply,NewState,Timeout}, or {noreply,NewState,hibernate},
%% the gen_server process continues executing with NewState. Any reply to From must be specified explicitly using reply/2.
%%
%% If {stop,Reason,Reply,NewState} is returned, Reply is given back to From.
%%
%% If {stop,Reason,NewState} is returned, any reply to From must be specified explicitly using reply/2.
%% The gen_server process then calls Module:terminate(Reason,NewState) and terminates.

%% config_set
handle_call({config_set, Application, Config = #t__config{}}, _From,
	State = #t__srv_state{}) when erlang:is_atom(Application) ->
	{Reply, NewState} = private_handle_config_set(Application, Config, State),
	{reply, Reply, NewState};

%% config_delete
handle_call({config_delete, Application}, _From,
	State = #t__srv_state{}) when erlang:is_atom(Application) ->
	{Reply, NewState} = private_handle_config_delete(Application, State),
	{reply, Reply, NewState};

%% Handle any other call
handle_call(_Msg, _From, State) -> {reply, error, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% handle_cast
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Handle gen_server casts
%% Whenever a gen_server process receives a request sent using cast/2 or abcast/2,3, this function is called to handle the request.
%% For a description of the arguments and possible return values, see Module:handle_call/3.

%% config_set
handle_cast({config_set, Application, Config = #t__config{}},
	State = #t__srv_state{}) when erlang:is_atom(Application) ->
	{_, NewState} = private_handle_config_set(Application, Config, State),
	{noreply, NewState};

%% config_delete
handle_cast({config_delete, Application},
	State = #t__srv_state{}) when erlang:is_atom(Application) ->
	{_, NewState} = private_handle_config_delete(Application, State),
	{noreply, NewState};

%% Handle any other cast
handle_cast(_Msg, State) -> {noreply, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% handle_info
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Handle gen_server info
%% This function is called by a gen_server process when a time-out occurs or when it receives any other message
%% than a synchronous or asynchronous request (or a system message).
%% Info is either the atom timeout, if a time-out has occurred, or the received message.
%% For a description of the other arguments and possible return values, see Module:handle_call/3.

%% Handle monitor
handle_info(monitor, State) -> private_handle_monitor(State);

%% Handle any other info
handle_info(_Info, State) -> {noreply, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% terminate/code_change
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Handle gen_server terminate
%%
%% This function is called by a gen_server process when it is about to terminate.
%% It is to be the opposite of Module:init/1 and do any necessary cleaning up.
%% When it returns, the gen_server process terminates with Reason. The return value is ignored.
%%
%% Reason is a term denoting the stop reason and State is the internal state of the gen_server process.
%%
%% Reason depends on why the gen_server process is terminating. If it is because another callback function has returned a stop tuple {stop,..},
%% Reason has the value specified in that tuple. If it is because of a failure, Reason is the error reason.
%%
%% If the gen_server process is part of a supervision tree and is ordered by its supervisor to terminate, this function is called with Reason=shutdown
%% if the following conditions apply:
%% - The gen_server process has been set to trap exit signals.
%% - The shutdown strategy as defined in the child specification of the supervisor is an integer time-out value, not brutal_kill.
%%
%% Even if the gen_server process is not part of a supervision tree, this function is called if it receives an 'EXIT' message from its parent.
%% Reason is the same as in the 'EXIT' message.
%% Otherwise, the gen_server process terminates immediately.
%%
%% Notice that for any other reason than normal, shutdown, or {shutdown,Term}, the gen_server process is assumed to terminate
%% because of an error and an error report is issued using error_logger:format/2.
terminate(_Reason, State) -> private_handle_terminate(State).

%% @doc Handle gen_server code change
%% This function is called by a gen_server process when it is to update its internal state during a release upgrade/downgrade,
%% that is, when the instruction {update,Module,Change,...}, where Change={advanced,Extra}, is specifed in the appup file.
%% For more information, see section Release Handling Instructions in OTP Design Principles.
%%
%% For an upgrade, OldVsn is Vsn, and for a downgrade, OldVsn is {down,Vsn}. Vsn is defined by the vsn attribute(s) of the old version
%% of the callback module Module. If no such attribute is defined, the version is the checksum of the Beam file.
%%
%% State is the internal state of the gen_server process.
%% Extra is passed "as is" from the {advanced,Extra} part of the update instruction.
%%
%% If successful, the function must return the updated internal state.
%% If the function returns {error,Reason}, the ongoing upgrade fails and rolls back to the old release.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% private_handle
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%=============================================================
%% private_handle_init
%%=============================================================

%% @doc Init gen_server
private_handle_init(_Args) -> private_handle_init(application:loaded_applications(), #t__srv_state{}).
private_handle_init([{Application, _Description, _Vsn} | T],
	State = #t__srv_state{
		configs = Configs
	}) ->
	case t__:config_get_environment(Application) of
		{ok, Config} ->
			%% Add configured applications
			private_handle_init(T, State#t__srv_state{
				configs = [{Application, Config} | Configs]
			});
		_ ->
			%% Skip unconfigured applications
			private_handle_init(T, State)
	end;
private_handle_init([], State) ->
	{ok, private_state_on_change(private_update_applications(State)), infinity}.

%%=============================================================
%% private_handle_config_set
%%=============================================================

%% @doc config_set
private_handle_config_set(Application, Config = #t__config{}, State = #t__srv_state{}) ->
	{_, DeleteState} = private_handle_config_delete(Application, State),
	DeleteStateConfigs = DeleteState#t__srv_state.configs,
	{ok, private_state_on_change(private_update_applications(DeleteState#t__srv_state{
		configs = [{Application, Config} | DeleteStateConfigs]
	}))}.

%%=============================================================
%% private_handle_config_delete
%%=============================================================

%% @doc Delete specified application configuration
private_handle_config_delete(Application, State = #t__srv_state{
	configs = Configs
}) ->
	case proplists:get_value(Application, Configs, undefined) of
		#t__config{
			repositories = Repositories
		} ->
			private_handle_config_delete_repositories(Repositories, Application),
			NewConfigs = proplists:delete(Application, Configs),
			application:unset_env(Application, t__config),
			{ok, private_state_on_change(State#t__srv_state{configs = NewConfigs})};
		_ ->
			{{error, not_found}, State}
	end.

private_handle_config_delete_repositories([#t__repository{name=RepositoryName}|T], Application) ->
	TableMsgs = t__utils:ets_table_msgs(Application, RepositoryName),
	TableHeaders = t__utils:ets_table_headers(Application, RepositoryName),
	TablePot = t__utils:ets_table_pot(Application, RepositoryName),
	t__utils:ets_table_delete(TableMsgs),
	t__utils:ets_table_delete(TableHeaders),
	t__utils:ets_table_delete(TablePot),
	private_handle_config_delete_repositories([T], Application);
private_handle_config_delete_repositories([], _Application) -> ok.

%%=============================================================
%% private_handle_monitor
%%=============================================================

%% @doc Monitor all configured applications
private_handle_monitor(State) ->
	NewState = private_update_applications(State),
	{noreply, private_state_on_change(NewState#t__srv_state{
		timer_ref = undefined
	})}.

%%=============================================================
%% private_handle_terminate
%%=============================================================

%% @doc Handle gen_server terminate
private_handle_terminate(State = #t__srv_state{
	configs = [{Application, #t__config{
		repositories = Repositories}}|T]}) ->
	private_delete_repositories(Application, Repositories),
	private_handle_terminate(State#t__srv_state{configs =T});
private_handle_terminate(#t__srv_state{}) ->
	%% POT update
	ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% private_
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%=============================================================
%% private_update_applications
%%=============================================================

%% @doc Update all applications
private_update_applications(State=#t__srv_state{configs = Configs}) ->
	State#t__srv_state{configs = private_update_applications(Configs, [])}.
private_update_applications([{ApplicationName, Config}|T], Acum) ->
	NewConfig = private_update_application(ApplicationName, Config),
	private_update_applications(T, [{ApplicationName, NewConfig}|Acum]);
private_update_applications([], Acum) -> Acum.

%% @doc Iterate all application repositories and update them
%% Ignore applications without repositories and handle only applications that have repositories
private_update_application(_ApplicationName, Config = #t__config{repositories = undefined}) ->
	Config;
private_update_application(ApplicationName, Config = #t__config{
	repositories = Repositories
}) when erlang:is_list(Repositories) ->
	Config#t__config{
		repositories = private_update_application(ApplicationName, Repositories, [])
	}.

%% @doc Update the specified application
private_update_application(ApplicationName, [Repository = #t__repository{}|T], Acum) ->
	case t__repository:update(ApplicationName, Repository) of
		{ok, NewRepository} -> private_update_application(ApplicationName, T, [NewRepository|Acum]);
		Error ->
			?T__LOG(error, "Updating application repository failed with error."
			" Repository directory does not exist or it is not accesible.",
				[
					{application, ApplicationName},
					{repository, Repository},
					{error, Error}
				]),
			%% We are facing 3 choices here:
			%% 1. exit()
			%% 2. keep the repository and generate errors on each timer info
			%% 3. delete the repository and silence the errors
			%%
			%% Do not forget we may encounter this situation only when development mode is enabled.
		    %% I do believe it's better to chose 2 on a developer setup. Maybe the DEV can fix the problem
			%% without us crushing the application and continue his work of changing repositories
			%% so... do nothing about it!
			private_update_application(ApplicationName, T, [Repository|Acum])
	end;
private_update_application(_ApplicationName, _, Acum) -> Acum.

%%=============================================================
%% private_state_on_change
%%=============================================================

%% @doc Update state on configs change
private_state_on_change(State = #t__srv_state{configs = Configs, timer_ref = TimerRef, timer_interval = TimerInterval}) ->
	%% Count configs with dev enabled
	NewDevCount = private_state_on_change_dev_count(Configs),
	%% Start a new timer for monitoring PO file changes if necessary
	NewTimerRef = case {NewDevCount > 0, TimerRef} of
					  %% We don't have a timer, create new
					  {true, undefined} -> erlang:send_after(TimerInterval, self(), monitor);
					  %% We already have a timer, return it
					  {true, _} -> TimerRef;
					  %% Nothing left to monitor, no timer
					  {false, undefined} -> undefined;
					  %% Nothing left to monitor, stop existing timer
					  {false, _} -> erlang:cancel_timer(TimerRef, [{async, true}]), undefined
				  end,
	?T__LOG(debug, "t__srv state changed", [State#t__srv_state{dev_count = NewDevCount, timer_ref = NewTimerRef}]),
	State#t__srv_state{dev_count = NewDevCount, timer_ref = NewTimerRef}.

%% @doc Count the dev in configs proplist
private_state_on_change_dev_count(Configs) -> private_state_on_change_dev_count(Configs, 0).
private_state_on_change_dev_count([{_, #t__config{dev = true}} | T], Acum) -> private_state_on_change_dev_count(T, Acum + 1);
private_state_on_change_dev_count([_ | T], Acum) -> private_state_on_change_dev_count(T, Acum);
private_state_on_change_dev_count([], Acum) -> Acum.

%%=============================================================
%% private_delete_repositories
%%=============================================================

%% @doc Delete the specified application repositories
private_delete_repositories(Application, [H|T]) ->
	private_delete_repository(Application, H),
	private_delete_repositories(Application, T);
private_delete_repositories(_Application, []) -> ok.

%% @doc Delete the specified repository
private_delete_repository(Application, #t__repository{name = Name}) ->
	Table = t__utils:ets_table_msgs(Application, Name),
	t__utils:ets_table_delete(Table).



