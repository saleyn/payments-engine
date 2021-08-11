%%% vim:ts=2:sw=2:et
%%%-----------------------------------------------------------------------------
%%% @doc       Payment processing application startup module
%%% @author    Serge Aleynikov <saleyn(at)gmail.com>
%%% @end
%%%-----------------------------------------------------------------------------
%%% Created: 2021-08-10
%%%-----------------------------------------------------------------------------
-module(payments_app).

-behaviour(application).
-behaviour(supervisor).

%% API
-export([bootstrap/1]).

%% Application callbacks
-export([start/2, stop/1, config_change/3]).

%% Supervisor callbacks
-export([init/1]).

-include("payments.hrl").

%%%-----------------------------------------------------------------------------
%%% External API
%%%-----------------------------------------------------------------------------

%% @doc Bootstrap the database
bootstrap(DbDir) when is_list(DbDir) ->
  net_kernel:nodename() == ignored
    andalso throw("Missing -sname Nodename startup argument"),
  application:set_env(mnesia, dir, DbDir),
  check_tables().

%%%-----------------------------------------------------------------------------
%%% Application callbacks
%%%-----------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @private
%% @doc Called by application behavior to start application by creating
%%      top-level supervisor. `StartArgs' are taken from `{mod, StartArgs}'
%%      option in the `*.app' file.
%% @see application
%% @end
%%------------------------------------------------------------------------------
-spec start(normal | {takeover,Node::atom()} | {failover,Node::atom()}, list()) ->
        {ok, pid()}.
start(_StartType, _StartArgs) ->
  {ok, Dir} = application:get_env(mnesia, dir),
  bootstrap(Dir),
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%------------------------------------------------------------------------------
%% @private
%% @doc Called by application behavior to stop application.
%% @see application
%% @end
%%------------------------------------------------------------------------------
stop(_State) ->
  ok.

%%------------------------------------------------------------------------------
%% @private
%% @doc Called by application behavior on change in application's environment.
%% @see application
%% @end
%%------------------------------------------------------------------------------
-spec config_change([{atom(),any()}], [{atom(),any()}], [{atom(),any()}]) -> ok.
config_change(_Changed, _New, _Removed) ->
  ok.

%%%-----------------------------------------------------------------------------
%%% Supervisor callbacks
%%%-----------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @private
%% @doc Supervison
%% @see supervisor:init/1
%% @end
%%------------------------------------------------------------------------------
init([]) ->
  Procs = [
    %% Payments processing engine
    #{id       => payments_engine,
      start    => {payments_engine,start_link,[]},  % StartFun = {M, F, A}
      restart  => permanent,                        % Restart  = permanent | transient | temporary
      shutdown => 4000,                             % Shutdown - wait 4 seconds, to
                                                    %            give child processes time
                                                    %            to be killed off.
      type     => worker,                           % Type     = worker | supervisor
      modules  => [payments_engine]}                % Modules  = [Module] | dynamic
  ],
  RestartStrategy = {one_for_one, 4, 3600},
  {ok, {RestartStrategy, Procs}}.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

check_tables() ->
  check_schema(),
  mnesia:start(),
  create_table(client, [
                  {attributes,  record_info(fields, client)}
                , {disc_copies, [node()]}
              ]),
  create_table(transaction, [
                  {attributes,  record_info(fields, transaction)}
                , {disc_copies, [node()]}
              ]),
  create_table(audit, [
                  {attributes,  record_info(fields, audit)}
                , {disc_copies, [node()]}
                , {type, ordered_set}  % Order records by key {ClientID, TransTime}
              ]),
  ok.

check_schema() ->
  % Make sure that mnesia schema is disk-based.
  Dir = application:get_env(mnesia, dir, []),
  case {mnesia:system_info(use_dir), Dir /= []} of
    {true, true} ->
      ok;
    {false, false} ->
      throw("Missing -mnesia dir '\"/path/to/db\"' option!");
    {false, true} ->
      ok == filelib:ensure_dir(Dir)
        orelse throw("Cannot create directory " ++ Dir),
			yes == mnesia_lib:is_running() andalso mnesia:stop(),
      ok   = mnesia:create_schema([node()]),
      ?LOG_INFO("Created disk schema\n"),
      ok   = mnesia:start()
	end.

create_table(Tab, Attrs) ->
  try
    mnesia:table_info(Tab, record_name),
    wait_for_table(Tab)
  catch _:{aborted,{no_exists,Tab,_}} ->
    case mnesia:create_table(Tab, Attrs) of
      {atomic, ok} ->
        ?LOG_NOTICE("Table ~w created", [Tab]);
      {aborted, Why} when element(1, Why) =:= already_exists ->
        ok;
      {aborted, Why} ->
        throw({error_creating_table, Tab, Why})
    end
  end.

wait_for_table(Tab) ->
  case mnesia:wait_for_tables([Tab], 60000) of
    ok ->
      ok;
    {timeout, [Tab]} ->
      ?LOG_WARNING("Timeout waiting for table ~w", [Tab]),
      wait_for_table(Tab);
    {error, Reason} ->
      throw(?FMT("Error waiting for table ~w: ~p", [Tab, Reason]))
  end.
