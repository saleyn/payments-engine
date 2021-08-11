-module(payments_test).

-include("payments.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([top_setup/0, top_cleanup/1]).

%%%-----------------------------------------------------------------------------
%%% Unit Tests
%%%-----------------------------------------------------------------------------

-ifdef(EUNIT).

-define(DIR, "/tmp/test/eunit").

top_setup() ->
  %% if not already started, start the application
  net_kernel:start([test@abc]),
  application:set_env(mnesia, dir, ?DIR),
  application:ensure_all_started(payments),
  logger:set_application_level(?APP, emergency),
  logger:set_handler_config(screen_log, level, emergency),
  ?debugMsg("Payments engine started").

top_cleanup(_) ->
  application:stop(payments),
  file:del_dir_r(?DIR),
  ?debugMsg("Payments engine stopped").

payments_test_() ->
  {setup,
    fun top_setup/0,
    fun top_cleanup/1,
    [?_assertEqual({ok, 5, 0}, payments:process_file("test/data/transactions.csv")),
     %% Running the same file again should load 0 records
     ?_assertEqual({ok, 0, 0}, payments:process_file("test/data/transactions.csv")),
     %% Load records with bad data
     ?_assertEqual({ok, 3, 2}, payments:process_file("test/data/bad-data.csv")),
     %% Test disputes
     ?_assertEqual({ok, 7, 2}, payments:process_file("test/data/resolve.csv")),

     %% Deposits/Withdrawals
     ?_assertEqual({ok, 1},
                  payments_engine:process(
                      [#transaction{id=100, type=deposit, amount=5.0, client_id=5, created=1000}])),
     ?_assertMatch(#client{id = 5,balance_avail = 5.0,balance_held = 0.0, balance_total = 5.0,
                           locked = false, created = 1000,updated = 1000},
                  payments:client_asset(5)),

     ?_assertEqual({ok, 1},
                  payments_engine:process(
                      [#transaction{id=101, type=deposit, amount=2.0, client_id=5, created=1001}])),
     ?_assertMatch(#client{id = 5,balance_avail = 7.0,balance_held = 0.0, balance_total = 7.0,
                           locked = false, created = 1000,updated = 1001},
                  payments:client_asset(5)),

     ?_assertEqual({ok, 1},
                    payments_engine:process(
                      [#transaction{id=102, type=withdrawal, amount=1.0, client_id=5, created=1002}])),
     ?_assertMatch(#client{id = 5,balance_avail = 6.0,balance_held = 0.0, balance_total = 6.0,
                           locked = false, created = 1000,updated = 1002},
                  payments:client_asset(5)),

     %% Bad input: chargeback/dispute/resolve has an amount
     ?_assertEqual({ok, 0},
                  payments_engine:process([#transaction{id=101, type=dispute,    client_id=5, amount=2.0}])),
     ?_assertEqual({ok, 0},
                  payments_engine:process([#transaction{id=101, type=chargeback, client_id=5, amount=2.0}])),
     ?_assertEqual({ok, 0},
                  payments_engine:process([#transaction{id=101, type=resolve,    client_id=5, amount=2.0}])),

     %% Bad input: chargeback/resolve a non-disputed transaction or a withdrawal
     ?_assertEqual({ok, 0},
                  payments_engine:process([#transaction{id=102, type=dispute,    client_id=5, created=1003}])),
     ?_assertEqual({ok, 0},
                  payments_engine:process([#transaction{id=101, type=chargeback, client_id=5, amount=undefined, created=1004}])),
     ?_assertEqual({ok, 0},
                  payments_engine:process([#transaction{id=101, type=resolve,    client_id=5, amount=undefined, created=1005}])),

     %% Successful dispute
     ?_assertEqual({ok, 1},
                  payments_engine:process([#transaction{id=101, type=dispute,    client_id=5, created=1003}])),
     ?_assertMatch(#client{id = 5,balance_avail = 4.0,balance_held = 2.0, balance_total = 6.0,
                          locked = false, created = 1000,updated = 1003},
                  payments:client_asset(5)),
     %% Duplicate dispute
     ?_assertEqual({ok, 0},
                  payments_engine:process([#transaction{id=101, type=dispute,    client_id=5, created=1003}])),

     %% Successful resolve/resolve
     ?_assertEqual({ok, 1},
                  payments_engine:process([#transaction{id=101, type=resolve,    client_id=5, created=1005}])),
     ?_assertMatch(#client{id = 5,balance_avail = 6.0,balance_held = 0.0, balance_total = 6.0,
                          locked = false, created = 1000,updated = 1005},
                  payments:client_asset(5)),
     ?_assertMatch([#transaction{id = 101, voided = 0}], mnesia:dirty_read(transaction, 101)),

     %% Successful dispute/chargeback
     ?_assertEqual({ok, 1},
                  payments_engine:process([#transaction{id=101, type=dispute,    client_id=5, created=1006}])),
     ?_assertMatch(#client{id = 5,balance_avail = 4.0,balance_held = 2.0, balance_total = 6.0,
                          locked = false, created = 1000,updated = 1006},
                  payments:client_asset(5)),
     ?_assertEqual({ok, 1},
                  payments_engine:process([#transaction{id=101, type=chargeback, client_id=5, created=1007}])),
     ?_assertMatch(#client{id = 5,balance_avail = 4.0,balance_held = 0.0, balance_total = 4.0,
                          locked = true, created = 1000,updated = 1007},
                  payments:client_asset(5)),
     ?_assertMatch([#transaction{id = 101, voided = 1007}], mnesia:dirty_read(transaction, 101)),
     ?_assertEqual({ok, 0},
                  payments_engine:process([#transaction{id=101, type=dispute,    client_id=5, created=1008}]))
    ]}.


-endif.
