%%==============================================================================
%% Data Schema
%%==============================================================================

-include_lib("kernel/include/logger.hrl").

-define(APP,      payments).
-define(FMT(F,A), lists:flatten(io_lib:format(F, A))).

%% Client asset account
-record(client, {
  id                    :: integer(),     %% Client ID (PK)
  balance_avail = 0.0   :: float(),       %% Available balance
  balance_held  = 0.0   :: float(),       %% Disputed/held balance
  balance_total = 0.0   :: float(),       %% Total balance (available + held)
  locked        = false :: boolean(),     %% Lock status of the account (true: it has chargebacks)
  created               :: integer(),     %% Timestamp when the record was created (usec from Epoch)
  updated     :: undefined|integer()      %% Timestamp when the record was updated (usec from Epoch)
}).

%% Client transaction
-record(transaction, {
  id                    :: integer(),     %% Transaction ID (PK)
  type                                    %% Transaction Type
                        :: deposit|withdrawal|dispute|chargeback|resolve,
  client_id             :: integer(),     %% Client ID (FK to the client table)
  amount      :: undefined|float(),       %% Amount
  disputed      = 0     :: integer(),     %% Timestamp when this transaction was disputed
  voided        = 0     :: integer(),     %% Timestamp when transaction is voided (disputed and resolved/charged-back)
  created               :: integer(),     %% Timestamp when transaction was created (usec from Epoch)
  updated     :: undefined|integer()      %% Timestamp when the record was updated (usec from Epoch)
}).

%% Client transaction audit record
-record(audit, {
  key :: {'_'|integer(), '$1'|integer()}, %% {ClientID, TransactionTimestamp}
  id                    :: '_'|integer(), %% Transaction ID (FK to the transaction table)
  type                                    %% Transaction Type
      :: '_'|deposit|withdrawal|dispute|chargeback|resolve,
  amount        = 0.0   :: '_'|float(),   %% Amount
  old_avail     = 0.0   :: '_'|float(),   %% Old available balance
  new_avail     = 0.0   :: '_'|float(),   %% New available balance
  old_held      = 0.0   :: '_'|float(),   %% Old held balance
  new_held      = 0.0   :: '_'|float(),   %% New held balance
  lock          = false :: '_'|boolean(), %% Account lock status
  created               :: '_'|integer()  %% Timestamp when transaction was inserted (usec from Epoch)
}).
