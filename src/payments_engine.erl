%%% vim:ts=2:sw=2:et
%%%----------------------------------------------------------------------------
%%% @doc     Load payment transactions to the database
%%% @author  Serge Aleynikov <saleyn(at)gmail.com>
%%% @end
%%%----------------------------------------------------------------------------
%%% Created: 2021-08-10
%%%----------------------------------------------------------------------------
-module(payments_engine).

-behaviour(gen_server).

%% API
-export([start_link/0, process/1, assets/0, assets/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-record(state, {}).

-include("payments.hrl").

%%%----------------------------------------------------------------------------
%%% External API
%%%----------------------------------------------------------------------------

%%-----------------------------------------------------------------------------
%% @doc To be called by the supervisor in order to start the server.
%% @end
%%-----------------------------------------------------------------------------
-spec start_link() -> {ok, pid()} | ignore | {error, any()}.
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Process a list of transactions
-spec process([#transaction{}]) -> {ok, Count::integer()}.
process([]) ->
  {ok, 0};
process([#transaction{}|_] = Transactions) ->
  gen_server:call(?MODULE, {process, Transactions}, infinity).

%% @doc Get a list of all client assets
-spec assets() -> [#client{}].
assets() ->
  mnesia:dirty_match_object(mnesia:table_info(client, wild_pattern)).

%% @doc Get a list of client assets
-spec assets(integer()) -> #client{} | not_found.
assets(ClientID) when is_integer(ClientID) ->
  case mnesia:dirty_read(client, ClientID) of
    []  -> not_found;
    [R] -> R
  end.

%%%----------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------------

%%-----------------------------------------------------------------------------
%% @private
%% @doc Initiates the server
%% @end
%%-----------------------------------------------------------------------------
-spec init(Options::list()) -> {ok, #state{}}.
init(_Options) ->
  {ok, #state{}}.

%%-----------------------------------------------------------------------------
%% @private
%% @doc Handling call messages
%% @end
%%-----------------------------------------------------------------------------
-spec handle_call(any(), From::tuple(), #state{}) ->
  {reply, Reply::any(), #state{}} |
  {stop, Reason::any(), #state{}}.
handle_call({process, Transactions}, _From, State) ->
  Count = do_process(Transactions),
  {reply, {ok, Count}, State};

handle_call(Request, _From, State) ->
  {stop, {unknown_call, Request}, State}.

%%-----------------------------------------------------------------------------
%% @private
%% @doc Handling cast messages
%% @end
%%-----------------------------------------------------------------------------
-spec handle_cast(any(), #state{}) ->
  {noreply, #state{}} | {stop, Reason::any(), #state{}}.
handle_cast(Msg, State) ->
  {stop, {unknown_cast, Msg}, State}.

%%-----------------------------------------------------------------------------
%% @private
%% @doc Handling all non call/cast messages
%% @end
%%-----------------------------------------------------------------------------
-spec handle_info(any(), #state{}) ->
  {noreply, #state{}} | {stop, Reason::any(), #state{}}.
handle_info(_Info, State) ->
  {noreply, State}.

%%-----------------------------------------------------------------------------
%% @private
%% @doc This function is called by a gen_server when it is about to terminate.
%% @end
%%-----------------------------------------------------------------------------
-spec terminate(any(), #state{}) -> ok.
terminate(_Reason, #state{}) ->
  ok.

%%-----------------------------------------------------------------------------
%% @private
%% @doc Convert process state when code is changed
%% @end
%%-----------------------------------------------------------------------------
-spec code_change(any(), #state{}, any()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%----------------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------------

%%-----------------------------------------------------------------------------
%% @doc Process a list of transactions, and return the number of successes
%% @end
%%-----------------------------------------------------------------------------
-spec do_process([#transaction{}]) -> WrittenTransCount::integer().
do_process(Transactions) ->
  TS=tm(),
  lists:foldl(
    fun(Trans, Acc) ->
      try do_transaction(tm(Trans, TS)) of
        {atomic, true} ->
          Acc+1;
        {atomic, false} ->
          Acc;  %% Duplicate transaction
        {aborted, Reason} ->
          ?LOG_ERROR(#{trans     => Trans,
                       error     => "Error processing transaction",
                       reason    => Reason}),
          Acc
      catch
        _:Err:Stack ->
          ?LOG_ERROR(#{trans     => Trans,
                       exception => Err,
                       stack     => Stack}),
          Acc
      end
    end,
    0,
    Transactions).

%%-----------------------------------------------------------------------------
%% @doc The core processor of transactions
%% @end
%%-----------------------------------------------------------------------------
do_transaction(#transaction{id=ID, type=Type, client_id=CliID, amount=Amt, created=TS} = T) ->
  mnesia:transaction(fun() ->
    %% Check for duplicates and for valid transactions
    {Res, OldCliRec, NewCliRec} =
      case mnesia:dirty_read(transaction, ID) of
        [_] when Type==deposit; Type==withdrawal ->
          %% This transaction was already processed, skip it
          {false, undefined, undefined};

        [] when Type==deposit; Type==withdrawal ->
          %% Apply the transaction to the client's asset record
          ok  = mnesia:write(T),                  %% Save this transaction
          add_balance(CliID, Amt*sign(Type), TS); %% Update the client's balance

        [] when Type==dispute; Type==chargeback; Type==resolve ->
          %% The transaction is not found, this is an error on the partner's side,
          %% which we should ignore
          ?LOG_WARNING("Disputed transaction #~w not found", [ID]),
          {false, undefined, undefined};

        [#transaction{client_id=N}] when N =/= CliID ->
          %% The transaction ID references a client ID which is different from the
          %% ClientID previously recorded, this is an error on the partner's side.
          %% FIXME: ignore this error?
          ?LOG_ERROR("Transaction #~w (~w) references the wrong client #~w (found #~w)",
                     [ID, Type, N, CliID]),
          {false, undefined, undefined};

        [_] when Amt =/= undefined; is_float(Amt), Amt =/= 0.0 ->
          ?LOG_WARNING("~w transaction shouldn't define an amount (found ~p)!", [Type, Amt]),
          {false, undefined, undefined};

        [#transaction{voided=Voided}] when Voided =/= 0 ->
          %% This transaction was previously disputed/charged back and cannot be disputed again
          {false, undefined, undefined};

        [#transaction{}=OldT] ->
          case Type of
            dispute    -> add_dispute   (OldT, TS);   %% Process dispute
            chargeback -> add_chargeback(OldT, TS);   %% Process chargeback
            resolve    -> add_resolve   (OldT, TS)    %% Process resolve
          end
      end,
    %% Add an audit log record
    store_audit(Res, OldCliRec, NewCliRec, {ID, Type, Amt, TS})
  end).

sign(deposit)    ->  1.0;
sign(withdrawal) -> -1.0.

%%-----------------------------------------------------------------------------
%% @doc Get (or create) a client asset identified by `CliID'.
%% @end
%%-----------------------------------------------------------------------------
-spec get_client(CliID::integer(), Timestamp::integer()) -> #client{}.
get_client(CliID, TS) ->
  case mnesia:read(client, CliID) of
    [] ->
      #client{id=CliID, created=TS};
    [R] ->
      R
  end.

%%-----------------------------------------------------------------------------
%% @doc Update client's balance
%% @end
%%-----------------------------------------------------------------------------
add_balance(CliID, Amount, TS) ->
  %% Get current client's balances
  C  = #client {balance_avail=BA, balance_held=BH} = get_client(CliID, TS),
  NA = BA+Amount,
  R  = C#client{balance_avail=NA, balance_total=BH+NA, updated=TS},
  ok = mnesia:write(R),
  {true, C, R}.

%%-----------------------------------------------------------------------------
%% @doc Dispute the given `Amount' (no change to total balance).
%% NOTE: only deposits can be disputed, and also don't process
%%       duplicate disputes.
%% @end
%%-----------------------------------------------------------------------------
add_dispute(#transaction{disputed=I, type=Type, voided=V}, _TS) when I /= 0; Type /= deposit; V /= 0 ->
  %% The transaction is already disputed; is not a deposit; or has been voided!
  {false, undefined, undefined};
add_dispute(#transaction{client_id=CliID, amount=Amount}=T, TS) ->
  C  = #client {balance_avail = BA,
                balance_held  = BH} = get_client(CliID, TS),
  R  = C#client{balance_avail = BA-Amount,
                balance_held  = BH+Amount,
                updated       = TS},
  %% Save the client asset record
  ok = mnesia:write(R),
  %% Update the transaction with indication that it was disputed
  ok = mnesia:write(T#transaction{disputed = TS,
                                  updated  = TS}),
  {true, C, R}.

%%-----------------------------------------------------------------------------
%% @doc Chargeback the given `Amount'.
%%      The transaction must be previously disputed!
%%      When this happens the account must be immediately locked.
%% @end
%%-----------------------------------------------------------------------------
add_chargeback(#transaction{disputed=I, voided=V}, _TS) when I==0; V > 0 ->
  %% A non-disputed or voided transaction cannot be charged back!
  {false, undefined, undefined};
add_chargeback(#transaction{client_id=CliID, amount=Amount}=T, TS) ->
  C  = #client {balance_avail = BA, balance_held = BH} = get_client(CliID, TS),
  NB = BH-Amount,
  R  = C#client{balance_held  = NB,
                balance_total = NB+BA,
                locked        = true,
                updated       = TS},
  %% Update the client asset record
  ok = mnesia:write(R),
  %% Update the transaction with indication that it is no longer disputed
  ok = mnesia:write(T#transaction{disputed = 0,
                                  voided   = TS,
                                  updated  = TS}),
  {true, C, R}.

%%-----------------------------------------------------------------------------
%% @doc Resolve the given `Amount' by releasing the held funds.
%%      The transaction must be previously disputed!
%% @end
%%-----------------------------------------------------------------------------
add_resolve(#transaction{disputed=I, voided=V}, _TS) when I==0; V > 0 ->
  %% A non-disputed or voided transaction cannot be resolved!
  {false, undefined, undefined};
add_resolve(#transaction{client_id=CliID, amount=Amount}=T, TS) ->
  C  = #client {balance_avail = BA, balance_held = BH} = get_client(CliID, TS),
  R  = C#client{balance_avail = BA+Amount,
                balance_held  = BH-Amount,
                updated       = TS},
  %% Update the client asset record
  ok = mnesia:write(R),
  %% Update the transaction with indication that it is no longer disputed
  ok = mnesia:write(T#transaction{disputed = 0,
                                  voided   = 0,
                                  updated  = TS}),
  {true, C, R}.

%%-----------------------------------------------------------------------------
%% @doc Save an audit record
%% @end
%%-----------------------------------------------------------------------------
store_audit(false, _, _, _) ->
  false;
store_audit(true, #client{balance_avail=OldA, balance_held=OldH},
                  #client{balance_avail=NewA, balance_held=NewH, id=CliID, locked=Locked},
            {TransID, TransType, Amount, TransTS}) ->
  ok = mnesia:dirty_write(
         #audit{key        = {CliID, TransTS},
                id         = TransID,
                type       = TransType,
                amount     = Amount,
                old_avail  = OldA,
                new_avail  = NewA,
                old_held   = OldH,
                new_held   = NewH,
                lock       = Locked,
                created    = tm()}),
  true.

tm() ->
  erlang:system_time(microsecond).

%% If the transaction has no creation timestamp, update it
tm(T=#transaction{created=C}, TS) when C==undefined; C=:=0 ->
  T#transaction{created=TS};
tm(T, _) ->
  T.

%%%----------------------------------------------------------------------------
%%% Test cases
%%%----------------------------------------------------------------------------
