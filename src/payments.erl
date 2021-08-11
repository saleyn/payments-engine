%%% vim:ts=2:sw=2:et
%%%-----------------------------------------------------------------------------
%%% @doc     Payments engine API for loading transaction files
%%%
%%% @author  Serge Aleynikov <saleyn(at)gmail.com>
%%% @end
%%%-----------------------------------------------------------------------------
%%% Created: 2021-08-10
%%%-----------------------------------------------------------------------------
-module(payments).

%% External API
-export([process_file/1, assets/0, assets/1, client_asset/1]).
-export([print_assets/0, print_assets/1, print_assets/2]).

%% Internal API
-export([parse_file/1]).

-include("payments.hrl").

%%%-----------------------------------------------------------------------------
%%% External API
%%%-----------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @doc Load transactions from file, and return the number of
%%      successfully processed and failed records.
%%      Rejected records could be due to data formatting issues.
%% @end
%%------------------------------------------------------------------------------
-spec process_file(binary()|string()) ->
  {ok, GoodCount::integer(), BadRecCount::integer()} | {error, string()}.
process_file(Filename) when is_binary(Filename) ->
  process_file(binary_to_list(Filename));

process_file(Filename) when is_list(Filename) ->
  try
    %% (1) Parse the file
    {GoodTrans, BadRows} = parse_file(Filename),

    %% (2) Log bad rows
    [?LOG_WARNING("Record #~w: ~s", [I, S]) || {I, S} <- BadRows],

    %% (3) Process good transactions
    {ok, GoodCount} = payments_engine:process(GoodTrans),

    %% (4) Return the number of successfully and unsuccessfully processed records
    {ok, GoodCount, length(BadRows)}
  catch _:Err:Stack ->
    ?LOG_ERROR(#{error => Err, stack => Stack}),
    {error, Err}
  end.

%%------------------------------------------------------------------------------
%% @doc Get a list of all client asset records
%% @end
%%------------------------------------------------------------------------------
-spec assets() -> [#client{}].
assets() ->
  assets([]).

%%------------------------------------------------------------------------------
%% @doc Get a list of all client assets as `#client{}` records or tuples.
%%      When `[{compact, Bool}]' option is provided, the output is reduced to
%%      tuples of arity 5.
%% @end
%%------------------------------------------------------------------------------
-spec assets([{compact,boolean()}]) ->
        [#client{}] |
        [{TransID::integer(), Avail::float(), Held::float(), Total::float(), Lock::boolean()}].
assets([]) ->
  payments_engine:assets();
assets(Options) when is_list(Options) ->
  Data = payments_engine:assets(),
  case proplists:get_value(compact, Options, true) of
    true ->
      [{ID, A, H, T, Lock}
        || #client{id=ID, balance_avail=A, balance_held=H,
                   balance_total=T, locked=Lock} <- Data];
    false ->
      assets([])
  end.

%%------------------------------------------------------------------------------
%% @doc Get a client asset
%% @end
%%------------------------------------------------------------------------------
-spec client_asset(integer()) -> #client{} | not_found.
client_asset(ClientID) when is_integer(ClientID) ->
  payments_engine:assets(ClientID).

%%------------------------------------------------------------------------------
%% @doc Print assets to stdout
%% @end
%%------------------------------------------------------------------------------
print_assets() ->
  print_assets([{compact, true}]).

%%------------------------------------------------------------------------------
%% @doc Print assets to stdout with given options (compact or pretty printing).
%% @end
%%------------------------------------------------------------------------------
print_assets(Options) when is_list(Options) ->
  print_table(assets(), Options).

%%------------------------------------------------------------------------------
%% @doc Print assets of one client to stdout with given options.
%% @end
%%------------------------------------------------------------------------------
-spec print_assets(integer() |
                   [{TransID::integer(), Avail::float(), Held::float(),
                     Total::float(), Lock::boolean()}],
                   [{compact,boolean()}]) -> ok.
print_assets(ClientID, Options) when is_integer(ClientID), is_list(Options) ->
  case client_asset(ClientID) of
    #client{} = R -> print_table([R], Options);
    not_found     -> ok
  end;
print_assets(Records, Options) when is_list(Records), is_list(Options) ->
  print_table(Records, Options).

%%------------------------------------------------------------------------------
%% @doc Parse a CSV input file with transactions
%% @end
%%------------------------------------------------------------------------------
-spec parse_file(string()) ->
        {Transactions::[#transaction{}],
         BadRows::[{RowNum::integer(), Reason::string()}]}.
parse_file(Filename) when is_list(Filename) ->
  parse_file(Filename, []).

-spec parse_file(string(), [{current_time, fun(() -> integer())}]) ->
        {list(), [{integer(), string()}]}.
parse_file(Filename, Options) when is_list(Filename) ->
  %% (1) Verify the filename's extension
  ".csv" == filename:extension(Filename)
    orelse throw(Filename ++ " must have a .csv extension!"),

  %% (2) Parse the input file in CSV format. `fix_length' will ensure that
  %% the rows with fewer than 4 columns will have missing ones filled-in
  %% with zero length strings.
  case csv:parse(Filename, [fix_length]) of
    [[<<"type">>, <<"client">>, <<"tx">>, <<"amount">>] | Rows] ->
      %% The column headers seem to be good.

      Now     = tm(),
      TimeFun = proplists:get_value(current_time, Options, fun() -> Now end),

      %% (3) Convert string data to know column types,
      %% separating good and bad (i.e. mulformed rows):
      {_GoodTrans, _BadRows} = convert_data(Rows, [], [], 1, TimeFun);
    [] ->
      throw("Given filename " ++ Filename ++ " has 0 records!");
    [Other|_] ->
      io:format("~p\n", [Other]),
      throw("Invalid format of " ++ Filename ++ ". Found columns: " ++
            binary_to_list(iolist_to_binary(Other)))
  end.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

-spec convert_data([[binary()]], [#transaction{}], [{integer(), string()}],
                   integer(), fun(() -> integer())) ->
        {[#transaction{}], [{integer(), string()}]}.
convert_data([], GoodAcc, BadAcc, _RowNum, _TimeFun) ->
  {lists:reverse(GoodAcc), lists:reverse(BadAcc)};
convert_data([Row|T], GoodAcc, BadAcc, RowNum, TimeFun) ->
  F = fun(Type, ClientID, TxID, Amt) ->
        case convert_row(Type, ClientID, TxID, Amt, RowNum, TimeFun) of
          {ok, TypedRow} ->
            {[TypedRow | GoodAcc], BadAcc};
          {error, Reason} ->
            {GoodAcc, [{RowNum, Reason}|BadAcc]}
        end
      end,

  case Row of
    [Type, ClientID, TxID] ->
      {NewGoodAcc, NewBadAcc} = F(Type, ClientID, TxID, undefined),
      convert_data(T, NewGoodAcc, NewBadAcc, RowNum+1, TimeFun);
    [Type, ClientID, TxID, Amount] ->
      {NewGoodAcc, NewBadAcc} = F(Type, ClientID, TxID, to_float(Amount)),
      convert_data(T, NewGoodAcc, NewBadAcc, RowNum+1, TimeFun);
    _Other ->
      convert_data(T, GoodAcc, [{RowNum, "Invalid data format"} | BadAcc], RowNum+1, TimeFun)
  end.

-spec convert_row(binary(), binary(), binary(), float()|undefined,
                  integer(), fun(() -> integer())) ->
        {ok, #transaction{}} | {error, string()}.
convert_row(Type, ClientID, TxID, Amount, RowNum, TimeFun) when is_integer(RowNum) ->
  try
    T = binary_to_existing_atom(Type),
    C = binary_to_integer(ClientID),
    I = binary_to_integer(TxID),

    % Sanity checks
    C > 0 orelse throw("Invalid ClientID"),
    I > 0 orelse throw("Invalid TxID"),

    Now = TimeFun(),

    if
      (T==deposit orelse T==withdrawal) andalso is_float(Amount) ->
        {ok, #transaction{id=I, type=T, client_id=C, amount=Amount, created=Now}};
      (T==dispute) andalso Amount==undefined ->
        {ok, #transaction{id=I, type=T, client_id=C, disputed=tm(), created=Now}};
      (T==chargeback orelse T==resolve) andalso Amount==undefined ->
        {ok, #transaction{id=I, type=T, client_id=C, created=Now}};
      T/=deposit, T/=withdrawal, T/=dispute, T/=chargeback, T/=resolve ->
        {error, "Invalid transaction type: " ++ str(T)};
      true ->
        {error, "Invalid amount: " ++ str(Amount)}
    end
  catch
    _:Why when is_list(Why) ->
      {error, ?FMT("~s (row=~w)", [Why, RowNum])};
    _:Why ->
      {error, ?FMT("Error: ~p (row=~w)", [Why, RowNum])}
  end.

tm() -> erlang:system_time(microsecond).

str(A) when is_atom(A)    -> atom_to_list(A);
str(A) when is_float(A)   -> float_to_list(A, [{decimals, 4}]).

to_float(B) ->
  try
    binary_to_float(B)
  catch _:_ ->
    float(binary_to_integer(B))
  end.

%% Compact printing of output rows
print_table(Data, Options) ->
  Compact = proplists:get_value(compact, Options, true),
  print_table2(Compact, Data).

print_table2(true, Data) ->
  [Headers|Rows] = format_rows(Data),
  io:format("~s\n", [string:to_lower(string:join(tuple_to_list(Headers), ","))]),
  [io:format("~w,~.4f,~.4f,~.4f,~w\n", [ID, A, H, T, L])
    || {ID, A, H, T, L} <- Rows],
  ok;

%% Pretty printing of output rows
print_table2(false, Data) ->
  [Headers|Rows] = format_rows(Data),
  FloatFmt       = fun(V) when is_float(V) -> {number, float_to_list(V, [{decimals, 4}])} end,
  io:format("~s\n", [
    stringx:pretty_table(
      Headers, Rows,
      #{td_formats => {
            undefined,
            FloatFmt,
            FloatFmt,
            FloatFmt,
            undefined
        }})]).

format_rows([]) ->
  [{"Client","Available","Held","Total","Locked"}];
format_rows(Data) ->
  Headers = format_rows([]),
  Headers ++
    lists:map(
      fun
        (#client{id=ID, balance_avail=A, balance_held=H,
                  balance_total=T, locked=Lock}) ->
          {ID, A, H, T, Lock};
        ({ID, _A, _H, _T, Lock}=R) when is_integer(ID), is_boolean(Lock) ->
          R
      end,
      Data).

%%%-----------------------------------------------------------------------------
%%% Tests
%%%-----------------------------------------------------------------------------
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

parse_file_test() ->
  Dir  = filename:dirname(filename:dirname(proplists:get_value(source, module_info(compile)))),
  File = filename:join(Dir, "test/data/transactions.csv"),
  ?assert(filelib:is_regular(File)),
  ?assertEqual({[{transaction,1,deposit,   1,1.0,0,0, 100000, undefined},
                 {transaction,2,deposit,   2,2.0,0,0, 100000, undefined},
                 {transaction,3,deposit,   1,2.0,0,0, 100000, undefined},
                 {transaction,4,withdrawal,1,1.5,0,0, 100000, undefined},
                 {transaction,5,withdrawal,2,3.0,0,0, 100000, undefined}],
                []},
               parse_file(File, [{current_time, fun() -> 100000 end}])).

-endif.
