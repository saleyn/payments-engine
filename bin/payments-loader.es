#!/usr/bin/escript
%% vim:ts=2:sw=2:et
%%! -sname loader
%%%-----------------------------------------------------------------------------
%%% Script for loading transactions to the payments engine
%%%-----------------------------------------------------------------------------

-mode(compile).

-record(args, {
  pretty   = false,
  reccount = false,
  audit,
  file     = []
}).

%%%-----------------------------------------------------------------------------
%%% Main entry point
%%%-----------------------------------------------------------------------------
main([]) ->
  usage();
main(Args) ->
  try
    %% (1) Parse arguments

    ParsedArgs = parse(Args, #args{}),

    %% (2) Validate arguments and run the action

    action(ParsedArgs)
  catch
    _:{nostack, Err} ->
      io:format(standard_error, "Error: ~s\n", [Err]);
    _:Err:S ->
      io:format(standard_error, "Error: ~p\n  ~p\n", [Err, S])
  end.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

action(#args{audit = Audit}) when is_integer(Audit); Audit == all ->
  Node = configure(),
  print_audit(Node, Audit);

action(#args{pretty=PrettyPrint, reccount=ShowCount, file=File}) ->
  File == []
    andalso throw("Missing required CSVFile argument!"),
  filelib:is_regular(File)
    orelse throw({nostack, "File " ++ File ++ " not found!"}),

  Node = configure(),
  process_file(Node, File, PrettyPrint, ShowCount).

configure() ->
  %% Connect to the local payments engine
  {ok, Host} = inet:gethostname(),
  Node = list_to_atom("payments@" ++ Host),

  net_kernel:connect_node(Node)
    orelse throw({nostack, "Payments engine not running!"}),

  %% Add local path to the payments module
  add_paths(),
  Node.

print_audit(Node, Audit) ->
  F = fun
        G({Recs, '$end_of_table'}, PrintHeader) ->
          payments:print_audit(Recs, PrintHeader);
        G({Recs, Cont}, PrintHeader) ->
          payments:print_audit(Recs, PrintHeader),
          G(erpc:call(Node, payments_engine, audit, [Cont]), false)
      end,
  F(erpc:call(Node, payments_engine, audit, [Audit, []]), true).

process_file(Node, AbsFile, PrettyPrint, ShowCount) ->
  %% Process the transaction file
  case erpc:call(Node, payments, process_file, [AbsFile]) of
    {ok, Success, 0} ->
      ShowCount andalso
        io:format("Loaded: ~w records\n\n", [Success]);
    {ok, Success, Failed} ->
      ShowCount andalso
        io:format("Loaded: ~w records\n"
                  "Failed: ~w records\n\n", [Success, Failed]);
    Other ->
      io:format("Error: ~p\n", [Other]),
      erlang:halt(1)
  end,

  %% Print results
  case erpc:call(Node, payments, assets, [[{compact, true}]]) of
    Assets when is_list(Assets) ->
      payments:print_assets(Assets, [{compact, not PrettyPrint}]);
    Error ->
      io:format("Error: ~p\n", [Error])
  end.

usage() ->
  io:format(standard_error,
            "Load payment transactions from a file\n"
            "Usage: ~s [-p] [-n] [-h|--help] CSVFile\n\n"
            "Options:\n"
            "  -p         - pretty print\n"
            "  -n         - show number of records loaded/failed\n"
            "  -a [CliID] - print audit log (optionally filter by ClientID)\n"
            "  -h|--help  - print help\n\n",
            [escript:script_name()]),
  erlang:halt(1).

parse(["-p"     | T], A)                   -> parse(T, A#args{pretty   = true});
parse(["-n"     | T], A)                   -> parse(T, A#args{reccount = true});
parse([F        | T], A) when hd(F) /= $-  -> parse(T, A#args{file     = filename:absname(F)});
parse(["-a", ID | T], A) when hd(ID)/= $-  -> parse(T, A#args{audit    = list_to_integer(ID)});
parse(["-a"     | T], A)                   -> parse(T, A#args{audit    = all });
parse(["-h"     | _], _)                   -> usage();
parse(["--help" | _], _)                   -> usage();
parse([Other    | _], _)                   -> throw("Invalid argument: " ++ Other);
parse([],             A)                   -> A.

add_paths() ->
  code:add_paths(filelib:wildcard(base_dir() ++ "/*/ebin")).

base_dir() ->
  filename:dirname(filename:dirname(filename:absname(escript:script_name()))) ++ "/lib".
