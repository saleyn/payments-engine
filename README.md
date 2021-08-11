# Sample Payments Engine

[![build](https://github.com/saleyn/payments-engine/actions/workflows/erlang.yml/badge.svg)](https://github.com/saleyn/payments-engine/actions/workflows/erlang.yml)

A payments engine that reads in a series of transactions from a CSV, updates individual client
accounts, handles disputes, chargebacks and resolutions, and then outputs the state of client
accounts as a CSV.

The engine uses the `mnesia` database for storing transactions and client asset balances.
The engine consists of a daemon process used to abstract the database access, and guarantee
atomicity of updates.

A loading script `payments-loader.es` is provided for communication with the daemon, and
it needs to be run on the same host.

A daemon management script `payments` is provided for managing startup/daemonization/attaching
to the daemon process.

## Author

- Serge Aleynikov <saleyn(at)gmail.com>

## Installing

### Prerequisites:

- Tools: git, make
- [Erlang/OTP 24](https://www.erlang.org/downloads)
  Erlang is most likely available for installation as a package for your host OS.
  For Arch Linux use: `sudo pacman -S erlang`

- [Rebar3](https://github.com/erlang/rebar3)
  The system needs to have a local installation of `rebar3`:
```
# For Arch Linux:
$ sudo pacman -S rebar3

# For others:
$ git clone https://github.com/erlang/rebar3.git
$ cd rebar3
$ ./bootstrap
$ sudo cp ./rebar3 /usr/local/bin     # Or possibly some other location included in the PATH
```

### Dependencies

The project uses a `csv` and `stringx` modules in an external
[util library](https://github.com/saleyn/util.git) for CSV parsing and pretty printing.
They are automatically pulled and built by the project.

## Installing the project from a release

Obtain the latest release from [here](https://github.com/saleyn/payments-engine/releases).
Untar the file to the installation directory, and it's ready to run via the scripts in the
`bin` directory discussed in the following sections.

```
$ mkdir payments
$ cd payments
$ tar zxf payments-0.1.tar.gz
```

## Building the project from sources

The alternative to downloading a prebuilt release is to build it from sources.
The commands below will pull the project and build a release in the `install` directory:
```
$ git clone https://github.com/saleyn/payments-engine.git
$ make release
```

## Project structure

```
.
├── _build                              # Build artifacts (generated)
├── bin                                 # Source directory for scripts
├── etc
│   ├── sys.config                      # System configuration
│   └── vm.args                         # Erlang VM arguments
├── doc                                 # Documentation
├── include
│   └── payments.hrl                    # Definition of table headers
├── Makefile
├── README.md
├── rebar.config                        # Build configuration
├── rebar.lock                          # Dependency versions lock
├── install                             # Installation root folder (generated)
│   ├── bin
│   │   ├── payments-loader.es          # Payment transactions loading script
│   │   └── payments                    # System startup / management script
│   ├── lib
│   │   ├── payments-0.1.0              # Bytecode of the payments engine
│   │   └── util-1.0.0                  # Bytecode of the dependency library
│   └── releases                        # Release versions
├── src
│   ├── payments.erl                    # External API source code
│   ├── payments_app.erl                # Application startup logic
│   ├── payments.app.src                # Application metadata
│   └── payments_engine.erl             # Payments core engine
└── test
    └── data
        └── transactions.csv            # Test data
```

## Running tests

A GitHub workflow action is setup to automatically run tests on version pushes.

The tests can be run locally by hand:
```
$ make test
```

This runs a dialyzer on the source code as well as executes test cases to verify
that all transaction types are properly handled.

## Running the engine

The engine can be started either in the interactive `console` mode or in the background
`daemon` mode.

The location of the engine's database and log files can be customized by setting the
`APP_ROOT` environment variable before running the `payments` daemon.

By default those artifacts go to `/tmp/test` directory:
```
$ tree /tmp/test/
/tmp/test/
├── log
│   ├── erl-debug.log                   # Debug log
│   └── erl-error.log                   # Error log
└── var
    └── db                              # Database directory for tables and logs
```

For development, two convenience shortcuts are provided in the `Makefile` to
start the engine's in daemon or interactive mode:
```
$ make start-daemon   # Starts a daemon
$ make start          # Starts the engine in interactive mode
```

Starting in console mode (just to ensure that everything starts correctly) can be
accomplished by running `make start`.

On the first startup the engine will bootstrap the database and create the required tables.
We exit the engine by executing `q().`:

```
$ make start
Exec: /usr/lib/erlang/erts-12.0/bin/erlexec -boot /home/serge/tmp/payments-engine/install/releases/0.1/start -mode embedded -boot_var SYSTEM_LIB_DIR /usr/lib/erlang/lib -config /home/serge/tmp/payments-engine/install/releases/0.1/sys.config -args_file /home/serge/tmp/payments-engine/install/releases/0.1/vm.args -- console
Root: /home/serge/tmp/payments-engine/install
/home/serge/tmp/payments-engine/install
Erlang/OTP 24 [erts-12.0] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:1] [jit]

=NOTICE REPORT==== 10-Aug-2021::19:56:23.247180 ===
Table client created
=NOTICE REPORT==== 10-Aug-2021::19:56:23.248460 ===
Table transaction created
=NOTICE REPORT==== 10-Aug-2021::19:56:23.249709 ===
Table audit created
Eshell V12.0  (abort with ^G)
(payments@zeos)1> q().
```

From here, we can start the engine as a daemon:
```
$ APP_ROOT=/tmp/test install/bin/payments start

# Verify that it's up:

$ install/bin/payments ping
pong
```

Loading transactions from a file can be done either in the interactive mode by calling
`payments:process_file/1`, or via a loader script.  For the later, execute the script
by giving it the file with transactions:
```
$ cat test/data/transactions.csv
type,  client, tx, amount
deposit,    1,  1, 1.0
deposit,    2,  2, 2.0
deposit,    1,  3, 2.0
withdrawal, 1,  4, 1.5
withdrawal, 2,  5, 3.0

$ install/bin/payments-loader.es test/data/transactions.csv
client,available,held,total,locked
1,1.5000,0.0000,1.5000,false
2,-1.0000,0.0000,-1.0000,false
```

Note that the script takes two optional arguments for pretty printing and outputing
the record counts:
```
$ install/bin/payments-loader.es -h
Load payment transactions from a file
Usage: install/bin/payments-loader.es [-p] [-n] [-h|--help] CSVFile

Options:
  -p         - pretty print
  -n         - show number of records loaded/failed
  -a [CliID] - print audit log (optionally filter by ClientID)
  -h|--help  - print help

$ install/bin/payments-loader.es -p -n test/data/transactions.csv
Result:  0 records loaded

Client | Available |  Held  |  Total  | Locked
-------+-----------+--------+---------+-------
     1 |    1.5000 | 0.0000 |  1.5000 | false
     2 |   -1.0000 | 0.0000 | -1.0000 | false
-------+-----------+--------+---------+-------
```

Note that in this second execution `0` records were loaded because
all of them are duplicates.

The system maintains an audit trail log of all client asset changes.
To view that log, execute the `payments-loader.es` with `-a` option:
```
$ install/bin/payments-loader.es -a 5
               Time |  ClientID |   TransID |       Type |       Amount |     OldAvail->NewAvail     |      OldHeld->NewHeld      |     NewTotal |  Lock
2021-08-10 19:00:00 |         5 |       100 |    deposit |       5.0000 |       0.0000->5.0000       |       0.0000->0.0000       |       5.0000 | false
```

## Assumptions

- The client has a single asset account. All transactions are to and from this single
  asset account
- There are multiple clients. Transactions reference clients. If a client does not exist a
  new record is created
- Clients are represented by integers. No names, addresses, or complex client profile info

- A file can be partially loaded, any bad records in the input file will be logged with the
  line number that is rejected and the reason
- If the input contains withdrawals that lead to client's negative balance, the balance
  will become negative (i.e. the transaction input file is "the source of truth")
- After running the loader script all client assets will be printed rather than just those
  that were modified by transactions
- Only deposits can be disputed/charged-back/resolved
- A charged-back transaction cannot be disputed again
- All records in a batch of transactions would have the same creation timestamp
