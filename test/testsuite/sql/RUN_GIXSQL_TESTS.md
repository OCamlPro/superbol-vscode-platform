# How to run GixSQL testsuite :

Note: This file can mainly be ignored as the testsuite of gixsql has been automated
in https://github.com/OCamlPro/gixsql

The failing tests are described in more detailed in [this issue](https://github.com/OCamlPro/superbol-studio-oss/issues/371).

## Install GixSQL

Following https://github.com/mridoni/gixsql/tree/main?tab=readme-ov-file#linux you can build GixSQL locally
Or with the PR https://github.com/OCamlPro/gixsql/pull/2.

## Configure at least one database type

This requires creating one user (e.g. `test`) with password access (e.g. `test`), and 2 databases (e.g. `testdb1` and `testdb2`).
Postgres with the provided user, password and database names will be used for the rest of the setup.

Note: mysql tests fail on Linux, it seems to be a case sensitivity issue between Windows and Linux.

From the doc https://dev.mysql.com/doc/refman/8.0/en/identifier-case-sensitivity.html
> In MySQL, databases correspond to directories within the data directory. Each table within a database corresponds to at least one file within the database directory (and possibly more, depending on the storage engine). Triggers also correspond to files. Consequently, the case sensitivity of the underlying operating system plays a part in the case sensitivity of database, table, and trigger names. This means such names are not case-sensitive in Windows, but are case-sensitive in most varieties of Unix.

## Modifying gixsql testsuite

The following PR https://github.com/OCamlPro/gixsql/pull/4 contains modification to the test runner source code that allows executing the testsuite with `superbol sql preproc` instead of `gixpp`.
The superbol-free executable needs to be accessible at `/opt/superbol-free-linux-x64` (symbolic linking works)

Configuring and running the testsuite can be done in 1 command using the PR https://github.com/OCamlPro/gixsql/pull/2.
Or manually by following the next two section

## GixSQL XML config file

The fields in `gixsql_test_local_linux.xml` are described in https://github.com/mridoni/gixsql/blob/main/TESTING.md.

Make sure to check the following :
- set `gixsql-install-base` : probably `/opt/gixsql` if you followed the default for GixSQL install
- clear `test-filter` : allow executing all test files
- set your configured databases in `dbtype-filter` : `pgsql` if you want only Postgres
- remove the `mem-check` field
- depending on you installation of GnuCOBOL change the `compiler` fields
    - `bin_dir_path` can be `/usr/local/bin`
    - `lib_dir_path` can be `/usr/local/lib`
    - `config_dir_path` can be `/usr/local/share/gnucobol/config`
- in `data-sources` configure the 2 databases required for each database type

## Building & executing the testsuite

Building the testsuite is described here : https://github.com/mridoni/gixsql/blob/main/TESTING.md#building-the-test-runner.
Things to consider :
- This require dotnet 6.0 SDK
- The command `dotnet build gixsql-tests-nunit/gixsql-tests-nunit.csproj` is slow and will block at `Determining projects to restore...` for some time (5 minutes).

Before executing the testsuite, the `GIXTEST_LOCAL_CONFIG` env variable needs to exists and be set to full path of your `gixsql_test_local_linux.xml` file.
Executing the testsuite with `dotnet gixsql-tests-nunit/bin/Debug/net6.0/gixsql-tests-nunit.dll` should work and execute the test either with GixSQL preprocessor or SuperBOL preprocessor if the patch file was applied

### Additionnal considerations for gixpp tests

At the moment of writing, and with my config (Postgres on linux), the following test seemed to fail even when executed with `gixpp` :

```
Failed tests:
(#000) - TSQL004A/x64/gcc/pgsql - CURSOR + misc data types                 : KO
(#000) - TSQL005D/x64/gcc/pgsql - BINARY/VARBINARY data types              : KO
(#091) - TSQL030A/x64/gcc/pgsql - Show-stopper bug in pgsql_prepare        : KO
Run: 71 - Success: 68 - Failed: 3
```

The test `TSQL004A` fails due to a invalid preprocessor option. (line 237 of `gixsql_test_data.xml`)
The other two fail due to preprocessor errors.

