
Sub-commands and Arguments
==========================
Common arguments to all sub-commands:


* :code:`-q` or :code:`--quiet`   Set verbosity level to 0

* :code:`-v` or :code:`--verbose`   Increase verbosity level

Overview of sub-commands::
  
  check syntax
    Check the syntax of a Cobol file
  
  ebcdic translate
    Convert to or from EBCDIC
  
  env
    Generate commands to set environment variables to use the specified switch
  
  indent file
    Indentation
  
  json vscode
    parse VSODE JSON files or generate package.json
  
  lsp
    run LSP server
  
  pp
    Preprocess a list of COBOL files, generating a preprocessed file with extension .i for each of them
  
  project config
    Print current project configuration in TOML format
  
  project init
    Project initialization
  
  snapshot
    Manage environment snapshots
  
  sql preproc
    Preprocess SQL EXECs
  
  switch
    Print current switch
  
  switch add
    Add a directory as a switch
  
  switch build
    Configure, build, install GnuCOBOL and add a switch
  
  switch config
    Change the current config
  
  switch env
    Generate commands to set environment variables to use the specified switch
  
  switch import
    Import existing GnuCOBOL installations as switches
  
  switch list
    List known switches
  
  switch set
    Set the current default switch
  
  texi2rst
    build .texi documentation from gnucobol-docs
  
  util detect cycle
    Detect a cycle of lines in a file
  
  util wc
    Stats on file of lines (without taking UTF8 chars into account)


main.exe check syntax
~~~~~~~~~~~~~~~~~~~~~~~

Check the syntax of a Cobol file



**DESCRIPTION**




**USAGE**
::
  
  main.exe check syntax FILE [OPTIONS]

Where options are:


* :code:`FILE`   Cobol file to check

* :code:`-D VAR=VAL`   Define a pre-processor variable VAR, with value VAL

* :code:`-I DIRECTORY`   Add DIRECTORY to library search path

* :code:`--conf CONF_FILE`   Set the configuration file to be used

* :code:`--dialect DIALECT` or :code:`--std DIALECT`   Set the dialect to bu used (overriden by `--conf` if used)

* :code:`--free`   Shorthand for `--source-format FREE`

* :code:`--recovery BOOL`   Enable/disable parser recovery after syntax errors (default: true)

* :code:`--silence STRING`   Silence specific messages

* :code:`--source-format SOURCE_FORMAT`   Set the format of source code; allowed values are: { FIXED (the default), FREE}
Overrides `format` from configuration file if present.


main.exe ebcdic translate
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Convert to or from EBCDIC



**DESCRIPTION**


This command translates to or from EBCDIC and ASCII.

**USAGE**
::
  
  main.exe ebcdic translate EBCDIC-FILE [OPTIONS]

Where options are:


* :code:`EBCDIC-FILE`   File in EBCDIC format (source or target)

* :code:`--colseq FILE`   Collating sequence file to read from GnuCOBOL

* :code:`--from-ascii ASCII-FILE`   File in ASCII format to translate

* :code:`--to-ascii ASCII-FILE`   File in ASCII format to generate


main.exe env
~~~~~~~~~~~~~~

Generate commands to set environment variables to use the specified switch



**DESCRIPTION**


This command generates a list of command to set environment variables for an existing switch.

It is usually used as:
::

  # eval $(superbol env)
  # cobc --version


The **--switch SWITCH** option can be used to choose a different switch from the default one. SWITCH should match a sub-string of a switch name. **--last** can be used to select the most recent switch.

The **--global** option can be used to generate a file **$HOME/.config/superbol/env** that can be loaded from shell init scripts, using a symlink to point PATH and LD_LIBRARY_PATH to the current switch at any time. Changing the current switch will then automatically redirect cobc to that new switch.


**ABOUT SWITCHES**


Switches are used to easily handle multiple GnuCOBOL installations. The section **[switches]** in the user configuration file **$HOME/.config/superbol/config.toml** contains several variables:

* * 'dir'
  The directory where installations should be performed, and switches imported from.

* * 'current'
  The current switch name to use by default

* * 'list'
  The list of known switches, with the corresponding installation directory

* * 'num'
  A counter used to name switches

Switches can be created, imported and used using the following commands:

* **superbol env [SWITCH] [--last] [--global]**
  Output a list of shell commands to set the environment variables to use a specific switch

* **superbol import [DIRS] [--clear] [--no-set]**
  Scan directories to detect GnuCOBOL installations, and created associated switches

* **superbol add DIR [--switch SWITCH] [--no-set]**
  Add a specific GnuCOBOL installation directory as a switch

* **superbol build [DIR] [--sudo] [--switch SWITCH] [--no-set]**
  From inside GnuCOBOL sources, configure, build and install and add the corresponding switch

* **superbol list**
  List known switches

* **superbol set [SWITCH] [--last]**
  Set the current default switch

**USAGE**
::
  
  main.exe env SWITCH [OPTIONS]

Where options are:


* :code:`SWITCH`   Switch to use, instead of current

* :code:`--global`   Use global configuration for default switch

* :code:`--last`   Use the latest imported switch


main.exe indent file
~~~~~~~~~~~~~~~~~~~~~~

Indentation



**DESCRIPTION**




**USAGE**
::
  
  main.exe indent file FILES [OPTIONS]

Where options are:


* :code:`FILES`   Cobol files to indent

* :code:`-D VAR=VAL`   Define a pre-processor variable VAR, with value VAL

* :code:`-I DIRECTORY`   Add DIRECTORY to library search path

* :code:`--conf CONF_FILE`   Set the configuration file to be used

* :code:`--dialect DIALECT` or :code:`--std DIALECT`   Set the dialect to bu used (overriden by `--conf` if used)

* :code:`--free`   Shorthand for `--source-format FREE`

* :code:`--gen-config`   Generate a config file .superbol-indent in this directory

* :code:`--inplace`   Modify files in place

* :code:`--intext`   For numeric, indentation size is relative to area A

* :code:`--lines LINE-LINE`   Indent only lines between these lines

* :code:`--numeric`   Output indentation size at the beginning of each line

* :code:`--recovery BOOL`   Enable/disable parser recovery after syntax errors (default: true)

* :code:`--silence STRING`   Silence specific messages

* :code:`--source-format SOURCE_FORMAT`   Set the format of source code; allowed values are: { FIXED (the default), FREE}
Overrides `format` from configuration file if present.

* :code:`--suffix EXT`   Set an extension for the file being generated


main.exe json vscode
~~~~~~~~~~~~~~~~~~~~~~

parse VSODE JSON files or generate package.json



**DESCRIPTION**




**USAGE**
::
  
  main.exe json vscode FILES [OPTIONS]

Where options are:


* :code:`FILES`   JSON Files to parse

* :code:`--gen FILE`   Generate FILE from current configuration

* :code:`--grammar`   Parse files as syntaxes/*.json files

* :code:`--language`   Parse files as language/configuration *.json files

* :code:`--manifest`   Parse files as package.json files

* :code:`--snippets`   Parse files as snippets/*.json files

* :code:`--tasks`   Parse files as .vscode/tasks.json files


main.exe lsp
~~~~~~~~~~~~~~

run LSP server



**DESCRIPTION**


Start a COBOL LSP server

**USAGE**
::
  
  main.exe lsp [OPTIONS]

Where options are:


* :code:`--caching`   Enable caching (enabled by default)

* :code:`--force-syntax-diagnostics`   Force reporting of syntax error and hint diagnostics for dialects other than COBOL85 (for which they are always enabled)

* :code:`--no-caching`   Disable caching (enabled by default)

* :code:`--storage-directory DIR`   Directory under which to store cache data --- prevents the creation of a "_superbol" storage directory at the root of project trees.


main.exe pp
~~~~~~~~~~~~~

Preprocess a list of COBOL files, generating a preprocessed file with extension .i for each of them



**DESCRIPTION**




**USAGE**
::
  
  main.exe pp FILE [OPTIONS]

Where options are:


* :code:`FILE`   Cobol file to preprocess

* :code:`-D VAR=VAL`   Define a pre-processor variable VAR, with value VAL

* :code:`-I DIRECTORY`   Add DIRECTORY to library search path

* :code:`--check`   If true, check the output (implies --parse)

* :code:`--cobc`   Activate cobc specific features

* :code:`--conf CONF_FILE`   Set the configuration file to be used

* :code:`--dialect DIALECT` or :code:`--std DIALECT`   Set the dialect to bu used (overriden by `--conf` if used)

* :code:`--free`   Shorthand for `--source-format FREE`

* :code:`--output FILE` or :code:`-o FILE`   Output File (use '-' for stdout)

* :code:`--parse`   If true, parse the generated cobol before printing

* :code:`--recovery BOOL`   Enable/disable parser recovery after syntax errors (default: true)

* :code:`--silence STRING`   Silence specific messages

* :code:`--source-format SOURCE_FORMAT`   Set the format of source code; allowed values are: { FIXED (the default), FREE}
Overrides `format` from configuration file if present.


main.exe project config
~~~~~~~~~~~~~~~~~~~~~~~~~

Print current project configuration in TOML format



**DESCRIPTION**


This command prints the current project configuration.

**USAGE**
::
  
  main.exe project config DIR [OPTIONS]

Where options are:


* :code:`DIR`   Project directory


main.exe project init
~~~~~~~~~~~~~~~~~~~~~~~

Project initialization



**DESCRIPTION**


This command initializes a default project in a given directory (or the current directory if not provided).

**USAGE**
::
  
  main.exe project init DIR [OPTIONS]

Where options are:


* :code:`DIR`   Project directory


main.exe snapshot
~~~~~~~~~~~~~~~~~~~

Manage environment snapshots



**DESCRIPTION**


This command can be used to snapshot the environment when a command is called, typically in a test script, to be able to run this command in the same environment from outside the script.

A snapshot typically contains:

* * **cmd**
  The command to run with its arguments

* * **pwd**
  The directory where the command should be run

* * **env**
  The environment variables for the command

Snapshots are stored in **$HOME/.config/superbol/snapshots**.


**SNAPSHOT CREATION**


To snapshot a command, use:
::

  # mname tname --save SNAP_ID -- cmd args


The previous command will create a snapshot **SNAP_ID**, and then run the command **cmd args**.

You can use **--quit** if you don't want to run the command at all (the command will exit with status 2)


**SNAPSHOT USAGE**


To run a command in a previously created snapshot, use:
::

  # mname tname --load SNAP_ID -- cmd args


The previous command will load the snapshot **SNAP_ID**, go to its directory, set the environment and then run the command **cmd args**.

If **cmd args** is empty, then the snapshot command is run.

The argument **--no-cd** can be used to run the command in the current directory.

The argument **--env VAR=VALUE** can be used to add an extra variable to the environment, after the one set by the snapshot.

If a **cmd args** is provided, the following special items are substituted:

* * **__**
  is replaced by all the arguments (including the command) from the snapshot

* * **_0**
  is replaced by the command from the snapshot

* * **_1**
  is replaced by all the arguments (excluding the commnad) from the snapshot

For example:
::

  # mname tname --load ID -- gdb _0 --args __


will run the command inside **gdb** with its arguments as provided by the snapshot.

**USAGE**
::
  
  main.exe snapshot ARGS [OPTIONS]

Where options are:


* :code:`ARGS`   Command line arguments

* :code:`--env VAR=VALUE`   Set a variable in the environment

* :code:`--load ID`   Load snapshot ID

* :code:`--no-cd`   Do not change directory to run the command

* :code:`--quit`   Do not run the command, just exit

* :code:`--save ID`   Create snapshot ID from state


main.exe sql preproc
~~~~~~~~~~~~~~~~~~~~~~

Preprocess SQL EXECs


**USAGE**
::
  
  main.exe sql preproc FILE [OPTIONS]

Where options are:


* :code:`FILE`   COBOL files to preproc

* :code:`-D VAR=VAL`   Define a pre-processor variable VAR, with value VAL

* :code:`-I DIRECTORY`   Add DIRECTORY to library search path

* :code:`--conf CONF_FILE`   Set the configuration file to be used

* :code:`--copybooks`   Preprocess copybooks also (without REPLACING)

* :code:`--dialect DIALECT` or :code:`--std DIALECT`   Set the dialect to bu used (overriden by `--conf` if used)

* :code:`--ext EXT`   Add .EXT as an extension to find copybooks (default is cpy)

* :code:`--free`   Shorthand for `--source-format FREE`

* :code:`--recovery BOOL`   Enable/disable parser recovery after syntax errors (default: true)

* :code:`--silence STRING`   Silence specific messages

* :code:`--source-format SOURCE_FORMAT`   Set the format of source code; allowed values are: { FIXED (the default), FREE}
Overrides `format` from configuration file if present.


main.exe switch
~~~~~~~~~~~~~~~~~

Print current switch



**DESCRIPTION**


This command prints the current default switch.


**ABOUT SWITCHES**


Switches are used to easily handle multiple GnuCOBOL installations. The section **[switches]** in the user configuration file **$HOME/.config/superbol/config.toml** contains several variables:

* * 'dir'
  The directory where installations should be performed, and switches imported from.

* * 'current'
  The current switch name to use by default

* * 'list'
  The list of known switches, with the corresponding installation directory

* * 'num'
  A counter used to name switches

Switches can be created, imported and used using the following commands:

* **superbol env [SWITCH] [--last] [--global]**
  Output a list of shell commands to set the environment variables to use a specific switch

* **superbol import [DIRS] [--clear] [--no-set]**
  Scan directories to detect GnuCOBOL installations, and created associated switches

* **superbol add DIR [--switch SWITCH] [--no-set]**
  Add a specific GnuCOBOL installation directory as a switch

* **superbol build [DIR] [--sudo] [--switch SWITCH] [--no-set]**
  From inside GnuCOBOL sources, configure, build and install and add the corresponding switch

* **superbol list**
  List known switches

* **superbol set [SWITCH] [--last]**
  Set the current default switch

**USAGE**
::
  
  main.exe switch [OPTIONS]

Where options are:



main.exe switch add
~~~~~~~~~~~~~~~~~~~~~

Add a directory as a switch



**DESCRIPTION**


This command adds a new known switch.


**ABOUT SWITCHES**


Switches are used to easily handle multiple GnuCOBOL installations. The section **[switches]** in the user configuration file **$HOME/.config/superbol/config.toml** contains several variables:

* * 'dir'
  The directory where installations should be performed, and switches imported from.

* * 'current'
  The current switch name to use by default

* * 'list'
  The list of known switches, with the corresponding installation directory

* * 'num'
  A counter used to name switches

Switches can be created, imported and used using the following commands:

* **superbol env [SWITCH] [--last] [--global]**
  Output a list of shell commands to set the environment variables to use a specific switch

* **superbol import [DIRS] [--clear] [--no-set]**
  Scan directories to detect GnuCOBOL installations, and created associated switches

* **superbol add DIR [--switch SWITCH] [--no-set]**
  Add a specific GnuCOBOL installation directory as a switch

* **superbol build [DIR] [--sudo] [--switch SWITCH] [--no-set]**
  From inside GnuCOBOL sources, configure, build and install and add the corresponding switch

* **superbol list**
  List known switches

* **superbol set [SWITCH] [--last]**
  Set the current default switch

**USAGE**
::
  
  main.exe switch add DIR [OPTIONS]

Where options are:


* :code:`DIR`   Directory to add

* :code:`--no-set`   Do not set this directory as the current one

* :code:`--switch SWITCH`   Name of switch to add


main.exe switch build
~~~~~~~~~~~~~~~~~~~~~~~

Configure, build, install GnuCOBOL and add a switch



**DESCRIPTION**


This command will build and install GnuCOBOL and add the corresponding switch. If DIR is specified, the installation directory will be created inside, otherwise the **dir** user option will be used. The name of the directory and the switch names are generated automatically from the GIT configuration. If **--switch SWITCH** is provided, it will be used for the switch name. The **--sudo** option will decide if installation should be performed with sudo. If the installation is successful, the switch is created and automatically set as the default switch, unless **--no-set** is specified.


**ABOUT SWITCHES**


Switches are used to easily handle multiple GnuCOBOL installations. The section **[switches]** in the user configuration file **$HOME/.config/superbol/config.toml** contains several variables:

* * 'dir'
  The directory where installations should be performed, and switches imported from.

* * 'current'
  The current switch name to use by default

* * 'list'
  The list of known switches, with the corresponding installation directory

* * 'num'
  A counter used to name switches

Switches can be created, imported and used using the following commands:

* **superbol env [SWITCH] [--last] [--global]**
  Output a list of shell commands to set the environment variables to use a specific switch

* **superbol import [DIRS] [--clear] [--no-set]**
  Scan directories to detect GnuCOBOL installations, and created associated switches

* **superbol add DIR [--switch SWITCH] [--no-set]**
  Add a specific GnuCOBOL installation directory as a switch

* **superbol build [DIR] [--sudo] [--switch SWITCH] [--no-set]**
  From inside GnuCOBOL sources, configure, build and install and add the corresponding switch

* **superbol list**
  List known switches

* **superbol set [SWITCH] [--last]**
  Set the current default switch

**USAGE**
::
  
  main.exe switch build DIR [OPTIONS]

Where options are:


* :code:`DIR`   Directory where GnuCOBOL should be installed

* :code:`--branch BRANCH`   Branch name to use instead of git branch (the auto-detected name from git will be 3.x-$DATE-$COMMIT)

* :code:`--no-set`   Do not set this directory as the current one

* :code:`--sudo`   Use sudo for 'make install'

* :code:`--switch SWITCH`   Name of switch to add


main.exe switch config
~~~~~~~~~~~~~~~~~~~~~~~~

Change the current config



**DESCRIPTION**


This command sets the current default switch.


**ABOUT SWITCHES**


Switches are used to easily handle multiple GnuCOBOL installations. The section **[switches]** in the user configuration file **$HOME/.config/superbol/config.toml** contains several variables:

* * 'dir'
  The directory where installations should be performed, and switches imported from.

* * 'current'
  The current switch name to use by default

* * 'list'
  The list of known switches, with the corresponding installation directory

* * 'num'
  A counter used to name switches

Switches can be created, imported and used using the following commands:

* **superbol env [SWITCH] [--last] [--global]**
  Output a list of shell commands to set the environment variables to use a specific switch

* **superbol import [DIRS] [--clear] [--no-set]**
  Scan directories to detect GnuCOBOL installations, and created associated switches

* **superbol add DIR [--switch SWITCH] [--no-set]**
  Add a specific GnuCOBOL installation directory as a switch

* **superbol build [DIR] [--sudo] [--switch SWITCH] [--no-set]**
  From inside GnuCOBOL sources, configure, build and install and add the corresponding switch

* **superbol list**
  List known switches

* **superbol set [SWITCH] [--last]**
  Set the current default switch

**USAGE**
::
  
  main.exe switch config [OPTIONS]

Where options are:


* :code:`--compiler-coverage BOOL`   Set compiler coverage

* :code:`--set-last`   Use the latest imported switch

* :code:`--set-switch SWITCH`   Switch to use


main.exe switch env
~~~~~~~~~~~~~~~~~~~~~

Generate commands to set environment variables to use the specified switch



**DESCRIPTION**


This command generates a list of command to set environment variables for an existing switch.

It is usually used as:
::

  # eval $(superbol env)
  # cobc --version


The **--switch SWITCH** option can be used to choose a different switch from the default one. SWITCH should match a sub-string of a switch name. **--last** can be used to select the most recent switch.

The **--global** option can be used to generate a file **$HOME/.config/superbol/env** that can be loaded from shell init scripts, using a symlink to point PATH and LD_LIBRARY_PATH to the current switch at any time. Changing the current switch will then automatically redirect cobc to that new switch.


**ABOUT SWITCHES**


Switches are used to easily handle multiple GnuCOBOL installations. The section **[switches]** in the user configuration file **$HOME/.config/superbol/config.toml** contains several variables:

* * 'dir'
  The directory where installations should be performed, and switches imported from.

* * 'current'
  The current switch name to use by default

* * 'list'
  The list of known switches, with the corresponding installation directory

* * 'num'
  A counter used to name switches

Switches can be created, imported and used using the following commands:

* **superbol env [SWITCH] [--last] [--global]**
  Output a list of shell commands to set the environment variables to use a specific switch

* **superbol import [DIRS] [--clear] [--no-set]**
  Scan directories to detect GnuCOBOL installations, and created associated switches

* **superbol add DIR [--switch SWITCH] [--no-set]**
  Add a specific GnuCOBOL installation directory as a switch

* **superbol build [DIR] [--sudo] [--switch SWITCH] [--no-set]**
  From inside GnuCOBOL sources, configure, build and install and add the corresponding switch

* **superbol list**
  List known switches

* **superbol set [SWITCH] [--last]**
  Set the current default switch

**USAGE**
::
  
  main.exe switch env SWITCH [OPTIONS]

Where options are:


* :code:`SWITCH`   Switch to use, instead of current

* :code:`--global`   Use global configuration for default switch

* :code:`--last`   Use the latest imported switch


main.exe switch import
~~~~~~~~~~~~~~~~~~~~~~~~

Import existing GnuCOBOL installations as switches



**DESCRIPTION**


This command will scan the directories, looking for **gnucobol-*** folders with GnuCOBOL installed, and add them as switches. With no argument, it scans the default installation directory.


**ABOUT SWITCHES**


Switches are used to easily handle multiple GnuCOBOL installations. The section **[switches]** in the user configuration file **$HOME/.config/superbol/config.toml** contains several variables:

* * 'dir'
  The directory where installations should be performed, and switches imported from.

* * 'current'
  The current switch name to use by default

* * 'list'
  The list of known switches, with the corresponding installation directory

* * 'num'
  A counter used to name switches

Switches can be created, imported and used using the following commands:

* **superbol env [SWITCH] [--last] [--global]**
  Output a list of shell commands to set the environment variables to use a specific switch

* **superbol import [DIRS] [--clear] [--no-set]**
  Scan directories to detect GnuCOBOL installations, and created associated switches

* **superbol add DIR [--switch SWITCH] [--no-set]**
  Add a specific GnuCOBOL installation directory as a switch

* **superbol build [DIR] [--sudo] [--switch SWITCH] [--no-set]**
  From inside GnuCOBOL sources, configure, build and install and add the corresponding switch

* **superbol list**
  List known switches

* **superbol set [SWITCH] [--last]**
  Set the current default switch

**USAGE**
::
  
  main.exe switch import DIRS [OPTIONS]

Where options are:


* :code:`DIRS`   Directories to scan

* :code:`--clear`   Clear the list before importing

* :code:`--no-set`   Do not set the last imported directory as the current one


main.exe switch list
~~~~~~~~~~~~~~~~~~~~~~

List known switches



**DESCRIPTION**


This command list existing switches.


**ABOUT SWITCHES**


Switches are used to easily handle multiple GnuCOBOL installations. The section **[switches]** in the user configuration file **$HOME/.config/superbol/config.toml** contains several variables:

* * 'dir'
  The directory where installations should be performed, and switches imported from.

* * 'current'
  The current switch name to use by default

* * 'list'
  The list of known switches, with the corresponding installation directory

* * 'num'
  A counter used to name switches

Switches can be created, imported and used using the following commands:

* **superbol env [SWITCH] [--last] [--global]**
  Output a list of shell commands to set the environment variables to use a specific switch

* **superbol import [DIRS] [--clear] [--no-set]**
  Scan directories to detect GnuCOBOL installations, and created associated switches

* **superbol add DIR [--switch SWITCH] [--no-set]**
  Add a specific GnuCOBOL installation directory as a switch

* **superbol build [DIR] [--sudo] [--switch SWITCH] [--no-set]**
  From inside GnuCOBOL sources, configure, build and install and add the corresponding switch

* **superbol list**
  List known switches

* **superbol set [SWITCH] [--last]**
  Set the current default switch

**USAGE**
::
  
  main.exe switch list [OPTIONS]

Where options are:



main.exe switch set
~~~~~~~~~~~~~~~~~~~~~

Set the current default switch



**DESCRIPTION**


This command sets the current default switch.


**ABOUT SWITCHES**


Switches are used to easily handle multiple GnuCOBOL installations. The section **[switches]** in the user configuration file **$HOME/.config/superbol/config.toml** contains several variables:

* * 'dir'
  The directory where installations should be performed, and switches imported from.

* * 'current'
  The current switch name to use by default

* * 'list'
  The list of known switches, with the corresponding installation directory

* * 'num'
  A counter used to name switches

Switches can be created, imported and used using the following commands:

* **superbol env [SWITCH] [--last] [--global]**
  Output a list of shell commands to set the environment variables to use a specific switch

* **superbol import [DIRS] [--clear] [--no-set]**
  Scan directories to detect GnuCOBOL installations, and created associated switches

* **superbol add DIR [--switch SWITCH] [--no-set]**
  Add a specific GnuCOBOL installation directory as a switch

* **superbol build [DIR] [--sudo] [--switch SWITCH] [--no-set]**
  From inside GnuCOBOL sources, configure, build and install and add the corresponding switch

* **superbol list**
  List known switches

* **superbol set [SWITCH] [--last]**
  Set the current default switch

**USAGE**
::
  
  main.exe switch set SWITCH [OPTIONS]

Where options are:


* :code:`SWITCH`   Switch to use

* :code:`--last`   Use the latest imported switch


main.exe texi2rst
~~~~~~~~~~~~~~~~~~~

build .texi documentation from gnucobol-docs



**DESCRIPTION**


Build .texi documentation from gnucobol-docs.

**USAGE**
::
  
  main.exe texi2rst FILE [OPTIONS]

Where options are:


* :code:`FILE`   .texi file

* :code:`-I DIR`   Add to lookup path for files

* :code:`-o DIR`   Target directory for RST generation


main.exe util detect cycle
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Detect a cycle of lines in a file



**DESCRIPTION**


This command will take a file of lines and detect cycles in the lines, and simplify them.


**ABOUT UTILS**


This is a list of small sub-commands that may be useful from time to time.

**USAGE**
::
  
  main.exe util detect cycle FILES [OPTIONS]

Where options are:


* :code:`FILES`   Files to unrec


main.exe util wc
~~~~~~~~~~~~~~~~~~

Stats on file of lines (without taking UTF8 chars into account)



**DESCRIPTION**


This command will print different statistics on a file of lines. Contrarily to wc, it will not take into account UTF8 chars for the max line length.


**ABOUT UTILS**


This is a list of small sub-commands that may be useful from time to time.

**USAGE**
::
  
  main.exe util wc FILES [OPTIONS]

Where options are:


* :code:`FILES`   Files to wc
