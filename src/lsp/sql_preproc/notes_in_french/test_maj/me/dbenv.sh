#!/bin/bash

# Source this file before running any COBOL program that uses the DB !
# Of course you should have set up your PostgreSQL server and database

export COB_DBSOURCE="pgsql://localhost:5432/testdb"
export COB_DBUSER="postgres"
export COB_DBPASS="postgres"
