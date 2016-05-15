#!/usr/bin/env bash

# Remove database 
sudo su - postgres -c "dropdb zotero"

# Create database
sudo su - postgres -c "createdb zotero"

# Run SQL command 
cat tables.pgsql | psql -h localhost -U postgres zotero | tee error.log

scsh -s sqlite2pg.scm
