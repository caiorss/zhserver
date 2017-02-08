#!/usr/bin/env bash

export PGROOT=/home/postgres
mkdir /run/postgresql
chown -R postgres:postgres /var/run/postgresql
su - postgres -c "/usr/bin/pg_ctl -s -D ${PGROOT}/data start -w -t 120"
