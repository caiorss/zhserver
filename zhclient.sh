#!/usr/bin/sh

export ZOTERO_DB="sqlite://testdb/zotero-db.sqlite"
export ZOTERO_PATH="/home/arch/.mozilla/firefox/mwad0hks.zotero/zotero/storage/" 


./zhclient.bin "$@"
