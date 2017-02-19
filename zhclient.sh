#!/usr/bin/sh

export ZOTERO_DB="sqlite:///home/arch/.mozilla/firefox/mwad0hks.zotero/zotero/zotero2.sqlite"
export ZOTERO_PATH="/home/arch/.mozilla/firefox/mwad0hks.zotero/zotero/storage/" 



./zhclient.bin "$@"
