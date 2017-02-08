all: server 

zotero:
	stack exec -- ghc --make Zotero 

server: Server 
	stack exec -- ghc --make Server

run: Server 
	./Server 

run-test:
	./Server --conf ./zhserver-test.conf

dbsrc  := database/zotero-test.sql
dbtest := testdb/zotero.sqlite
dbtest: $(dbtest)

$(dbtest): $(dbsrc)
	mkdir -p testdb
	mkdir -p testdb/storage
	cat $(dbsrc) | sqlite3 testdb/zotero.sqlite

clean-db:
	rm -rf ./testdb

clean:
	rm -rf *.o *.hi *.bin Zotero Server
