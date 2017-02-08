all: server 


zotero:
	stack exec -- ghc --make Zotero 

server: 
	stack exec -- ghc --make Server


dbsrc  := database/zotero-test.sql
dbtest := testdb/zotero.sqlite
dbtest: $(dbtest)

$(dbtest): $(dbsrc)
	mkdir -p testdb
	mkdir -p testdb/storage
	cat $(dbsrc) | sqlite3 testdb/zotero.sqlite

run: Server 
	./Server 

run-test: dbtest server
	./Server --conf ./zhserver.conf

run-test2: dbtest server
	./Server --conf ./my-zhserver.conf


clean-db:
	rm -rf ./testdb

clean:
	rm -rf *.o *.hi *.bin Zotero Server

clean-all:
	rm -rf ./testdb
	rm -rf *.o *.hi *.bin Zotero Server
