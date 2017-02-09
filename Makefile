all: server 

app = ZHServer 

zotero:
	stack exec -- ghc --make Zotero 

server: 
	stack exec -- ghc --make $(app)

## Linux Centos specific dependencies
deps-centos:
	yum install sqlite-devel.x86_64
	yum install postgresql-devel.x86_64

## Install dependencies need to compile the project
deps:
	stack install HDBC
	stack install HDBC-sqlite3
	stack install HDBC-postgresql
	stack install happstack-server 


dbsrc  := database/zotero-test.sql
dbtest := testdb/zotero.sqlite
dbtest: $(dbtest)

$(dbtest): $(dbsrc)
	mkdir -p testdb
	mkdir -p testdb/storage
	cat $(dbsrc) | sqlite3 testdb/zotero.sqlite

run: server
	./$(app) 

run-test: dbtest server
	./$(app) --conf ./zhserver.conf

run-test2: dbtest server
	./$(app) --conf ./my-zhserver.conf


clean-db:
	rm -rf ./testdb

clean:
	rm -rf *.o *.hi *.bin Zotero Server

clean-all:
	rm -rf ./testdb
	rm -rf *.o *.hi *.bin Zotero Server
