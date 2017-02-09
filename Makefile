all: server 

app = zhserver.bin
src = src/ZHServer.hs src/Zotero.hs

config = src/zhserver.conf

zotero:
	stack exec -- ghc --make Zotero 

server: $(src)
	stack exec -- ghc --make -o $(app) $(src)
	# stack exec -- ghc src/ZHServer.hs -o bin/ZHServer.bin

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
	./$(app) --conf $(config)

run-test2: dbtest server
	./$(app) --conf ./my-zhserver.conf


clean-db:
	rm -rf ./testdb

clean:
	rm -rf *.o *.hi *.bin Zotero Server

clean-all:
	rm -rf ./testdb
	rm -rf *.o *.hi *.bin Zotero Server
