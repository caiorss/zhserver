all: server 

app = zhserver.bin
src = src/ZHServer.hs src/Zotero.hs

config = src/zhserver.conf

zhclient:
	stack exec -- ghc --make -o zhclient.bin src/zhclient.hs src/Zotero.hs

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
	cat $(dbsrc) | sqlite3 $(dbtest)

dbtest-view: dbtest
	sqlitebrowser $(dbtest)


run: server
	./$(app) 

# Load sample configuration file 
run-test: dbtest server
	./$(app) --conf $(config)

run-test2: dbtest server
	./$(app) --conf ./my-zhserver.conf

# Load configuration file from environment variable 
run-test3: dbtest server 
	env ZHSERVER_CONF=src/zhserver.conf ./$(app) 

# Run with configuration passed as command line arguments
run-test4: dbtest server
	# Run server with
	# - Listen all hosts	: 0.0.0.0
	# - Listen port			: 9090
    # - Database driver		: "sqlite://testdb/zotero.sqlite"
    # - Static files		: ./assets/
    # - Storage path		: testdb/storage 
	./$(app) --params 0.0.0.0 9090 "sqlite://testdb/zotero.sqlite" ./assets/ testdb/storage 


# Test connection to a PostGres Server 
run-test5: dbtest server
	# Run server with
	# - Listen all hosts	: 0.0.0.0
	# - Listen port			: 9090
    # - Database driver		: "sqlite://testdb/zotero.sqlite"
    # - Static files		: ./assets/
    # - Storage path		: testdb/storage 
	./$(app) --params 0.0.0.0 9090 "postgres://postgres@localhost/zotero" ./assets/ testdb/storage 


#========================================#

app: zhserver
	mkdir -p lib 
	rm -rf lib/*

	# Copy shared libraries to ./lib directory
	sh copylibs.sh


app-run: dbtest 
	sh zhserver.sh --params 0.0.0.0 9090 "sqlite://testdb/zotero.sqlite" assets testdb/storage 

# Copy Sqlite database from system to top level project directory

sample-db     := testdb/zotero-db.sqlite
sample-db-src := /home/arch/.mozilla/firefox/dic34vce.default/zotero/zotero.sqlite

update: $(sample-db)

$(sample-db): $(sample-db-src)
	cp $(sample-db-src) $(sample-db)

#========= Clean Rules ===================#

doc:
	stack exec -- haddock --html src/Zotero.hs src/ZHServer.hs --hyperlinked-source --odir=dist/docs

doc-show:
	firefox dist/docs/index.html

clean-db:
	rm -rf ./testdb

clean:
	rm -rf *.o *.hi *.bin Zotero Server

clean-all:
	rm -rf ./testdb
	rm -rf *.o *.hi *.bin Zotero Server
