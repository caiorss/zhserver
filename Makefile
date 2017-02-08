all: server 

zotero:
	stack exec -- ghc --make Zotero 

server:
	stack exec -- ghc --make Server

run:
	./Server 

clean:
	rm -rf *.o *.hi *.bin Zotero Server
