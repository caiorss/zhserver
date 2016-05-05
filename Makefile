all: server 

zotero:
	nix-shell --command "ghc -c Zotero.hs"

server:
	nix-shell --command "ghc --make Server"

clean:
	rm -rf *.o *.hi *.bin Zotero Server
