

zotero:
	nix-shell ~/shell.nix --command "ghc --make Zotero"

server:
	nix-shell ~/shell.nix --command "ghc --make Server"

clean:
	rm -rf *.o *.hi *.bin Zotero Server
