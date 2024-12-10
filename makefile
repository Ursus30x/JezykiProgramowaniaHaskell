main: clean
	ghc --make liczbyZaprzyjaznione.hs

clean:
	rm -f liczbyZaprzyjaznione
	rm -f liczbyZaprzyjaznione.hi
	rm -f liczbyZaprzyjaznione.o