
prof:
	rm -f Prof
	ghc -O2 -prof -threaded -auto-all -o Prof main.hs
	./Prof +RTS -p -N -s
	cat Prof.prof

move:
	cp main.hs ~/Dropbox/hs
	cp -r game ~/Dropbox/hs