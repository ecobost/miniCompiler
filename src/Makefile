mini: *.hs
	ghc --make -o mini Main.hs


test: mini
	for fn in tests/*.mini ; do \
	  echo $$fn ;\
          ./mini -v $$fn ; \
        done
