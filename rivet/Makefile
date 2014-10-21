all:
	cabal sandbox init
	cabal sandbox add-source ../rivet-core/
	cabal sandbox add-source ../rivet-docker/
	cabal sandbox add-source ../rivet-migration/
	cabal sandbox add-source ../rivet-simple-deploy/
	cabal install -j && cp .cabal-sandbox/bin/rivet ~/.cabal/bin
