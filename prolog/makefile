.PHONY: tests shell bench pl docs coverage

tests:
	(cd specs; runghc -i../src Specs)

shell:
	ghci -isrc -iprolog-graph -outputdir dist/build Prolog GraphViz Quote IsString -XOverloadedStrings -XQuasiQuotes

bench:
	( cd bench; \
	  ghc -i../src -O --make Bench -main-is Bench -o runbench && \
	  time -p ./runbench 8 \
	)

Fig01_08:
	( cd bench; \
	  ghc -i../src -O --make Fig01_08 -main-is Fig01_08 -o fig01_08 && \
	  time -p ./runbench \
	)

pl:
	ghc -isrc -outputdir dist/build -O --make Console -main-is Console -o $@

docs:
	cabal configure && cabal haddock --hyperlink-source

coverage:
	ghc -fhpc -isrc -outputdir dist/build Specs -main-is Specs -o coverage/runspecs
	cd coverage; ./runspecs ../specs 2>/dev/null >/dev/null
	hpc report coverage/runspecs
	hpc markup coverage/runspecs --destdir=coverage --exclude=Prolog --exclude=Specs
	rm coverage/runspecs*


profiling:
	cabal configure --enable-library-profiling --enable-executable-profiling --enable-tests --enable-benchmarks
