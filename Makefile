
all:
	ghc --make dka-2-mka.hs

clean:
	rm -f dka-2-mka{,.o,.hi} *.zip 2>/dev/null

pack:
	zip -r flp-fun-xvokra00.zip dka-2-mka.hs README test/
