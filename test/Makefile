run: prepare
	./run.sh

prepare: bin check
	mkdir -p res
	./prepare.sh

bin:
	ghc ../dka-2-mka.hs

check:
	@which fstcompile fstminimize fstequivalent >/dev/null 2>&1 && echo "Required tools found" || echo "OpenFST libtools are missing, http://openfst.org"

clean:
	rm -f ./res/* ./minimal/* > /dev/null 2>&1
	rm -f ../dka-2-mka{.hi,.o,}
