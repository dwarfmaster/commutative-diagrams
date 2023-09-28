
all:
	wasm-pack build --target web

serve:
	../server/target/debug/commutative-diagrams-engine-server \
		--vfiles ../../coq/theories \
		--modname CommutativeDiagrams \
		--plugin ../../coq/_build/default

