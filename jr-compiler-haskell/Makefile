

run: build
	./main 

build: 
	ghc -o main -hidir pkg -odir pkg -isrc dir-traverse/DirTrav.hs src/*.hs

clean: 
	rm test.jr.rs

test: build
	./main -file test.jr