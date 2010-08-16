all: prep
	ghc -O3 -Wall           -i../src -odir ./tmp -hidir ./tmp --make ../src/Tests/Main.hs -o main
	./main +RTS -s

prof: prep
	ghc -O3 -prof -auto-all -i../src -odir ./tmp -hidir ./tmp --make ../src/Tests/Main.hs -o main
	./main +RTS -p

hpc: prep
	ghc -fhpc -O3 -i../src -odir ./tmp -hidir ./tmp --make ../src/Tests/Main.hs -o main
	./main
	hpc markup main

prep: clean
	locate *.mid > mid.txt
	locate *.sf2 > sf2.txt
	locate *.wav > wav.txt
	mkdir tmp

clean:
	rm -rf ./tmp
	rm -rf .hpc
	rm -f *.o *.hi *.html *.tix *.aux *.hp *.prof *.ps main mid.txt sf2.txt wav.txt
