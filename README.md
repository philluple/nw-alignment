# Needleman-Wunsch alogorithm 

The NW algorithm is an important algorithm in bioinformatics through the use of dynamic programming to compare and align biological sequences. The algorithm works by iterating through letter by letter and scoring every concievable alighment to yield a maximum score and populating a matrix, which at the end, will be traversed to construct the most 'aligned' set of sequences. The implementation is done by Phillip Le and Emily Lo as the final project for COMS 4995: Parallel Functioning Fall '23. 

We implemented four different solutions to the NW-Alignment Algorithm, one sequential, and three optimization attempts. 

You may choose to run the following:
	1. `sequential`
		a. Naive sequential approach. Evaluate the the cells, from left to right, from top to bottom. 
	2. `row`
		a. Naive parallel approach. Make parallel by computing scores of entire rows in parallel.
	3. `column`
		a. Naive parallel approach. Make parallel by computing scores of entire columns in parallel.
	4. `adiagonal`
		a. Best parallel approach. Divides matrix in sections based on anti-diagonals, compute scores of cells in parallel. 

Since the algorithm involved creating a matrix with bounds of the length of both strings, we were limited 
by our silly MacBooks. Therefore, we provided three appropriate tests.
	1. 100x100.txt
		i. Aligning two sequences, each of length 100
	2. 500x500.txt
		i. Aligning two sequences, each of length 500
	3. 1000x1000.txt 
		i. Aligning two sequences, each of length 1000

To build:
```
stack build 
```

To run:

```
stack exec nw-alignment <test> <path/to/test>
```

To run parallel:
```
stack exec nw-alignment <test> <path/to/test> -- +RTS -N<number of cores>
```

Examples: 

```
stack exec nw-alignment adiagonal ./input/100x100.txt -- +RTS -N8
```

Have fun and be careful!