# Needleman-Wunsch alogorithm 

The NW algorithm is an important algorithm in bioinformatics through the use of dynamic programming to compare and align biological sequences. The algorithm works by iterating through letter by letter and scoring every concievable alighment to yield a maximum score. The implementation 
is done by Phillip Le and Emily Lo as the final project for COMS 4995: Parallel Functioning Fall '23. 

 
To build:
```
stack build 
```

To run:
```
stack exec nw-solver <path/to/puzzle>
```

To run parallel:
```
stack exec nonogram-solver <path/to/puzzle> -- +RTS -N<number of cores>
```

Additionally the following additional targes are provided.

To run all unit tests:
```
stack test
```

To benchmark the program:
```
stack bench
```