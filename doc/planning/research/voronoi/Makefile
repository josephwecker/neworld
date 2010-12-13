C=edgelist.c geometry.c heap.c main.c memory.c output.c voronoi.c 
O=edgelist.o geometry.o heap.o main.o memory.o output.o voronoi.o

tt: voronoi t
	voronoi -t <t >tt
voronoi: $O
	cc -o voronoi $O -lm
$O:defs.h
