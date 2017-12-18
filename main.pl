:- use_module(library(clpfd)).
:- use_module(library(lists)).
:- use_module(library(fdbg)).

:- dynamic dim/1.
:- dynamic dim2/1.
:- dynamic nCubes/1.

snake0([3,2,2,2,2,1,2,2,2,2,2,1,2,2,2,1,2,2,2,2,2,1,2,2,2,2,3]). % ECCCCSCCCCCSCCCSCCCCCSCCCCE   
snake2([3,2,2,2,2,2,2,3]).
snake3([3,1,2,2,2,1,2,1,2,2,2,2,2,2,2,2,2,2,2,2,2,1,2,1,2,2,3]).
snake4([3,2,2,2,2,2,2,1,2,1,2,1,2,1,2,2,2,2,2,2,2,2,2,2,2,2,3]). % Most solutions
dragons_tail([3,1,2,1,2,1,2,1,2,2,2,2,1,2,1,2,2,2,1,2,2,1,2,2,2,1,3]).

dim(Dim).
dim2(Dim2).
nCubes(NCubes).

/**
	Cube values are 1..3, representing Straights, Corners, and Ends.
	Transitions values are 1..6 standing for the six directions in space where the next cube is, Front, Left, Up, Back, Right, Down respectively.
*/
snake_cube(Snake):-
	snake2(Snake),
	length(Snake, NCubes),
	Dim is round(NCubes ** (1/3)),
	Dim2 is round(NCubes ** (1/2)),
	asserta(dim(Dim)),
	asserta(dim2(Dim2)),
	asserta(nCubes(NCubes)),

	nth1(1, Snake, 3),
	nth1(NCubes, Snake, 3),

	length(Positions, NCubes),
	domain(Positions, 1, NCubes),
	all_distinct(Positions),
	check_transition(Snake, Positions),
	labeling([], Positions),
	write(Positions).

check_transition([3 | Snake], Positions):-
	dim(Dim),
	dim2(Dim2),
	nCubes(NCubes),
	First in 1..NCubes,
	element(First, Positions, 1),
	element(Second, Positions, 2),
	Second #= First + 1 #\/ Second #= First - 1 #\/ Second #= First + Dim #\/ Second #= First - Dim #\/ Second #= First + Dim2 #\/ Second #= First - Dim2,
	Second in 1..NCubes,
	check_transition(Snake, Positions, First, Second).
check_transition([3], _, _, _).
check_transition([Head | Snake], Positions, One, Two):-
	dim(Dim),
	dim2(Dim2),
	nCubes(NCubes),
	element(One, Positions, N1),
	element(Two, Positions, N2),
	N2 #= N1 + 1,
	N3 #= N2 + 1,
	Delta #= Two - One,
	(Head #= 1 #/\ Three #= Two + Delta)
	#\/ (Head #= 2 #/\ abs(Delta) #= 1 #/\ (Three #= Two + Dim #\/ Three #= Two - Dim #\/ Three #= Two + Dim2 #\/ Three #= Two - Dim2))
	#\/ (Head #= 2 #/\ abs(Delta) #= Dim #/\ (Three #= Two + 1 #\/ Three #= Two - 1 #\/ Three #= Two + Dim2 #\/ Three #= Two - Dim2))
	#\/ (Head #= 2 #/\ abs(Delta) #= Dim2 #/\ (Three #= Two + 1 #\/ Three #= Two - 1 #\/ Three #= Two + Dim #\/ Three #= Two - Dim)),
	Three in 1..NCubes,
	element(Three, Positions, N3),
	check_transition(Snake, Positions, Two, Three).
