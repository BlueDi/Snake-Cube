:- use_module(library(clpfd)).
:- dynamic dim/1.
:- dynamic dim2/1.

snake0([3,2,2,2,2,1,2,2,2,2,2,1,2,2,2,1,2,2,2,2,2,1,2,2,2,2,3]). % ECCCCSCCCCCSCCCSCCCCCSCCCCE   
snake2([3,2,2,2,2,2,2,3]).
snake3([3,1,2,2,2,1,2,1,2,2,2,2,2,2,2,2,2,2,2,2,2,1,2,1,2,2,3]).

/**
	Cube values are 1..3, representing Straights, Corners, and Ends.
	Transitions values are 1..6 standing for the six directions in space where the next cube is, Front, Left, Up, Back, Right, Down respectively.
*/
snake_cube(Snake, NStraights, NSolutions):-
	snake2(Snake),
	length(Snake, NCubes),
	Dim is round(NCubes ** (1/3)),
	asserta(dim(Dim)),
	asserta(dim2(NCubes)),
	StartPosition in 1..NCubes,
	create_initial_cube(StartPosition, Cube),
	
	domain(Snake, 1, 3),
	append([3], Tail, Snake),
	append(Body, [3], Tail),
	domain(Body, 1, 2),
	
	NTransitions is NCubes - 1,
	length(Transitions, NTransitions),
	domain(Transitions, 1, 6),
	
	duos(Transitions),
	%trios(Transitions),
	select_transitions(StartPosition, Body, Cube, Transitions),
	labeling([], Transitions),
	write(Transitions).
	
create_initial_cube(FirstCube, Cube):-
	dim2(FinalLength),
	length(Cube, FinalLength),
	domain(Cube, 1, 2),
	element(FirstCube, Cube, 2),
	N1 #= FinalLength - 1,
	global_cardinality(Cube, [1-N1, 2-1]).

select_transitions(_, [], _, [_]).
select_transitions(SP, [C | Cubes], BigCube, [T1, T2 | Transitions]):-
	C = 1,
	T1 #= T2,
	update_big_cube(SP, T1, BigCube, NP, NewBigCube),
	select_transitions(NP, Cubes, NewBigCube, [T2 | Transitions]).
select_transitions(SP, [C | Cubes], BigCube, [T1, T2 | Transitions]):-
	C = 2,
	#\ ((T1 #= 1 #\/ T1 #= 4) #/\ (T2 #= 1 #\/ T2 #= 4)),
	#\ ((T1 #= 2 #\/ T1 #= 5) #/\ (T2 #= 2 #\/ T2 #= 5)),
	#\ ((T1 #= 3 #\/ T1 #= 6) #/\ (T2 #= 3 #\/ T2 #= 6)),
	update_big_cube(SP, T1, BigCube, NP, NewBigCube),
	select_transitions(NP, Cubes, NewBigCube, [T2 | Transitions]).

/**
	Updates the state of the final cube.
*/
update_big_cube(Position, N, BigCube, NP, NewBigCube):-
	dim(Dim),
	dim2(Dim2),
	length(BigCube, ND),
	length(NewBigCube, ND),
	((N #= 1 #/\ NP #= Position + Dim)
	#\/ (N #= 2 #/\ NP #= Position - 1)
	#\/ (N #= 3 #/\ NP #= Position + Dim2)
	#\/ (N #= 4 #/\ NP #= Position - Dim)
	#\/ (N #= 5 #/\ NP #= Position + 1)
	#\/ (N #= 6 #/\ NP #= Position - Dim2)),
	NP #> 0,
	NP #=< Dim,
	element(NP, BigCube, 1),
	element(NP, NewBigCube, 2).

/**
	In a sequence of 2 transitions there can't be the oposite movement.
	For example: if the next cube is on the left, after that there can't be a cube on the right.
*/
duos([A, B | R]):-
	#\ (A #= 1 #/\ B #= 4),
	#\ (A #= 4 #/\ B #= 1),
	#\ (A #= 2 #/\ B #= 5),
	#\ (A #= 5 #/\ B #= 2),
	#\ (A #= 3 #/\ B #= 6),
	#\ (A #= 6 #/\ B #= 3),
	duos([B | R]).
duos([_]).

/**
	There can't be a sequence of 3 of the same action.
	With 3 it would leave the area of the cube.
*/
trios([A, B, C | R]):-
	#\ (A #= B #/\ B #= C),
	trios([B, C | R]).
trios([_, _]).
