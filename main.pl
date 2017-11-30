:- use_module(library(clpfd)).

snake0([3,2,2,2,2,1,2,2,2,2,2,1,2,2,2,1,2,2,2,2,2,1,2,2,2,2,3]). % ECCCCSCCCCCSCCCSCCCCCSCCCCE   
snake2([3,2,2,2,2,2,2,3]).
snake3([3,1,2,2,2,1,2,1,2,2,2,2,2,2,2,2,2,2,2,2,2,1,2,1,2,2,3]).

/**
	Cube values are 1..3, representing Straights, Corners, and Ends.
	Transitions values are 1..6 standing for the six directions in space where the next cube is, Front, Left, Up, Back, Right, Down respectively.
*/
snake_cube(Snake, NStraights, NSolutions):-
	snake0(Snake),
	length(Snake, NCubes),
	
	domain(Snake, 1, 3),
	append([3], Tail, Snake),
	append(Body, [3], Tail),
	domain(Body, 1, 2),
	
	NTransitions is NCubes - 1,
	length(Transitions, NTransitions),
	domain(Transitions, 1, 6),
	
	duos(Transitions),
	trios(Transitions),
	select_transition(Body, Transitions),
	labeling([], Transitions),
	write(Transitions).

select_transition([], _).
select_transition([C | Cubes], [T1, T2 | Transitions]):-
	C = 1,
	((T1 #= 1 #\/ T1 #= 4) #/\ (T2 #= 1 #\/ T2 #= 4))
	#\/ ((T1 #= 2 #\/ T1 #= 5) #/\ (T2 #= 2 #\/ T2 #= 5))
	#\/ ((T1 #= 3 #\/ T1 #= 6) #/\ (T2 #= 3 #\/ T2 #= 6)),
	select_transition(Cubes, [T2 | Transitions]).
select_transition([C | Cubes], [T1, T2 | Transitions]):-
	C = 2,
	#\ ((T1 #= 1 #\/ T1 #= 4) #/\ (T2 #= 1 #\/ T2 #= 4)),
	#\ ((T1 #= 2 #\/ T1 #= 5) #/\ (T2 #= 2 #\/ T2 #= 5)),
	#\ ((T1 #= 3 #\/ T1 #= 6) #/\ (T2 #= 3 #\/ T2 #= 6)),
	select_transition(Cubes, [T2 | Transitions]).

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
