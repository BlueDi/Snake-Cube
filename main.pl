:- use_module(library(clpfd)).
:- use_module(library(lists)).
:- use_module(library(aggregate)).

snake_2d([3,2,2,2,2,2,2,3]).
snake_example1([3,2,2,2,2,1,2,2,2,2,2,1,2,2,2,1,2,2,2,2,2,1,2,2,2,2,3]).
snake_example2([3,1,2,2,2,1,2,1,2,2,2,2,2,2,2,2,2,2,2,2,2,1,2,1,2,2,3]).
unique_solution([3,2,2,2,2,2,2,2,2,1,2,1,2,2,2,1,2,2,2,2,2,2,2,2,2,2,3]). % A snake with a unique solution
most_solutions([3,2,2,2,2,2,2,1,2,1,2,1,2,1,2,2,2,2,2,2,2,2,2,2,2,2,3]). % The snake with most solutions
dragons_tail([3,1,2,1,2,1,2,1,2,2,2,2,1,2,1,2,2,2,1,2,2,1,2,2,2,1,3]).
dragons_tail2([3,1,2,2,2,1,2,2,1,2,2,2,1,2,1,2,2,2,2,1,2,1,2,1,2,1,3]).

/**
	Snake Cube Solver
	Pieces values are 1..3, representing Straights, Corners, and Ends.
	Positions is a list of values that represent the order on wich the pieces of the cube will be visited.
*/
snake_cube(Snake):-
	open('SnakeCubeSolutions.txt', write, Stream),
	length(Snake, NCubes),
	Dim is round(NCubes ** (1/3)),
	Dim2 is round(Dim ** 2),
	LastPosition is NCubes - 1,

	nth1(1, Snake, 3),
	nth1(NCubes, Snake, 3),

	length(Positions, NCubes),
	domain(Positions, 0, LastPosition),
	all_distinct(Positions),
	check_transition_simple(Snake, Dim, Dim2, Positions),
	forall(labeling([], Positions), (write(Positions), nl, format(Stream, "~w~N", [Positions]))),
	close(Stream).

/**
	A way to only get the cubes that start on the same corner.
*/	
check_transition_simple([3 | Snake], Dim, Dim2, Positions):-
	element(1, Positions, 0),
	check_transition(Snake, Dim, Dim2, Positions, 0).

/**
	Restritions on the order of the pieces:
		To change line, the piece has to be on the same column and in the same layer.
		To change column, the piece has to be on the same line and in the same layer.
		To change layer, the piece has to be on the same line and in the same column.
*/
check_transition([3 | Snake], Dim, Dim2, Positions):-
	element(FirstT, Positions, 0),
	element(SecondT, Positions, 1),
	First #= FirstT - 1,
	Second #= SecondT - 1,
	(((Second #= First + 1 #\/ Second #= First - 1)
		#/\ Mod1 #= First // Dim
		#/\ Mod1 #= Second // Dim)
	#\/ ((Second #= First + Dim #\/ Second #= First - Dim)
		#/\ Mod1 #= First mod Dim 
		#/\ Mod1 #= Second mod Dim
		#/\ Mod2 #= First // Dim2 
		#/\ Mod2 #= Second // Dim2)
	#\/ ((Second #= First + Dim2 #\/ Second #= First - Dim2)
		#/\ Mod1 #= First mod Dim2
		#/\ Mod1 #= Second mod Dim2)
	),
	check_transition(Snake, Dim, Dim2, Positions, 1).
check_transition([3], _, _, _, _).
check_transition([Head | Snake], Dim, Dim2, Positions, N2):-
	N1 is N2 - 1,
	N3 is N2 + 1,
	element(OneT, Positions, N1),
	element(TwoT, Positions, N2),
	element(ThreeT, Positions, N3),
	One #= OneT - 1,
	Two #= TwoT - 1,
	Three #= ThreeT - 1,
	Delta #= Two - One,
	% Constrain Straight
	((Head #= 1 #/\
		Delta #= Three - Two
	)
	% Constrain Corner
	; (Head #= 2 #/\
		((abs(Delta) #= 1 #/\
			% Change line
			(((Three #= Two + Dim #\/ Three #= Two - Dim)
				#/\ Mod1 #= Two mod Dim
				#/\ Mod1 #= Three mod Dim
				#/\ Mod2 #= Two // Dim2 
				#/\ Mod2 #= Three // Dim2)
			% Change face
			#\/ (Three #= Two + Dim2 #\/ Three #= Two - Dim2
				#/\ Mod1 #= Two mod Dim2
				#/\ Mod1 #= Three mod Dim2)))
		#\/ (abs(Delta) #= Dim #/\ 
			% Change column
			(((Three #= Two + 1 #\/ Three #= Two - 1)
				#/\ Mod1 #= Two // Dim
				#/\ Mod1 #= Three // Dim)
			% Change face
			#\/ (Three #= Two + Dim2 #\/ Three #= Two - Dim2
				#/\ Mod1 #= Two mod Dim2
				#/\ Mod1 #= Three mod Dim2)))
		#\/ (abs(Delta) #= Dim2 #/\ 
			% Change column
			(((Three #= Two + 1 #\/ Three #= Two - 1)
				#/\ Mod1 #= Two // Dim
				#/\ Mod1 #= Three // Dim) 
			% Change line
			#\/ ((Three #= Two + Dim #\/ Three #= Two - Dim)
				#/\ Mod1 #= Two mod Dim 
				#/\ Mod1 #= Three mod Dim
				#/\ Mod2 #= Two // Dim2 
				#/\ Mod2 #= Three // Dim2))))
	)),
	!,
	check_transition(Snake, Dim, Dim2, Positions, N3).
