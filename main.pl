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

dragons_tail2([3,1,2,2,2,1,2,2,1,2,2,2,1,2,1,2,2,2,2,1,2,1,2,1,2,1,3]).

/**
	Cube values are 1..3, representing Straights, Corners, and Ends.
	Positions is a list of values that represent the order ono wich the cube will be visited.
*/
snake_cube(Snake, Positions):-
	length(Snake, NCubes),
	Dim is round(NCubes ** (1/3)),
	Dim2 is round(Dim ** 2),
	LastPosition is NCubes - 1,

	nth1(1, Snake, 3),
	nth1(NCubes, Snake, 3),

	length(Positions, NCubes),
	domain(Positions, 0, LastPosition),
	all_distinct(Positions),
	check_transition(Snake, Dim, Dim2, Positions),
	labeling([], Positions).

/**
	Para mudar de linha tem que estar na mesma coluna e mesma face
	Para mudar de coluna tem que estar na mesma linha e mesma face
	Para mudar de face tem que estar na mesma linha e mesma coluna
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
		%#/\ Mod1 #= First // Dim
		%#/\ Mod1 #= Second // Dim
		#/\ Mod1 #= First mod Dim2
		#/\ Mod1 #= Second mod Dim2)
	),
	check_transition(Snake, Dim, Dim2, Positions, FirstT, SecondT).
check_transition([3], _, _, _, _, _).
check_transition([Head | Snake], Dim, Dim2, Positions, OneT, TwoT):-
	element(OneT, Positions, N1),
	element(TwoT, Positions, N2),
	element(ThreeT, Positions, N3),
	One #= OneT - 1,
	Two #= TwoT - 1,
	Three #= ThreeT - 1,
	N2 #= N1 + 1,
	N3 #= N2 + 1,
	Delta #= Two - One,
	% Constrain Straight
	((Head #= 1 #/\
		Three #= Two + Delta #/\
		((abs(Delta) #= 1 #/\
			% Change column
			(Three #= Two + 1 #\/ Three #= Two - 1)
				#/\ Mod1 #= Two // Dim
				#/\ Mod1 #= Three // Dim)
		#\/ (abs(Delta) #= Dim #/\
			% Change line
			(Three #= Two + Dim #\/ Three #= Two - Dim)
				#/\ Mod1 #= Two mod Dim
				#/\ Mod1 #= Three mod Dim
				#/\ Mod2 #= Two // Dim2 
				#/\ Mod2 #= Three // Dim2)
		#\/ (abs(Delta) #= Dim2 #/\
			% Change face
			(Three #= Two + Dim2 #\/ Three #= Two - Dim2)
				%#/\ Mod1 #= Two // Dim
				%#/\ Mod1 #= Three // Dim
				#/\ Mod1 #= Two mod Dim2
				#/\ Mod1 #= Three mod Dim2))
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
				%#/\ Mod1 #= Two // Dim
				%#/\ Mod1 #= Three // Dim
				#/\ Mod1 #= Two mod Dim2
				#/\ Mod1 #= Three mod Dim2)))
		#\/ (abs(Delta) #= Dim #/\ 
			% Change column
			(((Three #= Two + 1 #\/ Three #= Two - 1)
				#/\ Mod1 #= Two // Dim
				#/\ Mod1 #= Three // Dim)
			% Change face
			#\/ (Three #= Two + Dim2 #\/ Three #= Two - Dim2
				%#/\ Mod1 #= Two // Dim
				%#/\ Mod1 #= Three // Dim
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
	check_transition(Snake, Dim, Dim2, Positions, TwoT, ThreeT).
