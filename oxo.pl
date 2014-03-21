%%%%%%%%%%%%%%%%%%%%%%%
% Noughts and Crosses %
%%%%%%%%%%%%%%%%%%%%%%%

% A working, but stupid, version of Noughts and Crosses.
% Human goes first, as X, and gives two coordinates of the square they want to take
% computer than takes the next available square, going left to right and then down.

% This file needs two supporting files, io.pl and fill.pl, in the same directory as
% this one. This one should then be consulted.

:- use_module( [library( lists ),
		io,
		fill] ).

% predicates to access different parts of the board, which is represented as a list
%	of 3 lists of lenghth 3. These predicates are used in the fill and io libraries.

row( N, Board, row( N, A, B, C ) ) :-
	nth1( N, Board, [A,B,C] ).

column( N, [Row1,Row2,Row3], col( N, A, B, C ) ) :-
	nth1( N, Row1, A ),
	nth1( N, Row2, B ),
	nth1( N, Row3, C ).

diagonal( top_to_bottom, Board, dia( top_to_bottom, A, B, C )) :-
	row( 1, Board, row( 1, A, _, _ )),
	row( 2, Board, row( 2, _, B, _ )),
	row( 3, Board, row( 3, _, _, C )).
diagonal( bottom_to_top, Board, dia( bottom_to_top, A, B, C )) :-
	row( 1, Board, row( 1, _, _, C )),
	row( 2, Board, row( 2, _, B, _ )),
	row( 3, Board, row( 3, A, _, _ )).

square( X, Y, Board, squ( X, Y, Piece )) :-
	nth1( Y, Board, Column ),
	nth1( X, Column, Piece ).

% predicates to define how to represent and display the players and empty spaces

is_cross( x ).

is_nought( o ).

is_empty( ' ' ).

is_piece( P ) :-
	is_cross( P ).
is_piece( P ) :-
	is_nought( P ).

other_player( P1, P2 ) :-
	is_piece( P1 ),
	is_piece( P2 ),
	\+ P1 = P2.

% predicate to define the initial state of the board

initial_board( [[E,E,E],
		[E,E,E],
		[E,E,E]] ) :-
	is_empty( E ).

% predicate to define the basic data structure of the board, with no values
%	at all in it (not even empty ones)

empty_board( [[_,_,_],[_,_,_],[_,_,_]] ).

% detect a win and return the winning player

and_the_winner_is( Board, Winner ) :-
	is_piece( Winner ),
	row( _, Board, row( _, Winner, Winner, Winner )).
and_the_winner_is( Board, Winner ) :-
	is_piece( Winner ),
	column( _, Board, col( _, Winner, Winner, Winner )).
and_the_winner_is( Board, Winner ) :-
	is_piece( Winner ),
	diagonal( _, Board, dia( _, Winner, Winner, Winner )).

% play the game. This is the predicate to call from the command line.

play :-
	welcome,
	initial_board( Board ),
	display_board( Board ),
	is_cross( Cross ),
	play( Cross, Board ).

play( _Player, Board ) :-
	and_the_winner_is( Board, Winner ),
	report_winner( Winner ),
	!. % this operator is called "cut". It prevents backtracking. Do not use it unless you
	   % know exactly what you are doing. If you introduce more cuts into your answer, you
	   % will lose 10 marks per extra cut.

play( _Player, Board ) :-
	\+ and_the_winner_is( Board, _ ),
	\+ empty_square( _, _, Board ),
	report_stalemate,
	!.

play( Player, Board ) :-
	is_cross( Player ),
	get_legal_move( Player, X, Y, Board ),
	fill_square( X, Y, Player, Board, NewBoard ),
	is_nought( NextPlayer ),
	display_board( NewBoard ),
	play( NextPlayer, NewBoard ),
	!.

play( Player, Board ) :-
	is_nought( Player ),
	choose_move( Player, X, Y, Board ),
	report_move( Player, X, Y ),
	fill_square( X, Y, Player, Board, NewBoard ),
	is_cross( NextPlayer ),
	display_board( NewBoard ),
	play( NextPlayer, NewBoard ),
	!.

% predicate to test for an empty square. (Remember that with uninstantiated X and Y, Prolog 
%	will try to find an empty square.)

empty_square( X, Y, Board ) :-
	is_empty( Empty ),
	square( X, Y, Board, squ( X, Y, Empty )).

% predicate to choose a move. As supplied, it plays in a very stupid way indeed. You will 
%	have to try hard to lose against this.
% THIS PREDICATE IS THE ONE YOU HAVE TO REPLACE FOR YOUR PRACTICAL

choose_move( _Player, X, Y, Board ) :-			% dumbly choose the
	empty_square( X, Y, Board ).			% next space
