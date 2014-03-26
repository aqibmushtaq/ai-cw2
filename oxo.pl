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

% choose_move( _Player, X, Y, Board ) :-			% dumbly choose the
% 	empty_square( X, Y, Board ).			% next space

line(N, Board, [A,B,C]):-
  row(N, Board, row(N, A,B,C));
  column(N, Board, col(N, A,B,C));
  diagonal(N, Board, dia(N, A,B,C)).

utility(Board, Value):-
  findall([A,B,C], line(_, Board, [A,B,C]), Lines),
  writer(Lines, x, 0, Xtotal),
  writer(Lines, o, 0, Ototal),
  Value is Xtotal - Ototal.

writer([],_, Sum, Total):-
  Total is Sum.
writer([Head|Tail],X, Sum, Total):-
  count(Head, X, Out),
  sum(Out, Result),
  NewSum is Sum + Result,
  writer(Tail,X, NewSum, Total).

sum(0, Result):-
  Result is 0.
sum(1, Result):-
  Result is 0.
sum(2,Result):-
  Result is 1.
sum(3,Result):-
  Result is 10.

count([],_,0).
count([X|T],X,Y):- count(T,X,Z), Y is 1+Z.
count([X1|T],X,Z):- X1\=X,count(T,X,Z).

% Get the positions at which the element occurs in the list
indexOf([Element|_], Element, 1). % We found the element
indexOf([_|Tail], Element, Index):-
    indexOf(Tail, Element, Index1), % Check in the tail of the list
    Index is Index1+1.  % and increment the resulting index

% Get the positions where the row contains empty squares
get_empty_spaces([A,B,C], [X,Y,Z]):-
    findall(X, indexOf(A, ' ', X), X),
    findall(Y, indexOf(B, ' ', Y), Y),
    findall(Z, indexOf(C, ' ', Z), Z).

% Create the board using various X-values 
make_boards(InitBoard, [], Y, []).  % Base case
make_boards(InitBoard, [X1|T], Y, [X1,Y,FinalBoard|FinalBoards]):-
    make_boards(InitBoard, T, Y, FinalBoards),  % Create the next board
	fill_square( X1, Y, o, InitBoard, FinalBoard).  % Create this board and add it to the list

% Get all combinations for the current move
get_all_boards(Board, Boards):-
    get_empty_spaces(Board, [X,Y,Z]),
    make_boards(Board, X, 1, A),
    % write('BOARD A'),write(A),nl,nl,
    make_boards(Board, Y, 2, B),
    % write('BOARD B'),write(B),nl,nl,
    make_boards(Board, Z, 3, C),
    % write('BOARD C'),write(C),nl,nl,
    append(A, B, BoardsAB),
    append(BoardsAB, C, Boards).
    % write('ALL BOARDS'),write(Boards),nl,nl.

get_best_move([], _, _, -100).
get_best_move([X,Y,Board|T], BestX, BestY, BestUtility):-
    % write('GETTING BEST MOVE x:'),write(X),write(' y:'),write(Y),nl,nl,
    get_best_move(T, BestX2, BestY2, BestUtility2),
    % write('BELOW get_best_move... BestUtility2:'),write(BestUtility2),nl,nl,
    utility(Board, Utility),
    % write('UTILITY:'),write(Utility),nl,nl,
    max([BestUtility2,BestX2,BestY2], [Utility,X,Y], [BestUtility,BestX,BestY]).
    % write('GOT MAX:'),write(BestUtility),nl,nl,
    % write(BestUtility),nl,write(BestX),nl,write(BestY),nl,nl.

choose_move( _Player, X, Y, Board ) :-
    % write('Player:'),write(_Player),nl,write('Board:'),write(Board),nl,nl,
    get_all_boards(Board, NewBoards),
    % write('GOT ALL BOARDS'),nl,nl,write(NewBoards),nl,nl,
    get_best_move(NewBoards, X, Y, BestUtility).
    % write(BestUtility).

max([A,AX,AY], [B,BX,BY], [A,AX,AY]) :- A >= B.
max([A,AX,AY], [B,BX,BY], [B,BX,BY]) :- B > A.
