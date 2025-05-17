/*------------------------------------------------------- */
%
%    D7012E Declarative languages
%    Luleå University of Technology
%
%    Student full name: Leo Rydeker Bergström 
%    Student user id  : leoryd-1 
%
/* ------------------------------------------------------- */



%do not chagne the follwoing line!
:- ensure_loaded('play.pl').

:- [testboards].

:-[rndBoard]. 

:- ensure_loaded('stupid.pl').


% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% /* ------------------------------------------------------ */
%               IMPORTANT! PLEASE READ THIS SUMMARY:
%       This files gives you some useful helpers (set &get).
%       Your job is to implement several predicates using
%       these helpers. Feel free to add your own helpers if
%       needed, as long as you write comments (documentation)
%       for all of them. 
%
%       Implement the following predicates at their designated
%       space in this file. You might like to have a look at
%       the file  ttt.pl  to see how the implementations is
%       done for game tic-tac-toe.
%
%          * initialize(InitialState,InitialPlyr).
%          * winner(State,Plyr) 
%          * tie(State)
%          * terminal(State) 
%          * moves(Plyr,State,MvList)
%          * nextState(Plyr,Move,State,NewState,NextPlyr)
%          * validmove(Plyr,State,Proposed)
%          * h(State,Val)  (see question 2 in the handout)
%          * lowerBound(B)
%          * upperBound(B)
% /* ------------------------------------------------------ */







% /* ------------------------------------------------------ */

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% We use the following State Representation: 
% [Row0, Row1 ... Rown] (ours is 6x6 so n = 5 ).
% each Rowi is a LIST of 6 elements '.' or '1' or '2' as follows: 
%    . means the position is  empty
%    1 means player one has a stone in this position
%    2 means player two has a stone in this position. 





% DO NOT CHANGE THE COMMENT BELOW.
%
% given helper: Inital state of the board

initBoard([ [.,.,.,.,.,.], 
            [.,.,.,.,.,.],
	        [.,.,1,2,.,.], 
	        [.,.,2,1,.,.], 
            [.,.,.,.,.,.], 
	        [.,.,.,.,.,.] ]).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%% IMPLEMENT: initialize(...)%%%%%%%%%%%%%%%%%%%%%
%%% Using initBoard define initialize(InitialState,InitialPlyr). 
%%%  holds iff InitialState is the initial state and 
%%%  InitialPlyr is the player who moves first. 

initialize(InitialState,InitialPlyr) :- initBoard(InitialState), InitialPlyr = 1.



% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%winner(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define winner(State,Plyr) here.  
%     - returns winning player if State is a terminal position and
%     Plyr has a higher score than the other player 

% getOtherPlayer(Plyr, OtherPlyr): returns the other player
getOtherPlayer(1, 2).
getOtherPlayer(2, 1).

winner(State, Plyr) :- 
    terminal(State),
    getScore(State, 1, Score1), 
    getScore(State, 2, Score2),
    Score1 \= Score2,
    (
        (Score1 < Score2, Plyr = 1) ;
        (Score2 < Score1, Plyr = 2)
    ). 
	

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%tie(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define tie(State) here. 
%    - true if terminal State is a "tie" (no winner) 

tie(State) :- 
    terminal(State),
    getScore(State, 1, Score1), 
    getScore(State, 2, Score2),
    Score1 = Score2. 



% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%terminal(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define terminal(State). 
%   - true if State is a terminal   

terminal(State) :- 
	% Check if there are no more moves left for both players
	moves(1, State, []), 
	moves(2, State, []).



% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%showState(State)%%%%%%%%%%%%%%%%%%%%%%%%%%
%% given helper. DO NOT  change this. It's used by play.pl
%%

showState( G ) :- 
	printRows( G ). 
 
printRows( [] ). 
printRows( [H|L] ) :- 
	printList(H),
	nl,
	printRows(L). 

printList([]).
printList([H | L]) :-
	write(H),
	write(' '),
	printList(L).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%moves(Plyr,State,MvList)%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define moves(Plyr,State,MvList). 
%   - returns list MvList of all legal moves Plyr can make in State
%

%moves(Plyr, State, MvList) :- 
%	findall(Move, validmove(Plyr, State, Move), MvList). % Find all moves for player Plyr in the given state

% range(I, K, List): List is the list of integers from I to K inclusive.
range(I, I, [I]).
range(I, K, [I|L]) :- I < K, I1 is I + 1, range(I1, K, L).

% perm(N, List, Perm): generates all N-length lists of elements from List (with repetition).
perm(1, Input, [Elem]) :-
    member(Elem, Input).
perm(N, Input, [Elem|Rest]) :-
    N > 1,
    member(Elem, Input),
    N1 is N - 1,
    perm(N1, Input, Rest).

% Remove all empty lists from a list of moves
removeEmpties([], []).
removeEmpties([[]|T], R) :- removeEmpties(T, R).
removeEmpties([H|T], [H|R]) :- H \= [], removeEmpties(T, R).

is_coord([X,Y]) :- integer(X), integer(Y), X >= 0, X < 6, Y >= 0, Y < 6.

filter_coords([], []).
filter_coords([H|T], R) :- is_coord(H), !, filter_coords(T, RT), R = [H|RT].
filter_coords([_|T], R) :- filter_coords(T, R).

moves(Plyr, State, MvList) :-
    length(State, Len),
    Index is Len - 1,
    range(0, Index, RangeList),
    findall(Y, perm(2, RangeList, Y), CoordList),
    calculateTestmoves(Plyr, State, CoordList, [], Moves),
    sort(Moves, Moves2),
    removeEmpties(Moves2, Moves3),
    filter_coords(Moves3, MvList).  % Ensures that the moves are valid coordinates
    %format('DEBUG: Player ~w possible moves: ~w~n', [Plyr, MvList]).

calculateTestmoves(_,_,[],Temp,Temp). 						% For the list of Cordinates run its
calculateTestmoves(Plyr,State,[Head|Tails],Temp,Moves) :- 	% length to find all possible movies.
	testmoves(Plyr,State,Head,SoFar),						% Returns the cordinates if they are valid.
	append(Temp,[SoFar],SoFar2),							% Append the list to total MvList
	calculateTestmoves(Plyr,State,Tails,SoFar2,Moves).

testmoves(Plyr,State,[X,Y],SoFar) :-
	(validmove(Plyr, State, [X,Y]) -> 							   
		SoFar = [X,Y]			     
	;
		SoFar = []
	).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%nextState(Plyr,Move,State,NewState,NextPlyr)%%%%%%%%%%%%%%%%%%%%
%% 
%% define nextState(Plyr,Move,State,NewState,NextPlyr). 
%   - given that Plyr makes Move in State, it determines NewState (i.e. the next 
%     state) and NextPlayer (i.e. the next player who will move).
%

nextState(Plyr, 'n', State, State, NextPlyr) :- getOtherPlayer(Plyr, NextPlyr).
nextState(Plyr, 'null', State, State, NextPlyr) :- getOtherPlayer(Plyr, NextPlyr).
nextState(Plyr, Move, State, NewState, NextPlyr) :- 
    getOtherPlayer(Plyr, NextPlyr),           % Get the other player
    get(State, Move, '.'),                    % Ensure the move is on an empty cell.
    moves(Plyr, State, MoveList),             % Get the list of valid moves.
    member(Move, MoveList),                   % Ensure the move is valid.
    set(State, TempState1, Move, Plyr),       % Place the player's stone on the board.
    findAllFlips(Plyr, TempState1, Move, NewState). % Flip opponent stones.

% Find all flips in 8 directions
findAllFlips(Plyr, State,       Move, Result) :- 
    findFlip(Plyr, State,       Move, [0, -1], TempState1),  % North
    findFlip(Plyr, TempState1,  Move, [0, 1], TempState2),   % South
    findFlip(Plyr, TempState2,  Move, [1, 0], TempState3),   % East
    findFlip(Plyr, TempState3,  Move, [-1, 0], TempState4),  % West
    findFlip(Plyr, TempState4,  Move, [1, -1], TempState5),  % Northeast
    findFlip(Plyr, TempState5,  Move, [-1, -1], TempState6), % Northwest
    findFlip(Plyr, TempState6,  Move, [1, 1], TempState7),   % Southeast
    findFlip(Plyr, TempState7,  Move, [-1, 1], Result).      % Southwest

% Find flips in a specific direction: collect chain, then flip if valid
findFlip(Plyr, State, [X, Y], [DX, DY], Result) :-
    getOtherPlayer(Plyr, Opponent),
    X1 is X + DX, Y1 is Y + DY,
    validCords([X1, Y1]),
    get(State, [X1, Y1], Opponent),
    collect_chain(Plyr, State, [X1, Y1], [DX, DY], Chain),
    Chain \= [],
    flip_chain(State, Chain, Plyr, Result), !.
findFlip(_, State, _, _, State).

% Collect a chain of opponent pieces until you find your own piece
collect_chain(Plyr, State, [X, Y], [DX, DY], [[X, Y]|Rest]) :-
    getOtherPlayer(Plyr, Opponent),
    X1 is X + DX, Y1 is Y + DY,
    validCords([X1, Y1]),
    get(State, [X, Y], Opponent),
    get(State, [X1, Y1], Opponent),
    collect_chain(Plyr, State, [X1, Y1], [DX, DY], Rest).
collect_chain(Plyr, State, [X, Y], [DX, DY], [[X, Y]]) :-
    getOtherPlayer(Plyr, Opponent),
    X1 is X + DX, Y1 is Y + DY,
    validCords([X1, Y1]),
    get(State, [X, Y], Opponent),
    get(State, [X1, Y1], Plyr).
collect_chain(_, State, [X, Y], _, []) :-
    get(State, [X, Y], Elem),
    (Elem = '.' ; Elem = [] ; Elem = nil).

% Flip all positions in the chain
flip_chain(State, [], _, State).
flip_chain(State, [[X, Y]|Rest], Plyr, Result) :-
    set(State, TempState, [X, Y], Plyr),
    flip_chain(TempState, Rest, Plyr, Result).


% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%validmove(Plyr,State,Proposed)%%%%%%%%%%%%%%%%%%%%
%% 
%% define validmove(Plyr,State,Proposed). 
%   - true if Proposed move by Plyr is valid at State.

validmove(Plyr, State, n) :-
    moves(Plyr, State, MvList),
    MvList = [].  % allow 'n' if there are no valid moves

validmove(Plyr, State, null) :-
    moves(Plyr, State, MvList),
    MvList = [].  % Allow 'null' if there are no valid moves

validmove(Plyr, State, Proposed) :- 
    get(State, Proposed, '.'),      % The cell must be empty
    move(Plyr, State, Proposed).    % There must be at least one direction to flip

% Try all 8 directions for a possible flip
% Try all 8 directions for a possible flip
move(Plyr, State, [X, Y]) :- trymove(Plyr, State, [X, Y], [0, -1]). % North
move(Plyr, State, [X, Y]) :- trymove(Plyr, State, [X, Y], [0, 1]).  % South
move(Plyr, State, [X, Y]) :- trymove(Plyr, State, [X, Y], [1, 0]).  % East
move(Plyr, State, [X, Y]) :- trymove(Plyr, State, [X, Y], [-1, 0]). % West
move(Plyr, State, [X, Y]) :- trymove(Plyr, State, [X, Y], [1, -1]). % Northeast
move(Plyr, State, [X, Y]) :- trymove(Plyr, State, [X, Y], [-1, -1]).% Northwest
move(Plyr, State, [X, Y]) :- trymove(Plyr, State, [X, Y], [1, 1]).  % Southeast
move(Plyr, State, [X, Y]) :- trymove(Plyr, State, [X, Y], [-1, 1]). % Southwest

% trymove/4: True if there is a valid flip in the given direction
trymove(Plyr, State, [X,Y], [X2,Y2]) :-
	X3 is X+X2,  % Opponent coordinate
	Y3 is Y+Y2,
	validCords([X3,Y3]),
	X4 is X3+X2, % Player coordinate
	Y4 is Y3+Y2,
	validCords([X4,Y4]),
	getOtherPlayer(Plyr, Opponent),
	get(State, [X3,Y3],Opponent),
	((get(State, [X3,Y3], Opponent)),(get(State,[X4,Y4], Plyr)) -> 
		!
		;
		X5 is X + X2,
		Y5 is Y + Y2,
		get(State,[X5,Y5],Opponent),
		trymove(Plyr,State,[X5,Y5],[X2,Y2])
	).


% Valid coordinates for a 6x6 board
validCords([X,Y]) :- X >= 0, X < 6, Y >= 0, Y < 6.

% getScore(State, Plyr, Score): counts how many Plyr's pieces are on the board.
getScore(State, Plyr, Score) :-
    countStones(State, Plyr, 0, Score).

% Helper to count stones recursively
countStones([], _, Acc, Acc).
countStones([Row|Rest], Plyr, Acc, Score) :-
    countInRow(Row, Plyr, RowCount),
    NewAcc is Acc + RowCount,
    countStones(Rest, Plyr, NewAcc, Score).

% Count Plyr's stones in a single row
countInRow([], _, 0).
countInRow([Plyr|T], Plyr, N) :-
    countInRow(T, Plyr, N1),
    N is N1 + 1.
countInRow([Other|T], Plyr, N) :-
    Other \= Plyr,
    countInRow(T, Plyr, N).


% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%h(State,Val)%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define h(State,Val). 
%   - given State, returns heuristic Val of that state
%   - larger values are good for Max, smaller values are good for Min
%   NOTE1. If State is terminal h should return its true value.
%   NOTE2. If State is not terminal h should be an estimate of
%          the value of state (see handout on ideas about
%          good heuristics.


% Large values for terminal states
h(State, 500) :- winner(State, 2), !.
h(State, -500) :- winner(State, 1), !.
h(State, 0) :- tie(State), !.

h(State, Val) :-
    getScore(State, 1, Score1),
    getScore(State, 2, Score2),
    emptyCount(State, Empty),
    cornerPenalty(State, CornerPenalty),
    % Only start caring about disc difference in the endgame
    (Empty < 12 -> DiscScore is (Score1 - Score2) ; DiscScore is 0),
    % Parity bonus: prefer even number of empty squares
    (0 is Empty mod 2 -> ParityBonus = 10 ; ParityBonus = 0),
    Val is (-DiscScore + 15*CornerPenalty - ParityBonus).

% Count empty squares
emptyCount(State, Count) :-
    flatten(State, Flat),
    include(=(.), Flat, Empties),
    length(Empties, Count).

% Penalize for owning corners
cornerPenalty(State, Penalty) :-
    findall([X,Y], member([X,Y], [[0,0],[0,5],[5,0],[5,5]]), Corners),
    findall(1, (member([X,Y], Corners), get(State, [X,Y], 2)), L),
    length(L, Penalty).




% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%lowerBound(B)%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define lowerBound(B).  
%   - returns a value B that is less than the actual or heuristic value
%     of all states.

lowerBound(B) :- B = -10000.


% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%upperBound(B)%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define upperBound(B). 
%   - returns a value B that is greater than the actual or heuristic value
%     of all states.

upperBound(B) :- B = 10000.


% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                       %
%                                                                       %
%                Given   UTILITIES                                      %
%                   do NOT change these!                                %
%                                                                       %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% get(Board, Point, Element)
%    : get the contents of the board at position column X and row Y
% set(Board, NewBoard, [X, Y], Value):
%    : set Value at column X row Y in Board and bind resulting grid to NewBoard
%
% The origin of the board is in the upper left corner with an index of
% [0,0], the upper right hand corner has index [5,0], the lower left
% hand corner has index [0,5], the lower right hand corner has index
% [5,5] (on a 6x6 board).
%
% Example
% ?- initBoard(B), showState(B), get(B, [2,3], Value). 
%. . . . . . 
%. . . . . . 
%. . 1 2 . . 
%. . 2 1 . . 
%. . . . . . 
%. . . . . . 
%
%B = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], 
%     ['.', '.', 1, 2, '.', '.'], ['.', '.', 2, 1, '.'|...], 
%     ['.', '.', '.', '.'|...], ['.', '.', '.'|...]]
%Value = 2 
%Yes
%?- 
%
% Setting values on the board
% ?- initBoard(B),  showState(B),set(B, NB1, [2,4], 1),
%         set(NB1, NB2, [2,3], 1),  showState(NB2). 
%
% . . . . . . 
% . . . . . . 
% . . 1 2 . . 
% . . 2 1 . . 
% . . . . . . 
% . . . . . .
% 
% . . . . . . 
% . . . . . . 
% . . 1 2 . . 
% . . 1 1 . . 
% . . 1 . . . 
% . . . . . .
%
%B = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], ['.', '.', 
%1, 2, '.', '.'], ['.', '.', 2, 1, '.'|...], ['.', '.', '.', '.'|...], ['.', '.',
% '.'|...]]
%NB1 = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], ['.', '.'
%, 1, 2, '.', '.'], ['.', '.', 2, 1, '.'|...], ['.', '.', 1, '.'|...], ['.', '.
%', '.'|...]]
%NB2 = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], ['.', '.'
%, 1, 2, '.', '.'], ['.', '.', 1, 1, '.'|...], ['.', '.', 1, '.'|...], ['.', 
%'.', '.'|...]]

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% get(Board, Point, Element): get the value of the board at position
% column X and row Y (indexing starts at 0).
% Do not change get:

get( Board, [X, Y], Value) :- 
	nth0( Y, Board, ListY), 
	nth0( X, ListY, Value).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% set( Board, NewBoard, [X, Y], Value): set the value of the board at position
% column X and row Y to Value (indexing starts at 0). Returns the new board as
% NewBoard. Do not change set:

set( [Row|RestRows], [NewRow|RestRows], [X, 0], Value) :-
    setInList(Row, NewRow, X, Value). 

set( [Row|RestRows], [Row|NewRestRows], [X, Y], Value) :-
    Y > 0, 
    Y1 is Y-1, 
    set( RestRows, NewRestRows, [X, Y1], Value). 

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% setInList( List, NewList, Index, Value): given helper to set. Do not
% change setInList:

setInList( [_|RestList], [Value|RestList], 0, Value). 

setInList( [Element|RestList], [Element|NewRestList], Index, Value) :- 
	Index > 0, 
	Index1 is Index-1, 
	setInList( RestList, NewRestList, Index1, Value). 
 
