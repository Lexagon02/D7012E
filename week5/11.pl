add(X, L,[X|L]).

% add(4, [1,2,3], R).



del(X, [X| Tail], Tail).
del(X, [Y| Tail], [Y|Tail1]) :- del( X, Tail, Tail1).

% del(t, [r, s, t, u, v, t], R).






insert( X, List, Bigger) :- del( X, Bigger, List).

% insert(4, [1,2,3], R).





member3( X, List) :- del( X, List, _).

% member3(z, [x,y,z,a]).





conc( [], L, L).
conc( [X| L1], L2, [X| L3]) :- conc( L1, L2, L3).

sublist( S, L) :- conc( _, L2, L), conc( S, _, L2).

% sublist( [c, d, e], [a, b, c, d, e]).
% sublist( [c, e], [a, b, c, d, e, f]).






permutation1([],[]).
permutation1([ X| L], P):- 
          permutation1( L, L1), insert( X, L1, P).

% permutation1([6,7,8],R).





permutation2([],[]).
permutation2(L, [ X| P]):- del( X, L, L1), permutation2(L1, P).

% permutation2([6,7,8],R).





ordered([]).
ordered([_]).
ordered([A, B | T]) :- A=<B, ordered([B | T]).

% ordered([1,2,3,4]).
% ordered([1,2,0,4]).





pSort(I,R) :- permutation1(I,R), ordered(R).

% pSort([3,2,4,6,5],R).
% pSort(I,[2,3,4]).





gcd( X, X, X).
gcd( X, Y, D) :- X<Y, Y1 is Y-X, gcd( X, Y1, D).
gcd( X, Y, D) :- Y<X, gcd( Y, X, D).

% gcd(20, 12, D).
% gcd(20, F, 5). 



 

move( state( middle, onbox, middle, hasnot), 
	grasp,                                  
          state( middle, onbox, middle, has) ).    

move( state( P, onfloor, P, H),
           climb, 
           state( P, onbox, P, H) ).   

move( state( P1, onfloor, P1, H),
           push( P1, P2), 
           state( P2, onfloor, P2, H) ).

move( state( P1, onfloor, B, H),
            walk( P1, P2), 
   	 state( P2, onfloor, B, H) ). 


canget( state( _, _, _, has) ).       
canget( State1)  :- move( State1, Move, State2), canget( State2).
canget( state( atdoor, onfloor, atwindow, hasnot)).

%  canget( state( atdoor, onfloor, atwindow, hasnot) ).





parent( pam, bob).       % Pam is a parent of Bob
parent( tom, bob).
parent( tom, liz).
parent( bob, ann).
parent( bob, pat).
parent( pat, jim).


% Figure 2.16   Four versions of the predecessor program.


% Four versions of the predecessor program

% The original version

pred1( X, Z)  :-
   parent( X, Z).

pred1( X, Z)  :-
   parent( X, Y),
   pred1( Y, Z).

% Variation a: swap clauses of the original version

pred2( X, Z)  :-
   parent( X, Y),
   pred2( Y, Z).

pred2( X, Z)  :-
   parent( X, Z).

% Variation b: swap goals in second clause of the original version

pred3( X, Z)  :-
   parent( X, Z).

pred3( X, Z)  :-
   pred3( X, Y),
   parent( Y, Z).

% Variation c: swap goals and clauses of the original version

pred4( X, Z)  :-
   pred4( X, Y),
   parent( Y, Z).

pred4( X, Z)  :-
   parent( X, Z).

