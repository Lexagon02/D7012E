% Figure 1.8   The family program.

parent( pam, bob).   % 1 (Pam is a parent of Bob)
parent( tom, bob).   % 2
parent( tom, liz).   % 3
parent( bob, ann).   % 4
parent( bob, pat).   % 5
parent( pat, jim).   % 6

female( pam).            % Pam is female
female( liz).
female( ann).
female( pat).
male( tom).              % Tom is male
male( bob).
male( jim).

offspring( Y, X)  :-     % Y is an offspring of X if
   parent( X, Y).        % X is a parent of Y

mother( X, Y)  :-        % X is the mother of Y if
   parent( X, Y),        % X is a parent of Y and
   female( X).           % X is female

grandparent( X, Z)  :-   % X is a grandparent of Z if
   parent( X, Y),        % X is a parent of Y and
   parent( Y, Z).        % Y is a parent of Z

different(X,Y) :- \+(X=Y).

sister( X, Y)  :-        % X is a sister of Y if
   parent( Z, X),
   parent( Z, Y),        % X and Y have the same parent and
   female( X),           % X is female and
   different( X, Y).     % X and Y are different

predecessor( X, Z)  :-   % Rule prl: X is a predecessor of Z
   parent( X, Z).

predecessor( X, Z)  :-   % Rule pr2: X is a predecessor of Z
   parent( X, Y),
   predecessor( Y, Z).






member1( X, [X| Tail]).
member1( X, [Head| Tail]) :-  member1( X, Tail).



member1b( X, [X| _]).
member1b( X, [_| Tail]) :-  member1( X, Tail).



conc( [], L, L).
conc( [X| L1], L2, [X| L3]) :- conc( L1, L2, L3).



member2b(X, L):- conc(L1,[X|L2],L).


member2c(X, L):- conc(_,[X|_],L).




length1( [], 0).
length1( [ _| Tail], N) :- length1( Tail, N1), 
                           N is 1 + N1.



length2( [], 0).
length2( [_ | Tail], N) :- length2( Tail, N1), 
                           N = 1 + N1.
