% Leo Rydeker Bergström

% Goal state: Package is delivered to room2
goal_state(state(room2, _, _, room2)).

% Move between rooms
doMove(state(room1, SteelKey, BrassKey, Package), state(room2, SteelKey, BrassKey, Package), _, _, move(room1, room2)).
doMove(state(room2, SteelKey, BrassKey, Package), state(room1, SteelKey, BrassKey, Package), _, _, move(room2, room1)).
doMove(state(room2, SteelKey, BrassKey, Package), state(room3, SteelKey, BrassKey, Package), _, _, move(room2, room3)).
doMove(state(room3, SteelKey, BrassKey, Package), state(room2, SteelKey, BrassKey, Package), _, _, move(room3, room2)).

% Pick up items
doMove(state(Room, Room, BrassKey, Package), state(Room, robot, BrassKey, Package), steel, _, pick_up(steel)).
doMove(state(Room, SteelKey, Room, Package), state(Room, SteelKey, robot, Package), brass, _, pick_up(brass)).
doMove(state(Room, SteelKey, BrassKey, Room), state(Room, SteelKey, BrassKey, robot), package, _, pick_up(package)).

% Drop items
doMove(state(Room, robot, BrassKey, Package), state(Room, Room, BrassKey, Package), steel, _, drop(steel)).
doMove(state(Room, SteelKey, robot, Package), state(Room, SteelKey, Room, Package), brass, _, drop(brass)).
doMove(state(Room, SteelKey, BrassKey, robot), state(Room, SteelKey, BrassKey, Room), package, _, drop(package)).

% Base case: Goal state is reached
solveR(State, _, []) :-
    goal_state(State).

% Recursive case: Try a move and continue searching
solveR(State, N, [Move|Trace]) :-
    N > 0,
    doMove(State, NewState, _, _, Move), % Replace Item1 and Item2 with `_`
    N1 is N - 1,
    solveR(NewState, N1, Trace).

% Start the search
start(Trace, N) :-
    InitialState = state(room1, room1, room2, room3), % Robot starts in room1, items are in their respective rooms
    solveR(InitialState, N, Trace).

% start(Trace, 5).