% Leo Rydeker Bergström         leoryd-1

% List of moves in optimal order
% 1. Pick up the steel key
% 2. Move to room2
% 3. Drop pick up brass key
% 4. Move to room1
% 5. Drop steel key
% 6. move to room3
% 7. Pick up package
% 8. Move to room1
% 9. drop brass key
% 10. pick up steel key
% 11. Move to room2
% 12. Drop package


% Move between rooms (requires keys)
move(state(SK, has, Pa, Items, r1), walk(r1, r3), state(SK, has, Pa, Items, r3)). % Move to room3 from room1
move(state(SK, has, Pa, Items, r3), walk(r3, r1), state(SK, has, Pa, Items, r1)). % Move to room1 from room3
move(state(has, BK, Pa, Items, r1), walk(r1, r2), state(has, BK, Pa, Items, r2)). % Move to room2 from room1
move(state(has, BK, Pa, Items, r2), walk(r2, r1), state(has, BK, Pa, Items, r1)). % Move to room1 from room2

% Pick up items (enforce two-item limit)
move(state(Room, BK, Pa, Items, Room), grasp(steelKey, Room), state(has, BK, Pa, Items2, Room)) :-
    Items < 2, Items2 is Items + 1. % Pick up steel key
move(state(SK, Room, Pa, Items, Room), grasp(brassKey, Room), state(SK, has, Pa, Items2, Room)) :-
    Items < 2, Items2 is Items + 1. % Pick up brass key
move(state(SK, BK, Room, Items, Room), grasp(package, Room), state(SK, BK, has, Items2, Room)) :-
    Items < 2, Items2 is Items + 1. % Pick up package

% Drop items (only if holding an item)
move(state(has, BK, Pa, Items, Room), drop(steelKey, Room), state(Room, BK, Pa, Items2, Room)) :-
    Items2 is Items - 1, Items > 0. % Drop steel key
move(state(SK, has, Pa, Items, Room), drop(brassKey, Room), state(SK, Room, Pa, Items2, Room)) :-
    Items2 is Items - 1, Items > 0. % Drop brass key
move(state(SK, BK, has, Items, Room), drop(package, Room), state(SK, BK, Room, Items2, Room)) :-
    Items2 is Items - 1, Items > 0. % Drop package

% Base case: Goal state is reached
solve_robot(state(_, _, has, _, r2), _, [done|[]]). 
solve_robot(state(_, _, r2, _, r2), _, [done|[]]). 

% Recursive case: Try a move and continue searching
solve_robot(State1, N, [Move|Trace]) :-
    N > 0,
    move(State1, Move, State2),
    N2 is N - 1,
    solve_robot(State2, N2, Trace).

% Start the search
start(Trace, N) :-
    integer(N), % Ensure N is instantiated
    solve_robot(state(r1, r2, r3, 0, r1), N, Trace).