/*
  State is a list [C, M, Ship]
  C is the number of cannibals on the right side
  M is the number of missionaries on the left side
  Ship is the position of the ship (1 is right, 0 is left).
*/


move([C,M,1], [NC, NM, Ship]) :-
  (
    (NC is C - 1, NM=M);   % move 1 cannibal to the left
    (NC is C - 2, NM=M);   % move 2 cannibals to the left
    (NM is M - 1, NC=C);   % move 1 missionary to the left
    (NM is M - 2, NC=C);   % move 2 missionaries to the left
    (NM is M - 1, NC is C - 1) % move 1 cannibal and 1 missionary to the left
  ),
  Ship=0. % change the position of the ship to the left

move([C, M, 0], [NC, NM, Ship]) :-
  (
    (NC is C + 1, NM=M);  % move 1 cannibal to the right
    (NC is C + 2, NM=M);   % move 2 cannibals to the right
    (NM is M + 1, NC=C);   % move 1 missionary to the right
    (NM is M + 2, NC=C);   % move 2 missionaries to the right
    (NM is M + 1, NC is C + 1) % move 1 cannibal and 1 missionary to the right
  ),
  Ship=1. % change the position of the ship to the right

safe([C, M, _]) :-
  C >= 0,
  M >= 0,
  C < 4,
  M < 4,
  LC is 3 - C,
  LM is 3 - M,
  (M >= C ; M=0), % (missionaries on the right >= cannibals on the right) OR (there are no missionaries on the right)
  (LM >= LC ; LM=0). % (missionaries on the left >= cannibals on the left) OR (there are no missionaries on the left side)

play(Movements) :-
  play([3,3,1], [ [3,3,1] ], Movements).

play(State, _, L) :-
  State = [0,0,_],
  L = [].

play(State, Visited, [H | T]) :- % can this be improved to use only 1 list (visited and movements)
  move(State, NewState),
  not(member(NewState, Visited)),
  safe(NewState),
  append(Visited, [NewState], NewVisited),
  play(NewState, NewVisited, T),
  H = NewState.
