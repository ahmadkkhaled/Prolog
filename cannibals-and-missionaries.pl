% [C1, C2, C3, M1, M2, M3, B] Ci denotes i-th Cannibal, Mi denotes i-th missionary, and B denotes Board using which the movement from side to side is possible


move([1, C2, C3, M1, M2, M3, 1], [0, C2, C3, M1, M2, M3, 0]).
move([0, C2, C3, M1, M2, M3, 0], [1, C2, C3, M1, M2, M3, 1]).

move([C1, 1, C3, M1, M2, M3, 1], [C1, 0, C3, M1, M2, M3, 0]).
move([C1, 0, C3, M1, M2, M3, 0], [C1, 1, C3, M1, M2, M3, 1]).

move([C1, C2, 1, M1, M2, M3, 1], [C1, C2, 0, M1, M2, M3, 0]).
move([C1, C2, 0, M1, M2, M3, 0], [C1, C2, 1, M1, M2, M3, 1]).

move([C1, C2, C3, 1, M2, M3, 1], [C1, C2, C3, 0, M2, M3, 0]).
move([C1, C2, C3, 0, M2, M3, 0], [C1, C2, C3, 1, M2, M3, 1]).

move([C1, C2, C3, M1, 1, M3, 1], [C1, C2, C3, M1, 0, M3, 0]).
move([C1, C2, C3, M1, 0, M3, 0], [C1, C2, C3, M1, 1, M3, 1]).

move([C1, C2, C3, M1, M2, 1, 1], [C1, C2, C3, M1, M2, 0, 0]).
move([C1, C2, C3, M1, M2, 0, 0], [C1, C2, C3, M1, M2, 1, 1]).

% unsafe([1, 1, 1, 1, 1, 0, _]).
% unsafe([1, 1, 1, 0, 1, 1, _]).
% unsafe([1, 1, 1, 1, 0, 1, _]).
%
% unsafe([1, _, 1, 1, 0, 0, _]).
% unsafe([_, 1, 1, 0, 1, 0, _]).
% unsafe([1, 1, _, 0, 0, 1, _]).
%
%
% unsafe([0, 0, 0, 0, 0, 1, _]).
% unsafe([0, 0, 0, 1, 0, 0, _]).
% unsafe([0, 0, 0, 0, 1, 0, _]).
%
% unsafe([0, _, 0, 0, 1, 1, _]).
% unsafe([_, 0, 0, 0, 1, 1, _]).

unsafe(State) :-
  count_left_cannibals(State, LC),
  RC is 3-LC,

  delete_cannibals(State, NewState),

  count_left_missionaries(NewState, LM),
  RM is 3-LM,
  !,
  (((RC > RM) , not(RM = 0)) ; ((LC > LM) , not(LM = 0))).

% play(Movements) :-
%   play([1,1,1,1,1,1,1], [ [[1,1,1,1,1,1,1]] | T ], Movements).

% number of cannibals currently on the left side (Ci = 0)
count_left_cannibals([_, _, _, _], C) :-
  C is 0.
count_left_cannibals([H | T], C) :-
  count_left_cannibals(T, NewC),
  (H = 0 -> C is NewC + 1 ; C = NewC).

% number of missionaries currently of the left side (Mi = 0)
count_left_missionaries([_], C) :-
  C is 0.
count_left_missionaries([H | T], C) :-
  count_left_missionaries(T, NewC),
  (H = 0 -> C is NewC + 1 ; C = NewC).


% removing cannibals from state list
delete_cannibals([M1,M2,M3,B], NL) :-
  NL = [M1, M2, M3, B].
delete_cannibals([_ | T] , NL) :-
  delete_cannibals(T, NL).
