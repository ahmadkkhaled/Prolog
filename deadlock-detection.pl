:- dynamic available_resource/2.

process(p1).
process(p2).
process(p3).
process(p4).

% available_resources([[r1, 0], [r2, 0]]).

available_resource(r1, 0).
available_resource(r2, 0).

allocated(p1, r2).
allocated(p2, r1).
allocated(p3, r1).
allocated(p4, r2).

requested(p1, r1).
requested(p3, r2).

%=================================================================

safe_state() :-
	safe_state(X, []).

safe_state(_, LFinished) :-
	findall(X, process(X), LProcesses),  % fetch list of all processes
	length(LProcesses, Size_LProcesses), % get size of list of all processes
	length(LFinished, Size_LFinished),   % get size of list of processes that finished executing
	Size_LFinished = Size_LProcesses.    % number of processes that finished executing = total number of processes
	%print_list(LFinished),
	%!.

safe_state(X, LFinished) :-
	process(X),
	not(member(X, LFinished)),
	can_finish(X).

can_finish(X) :-
	not(requested(X,_)) ; not(cannot_allocate_resource(X)) ; not(requested_not_allocated(X)).
	% true if X has no requests,
	% or there exists no resource R that X requests that has 0 instances (can satisfy all X requests)
	% or there exists no resource R that X requests and hasn't been allocated to X (all X requests have already been satisfied)

cannot_allocate_resource(X) :- % true if at least 1 resource that X requests has 0 instances.
	requested(X, R),
	available_resource(R, Instances),
	Instances = 0,
	!.
requested_not_allocated(X) :- % true if X has at least 1 request of type R but R hasn't been allocated to X.
	requested(X, R),
	not(allocated(X, R)).
