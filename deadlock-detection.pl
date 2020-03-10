process(p1).
process(p2).
process(p3).
process(p4).

% available_resources([[r1, 0], [r2, 0]]). % passed as input.


allocated(p1, r2).
allocated(p2, r1).
allocated(p3, r1).
allocated(p4, r2).

requested(p1, r1).
requested(p3, r2).

%=================================================================

safe_state(Sequence) :-
	safe_state([], [ [r1, 0], [r2, 0] ], Sequence).

safe_state(LFinished, _, Sequence) :-
	findall(P, process(P), LProcesses),  % fetch list of all processes
	length(LProcesses, Size_LProcesses), % get size of list of all processes
	length(LFinished, Size_LFinished),
	Size_LFinished = Size_LProcesses,    % number of processes that finished executing = total number of processes
	Sequence = [],
	!.

safe_state(LFinished, LAvailable_Resources, [HSequence | TSequence]) :-
	process(P),
	not(member(P, LFinished)),
	can_finish(P, LAvailable_Resources),
	release(P, LAvailable_Resources, New_LAvailable_Resources),
	append(LFinished, [P], New_LFinished),
	HSequence = P,
	safe_state(New_LFinished, New_LAvailable_Resources, TSequence).

can_finish(P, LAvailable_Resources) :-
	not(requested(P,_));
	not(cannot_allocate_resource(P, LAvailable_Resources));
	not(not_satisfied(P)).

cannot_allocate_resource(P, LAvailable_Resources) :- % true if at least 1 resource that P requests has 0 instances.
	requested(P, R),
	get_instances(R, LAvailable_Resources, Instances),
	Instances = 0,
	!.

% Given a resource of type R
% iterates through a list of resources L = [ [r_i, instances_i] , [r_j, instances_j] , .... [r_n, instances_n]]
% and returns the instances of R.
get_instances(R, [ [R_i, Instances_i] | _], Instances) :-
	R = R_i,
	Instances = Instances_i,
	!.

get_instances(R, [_ | T], Instances) :-
	get_instances(R, T, Instances).


not_satisfied(P) :- % true if P has at least 1 request of type R but R hasn't been allocated to P.
	requested(P, R),
	not(allocated(P, R)).

release(P, LAvailable_Resources, New_LAvailable_Resources) :-
	findall(R, allocated(P, R), LResources),  % fetch list of all resources allocated to process P
	deallocate(LResources, LAvailable_Resources, New_LAvailable_Resources).


% Given a list of resources [r1, r2, r3, ... rn] LResources
% and a list of the current available instances of resources [ [r1,i1] , [r2, i2], ... [rn, in] ] LAvailable_Resources
% Initializes New_LAvailable_Resources where for each resource r(i) in LResources, the number of instances in LAvailable_Resources is incremented by 1.
deallocate(LResources, LAvailable_Resources, New_LAvailable_Resources) :-
	LResources = [],
	New_LAvailable_Resources = LAvailable_Resources.

deallocate([HR | TR], LAvailable_Resources, New_LAvailable_Resources) :-
	get_instances(HR, LAvailable_Resources, Instances),
	delete(LAvailable_Resources, [HR, Instances], Erased_LAvilable_Resources),
	New_Instances is Instances + 1,
	append(Erased_LAvilable_Resources, [ [HR, New_Instances] ], Updated_LAvailable_Resources),
	deallocate(TR, Updated_LAvailable_Resources, New_LAvailable_Resources).
