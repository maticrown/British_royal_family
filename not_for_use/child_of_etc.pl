

successionList(X, SuccessionList):-
	findall(Y, successor(X, Y), SuccessionList).

precedes(X,Y):- male(X), male(Y), older_than(X,Y).
%precedes(X,Y):- male(X), female(Y), Y\=elizabeth.
precedes(X,Y):- female(X), female(Y), older_than(X,Y).

%% Sorting algorithm
succession_sort([A|B], Sorted) :- succession_sort(B, SortedTail), insert(A, SortedTail, Sorted).
succession_sort([], []).

insert(A, [B|C], [B|D]) :- not(precedes(A,B)), !, insert(A, C, D).
insert(A, C, [A|C]).


older_than('Prince Charles', 'Princess Anne').
older_than('Prince Charles', 'Prince Andrew').
older_than('Prince Charles', 'Prince Edward').
older_than('Princess Anne', 'Prince Andrew').
older_than('Princess Anne', 'Prince Edward').
older_than('Prince Andrew', 'Prince Edward').

successor(X, Y):- child_of(Y,X).


successionListIndependent(X, SuccessionList):-
	findall(Y, child_of(Y,X), Children),
	succession_sort(Children, SuccessionList).

child_of('Prince Charles', 'Queen Elizabeth II').
child_of('Princess Anne', 'Queen Elizabeth II').
child_of('Prince Andrew', 'Queen Elizabeth II').
child_of('Prince Edward', 'Queen Elizabeth II').


eldest(X,Year):-
    alive(X,Year,B),
    \+((alive(_,Year,T), T<B)).

%alive during the period given
alive(X, Year, B) :- born(X, B), B =< Year, \+ (died(X, D), D < Year).

possibleSuccessor(Year, X):-
    alive(X, Year, _).


actualSuccessor(Year):-
    possibleSuccessor(Year,X),
    eldest(X,Year).

%oldest(P):-not(monarch('Prince Philip')),!.
oldest(P):-
  aggregate(max(A,Pers),age(Pers,A),max(_,P)).


%right here
real_successor(X,Y) :- parent(X,Z), (Y=Z ; successor(Z,Y)).


/*commits(j,l).
commits(x,y).

guilty(X) :-
    commits(X,Y),
    crime(Y).
crime(murder).
crime(theft).


commits(Person, Crime) :-
    crime(Crime),
    format('is ~w ?', [commits(Person, Crime)]),
    read(yes).
*/



