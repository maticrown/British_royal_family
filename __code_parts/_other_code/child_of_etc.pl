

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


older_than(charles, anne).
older_than(charles, andrew).
older_than(charles, edward).
older_than(anne, andrew).
older_than(anne, edward).
older_than(andrew, edward).

successor(X, Y):- child_of(Y,X).


successionListIndependent(X, SuccessionList):-
	findall(Y, child_of(Y,X), Children),
	succession_sort(Children, SuccessionList).

child_of(charles, elizabethII).
child_of(anne, elizabethII).
child_of(andrew, elizabethII).
child_of(edward, elizabethII).


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

%oldest(P):-not(monarch(philip)),!.
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



