%%%  Clausifier

%%%  clausify(FOL, Clause, FreeVars)
%%%
%%%  FOL      ==> FOL expression to be converted to clause form
%%%  Clause   <== clause form of FOL expression
%%%  FreeVars <== free variables in clause

% Universals: variable is left implicitly scoped.
clausify(all(X,F0),F,[X|V]) :- clausify(F0,F,V).

% Implications: consequent must be a literal,
%               antecedent is clausified specially.
clausify(A0=>C0,(C:-A),V) :- clausify_literal(C0,C),
                             clausify_antecedent(A0,A,V).

% Literals: left unchanged (except literal marker is removed).
clausify(C0,C,[]) :- clausify_literal(C0,C).

% Note that conjunctions and existentials are
% disallowed, since they can't form Horn clauses.

%%% clausify_antecedent(FOL, Clause, FreeVars)
%%%
%%%     FOL      ==> FOL expression to be converted to clause form
%%%     Clause   <== clause form of FOL expression
%%%     FreeVars ==> list of free variables in clause

% Literals: left  unchanged (except literal marker is removed).
clausify_antecedent(L0,L,[]) :- clausify_literal(L0,L).

% Conjunctions: each conjunct is clausified separately.
clausify_antecedent(E0&F0, (E,F), V) :-
        clausify_antecedent(E0,E,V0),
        clausify_antecedent(F0,F,V1),
        conc(V0,V1,V).

% Existentials: variable is left implicitly scoped.
clausify_antecedent(exists(X,F0), F, [X|V]) :-
        clausify_antecedent(F0,F,V).

%%%  clausify_literal(Literal, Clause)
%%%
%%%      Literal ==> FOL literal to be converted
%%%                  to clause form
%%%      Clause  <== clause form of FOL expression

% Literal is left unchanged (except literal marker is removed).
clausify_literal(L, L).

conc([], List, List).
conc([Element|Rest], List, [Element|LongRest]) :-
                        conc(Rest, List, LongRest).
