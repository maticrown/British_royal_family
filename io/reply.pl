
%%% reply(Type, FreeVars, Clause, Reply)
%%%
%%%     Type    ==>     the constant "query" or "assertion"
%%%                            depending on whether clause should
%%%                            be interpreted as a query or assertion
%%%     FreeVars        ==>     the free variables (to be
%%%                            interpreted existentially) in the clause
%%% Clause ==> the clause being replied to
%%% Reply  <== the reply
%%%
%%% If the clause is interpreted as an assertion,
%%% the predicate has a side effect of asserting
%%% the clause to the database.

%Replying to a query.
reply(query, FreeVars,
     (answer(Answer):-Condition), Reply) :-
   % find all the answers that satisfy the query,
   % replying with that set if it exists, or "no"
   % or "none" if it doesn't.
     (setof(Answer, FreeVars^Condition, Answers)
         -> Reply = answer(Answers)
         ;  (Answer = yes
               -> Reply = answer([no])
               ;  Reply = answer([none]))), !.

% Replying to an assertion.
% assert the assertion and tell user what we asserted
reply(assertion, _FreeVars, Assertion, asserted(Assertion)) :-
        assert(Assertion), !.

% Replying to some other type of sentence.
reply(_Type, _FreeVars, _Clause, error('unknown type')).


%%%
reply(retraction, _FreeVars, retraction, retracted(Retraction)) :-
        retract(Retraction), !.
%%%

