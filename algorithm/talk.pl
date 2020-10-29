
%%% talk(Sentence, Reply)
%%%
%%%     Sentence ==> sentence to form a reply to
%%%     Reply    <== appropriate reply to the sentence

%%% The main parsing function
talk(Sentence, Reply) :-                  % parse the sentence
         parse(Sentence, LF, Type),
       % convert the FOL logical form into a Horn
       % clause, if possible
         clausify(LF, Clause, FreeVars), !,
       % concoct a reply, based on the clause and
       % whether sentence was a query or assertion
         reply(Type, FreeVars, Clause, Reply).

talk(_Sentence, error('too difficult')).
%%% parse(sentence, LF, Type)
%%%
%%% Sentence    ==> sentence to parse
%%%     LF      <== logical form (in FOL) of sentence
%%%     Type    <== type of Sentence
%%%             (query or assertion)

% Parsing an assertion: a finite sentence without gaps.
parse(Sentence, LF, assertion) :- s(LF, nogap, Sentence, []).

% Parsing a query: a question.
parse(Sentence, LF, query) :- q(LF, Sentence, []).
