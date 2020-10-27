%%women are direct heirs since they changed the law in 2011
heir_to_throne(H):-
  oldest(H),not(monarch_husband('Prince Philip',W)),monarch(W),!.

%basis of parents
parent('Prince Philip','Prince Charles').
parent('Prince Philip','Prince Andrew').
parent('Prince Philip','Prince Edward').
parent('Prince Philip','Princess Anne').


parent('Queen Elizabeth II','Princess anne').
parent('Queen Elizabeth II','Prince Charles').
parent('Queen Elizabeth II','Prince Edward').
parent('Queen Elizabeth II','Prince Andrew').


parent('Prince Charles','Prince William').
parent('Prince Charles','Prince Harry').
parent('Lady Diana','Prince Harry').
parent('Lady Diana','Prince William').

parent('Princess Anne','Prince Peter').
parent('Princess Anne','Princess Zara').

parent('Prince Andrew','Princess Beatrice').
parent('Prince Andrew','Princess Eugeine').

parent('Prince Edward','Princess Louise').
parent('Prince Edward','Prince James').


parent('Prince William','Prince George').
parent('Prince William','Princess Charlotte').
parent('Prince William','Prince Louis').

parent('Prince Harry','Archie').

parent('Prince Peter','Princess Savannah').
parent('Prince Peter','Princess Isla').

parent('Princess Zara','Princess Mia Grace').
parent('Princess Zara','Princess Lena Elizabeth').


female('Princess Anne').
female('Lady Camilla').
female('Lady Diana').
female('Queen Elizabeth II').
female('Princess Louise').
female('Princess Sohpie').
female('Princess Beatrice').
female('Princess Eugiene').
female('Princess Savannah').
female('Princess Isla').
female('Princess Zara').
female('Princss Lena Elizabeth').

male('Prince Charles').
male('Prince Edward').
male('Prince George').
male('Prince Harry').
male('Prince James').
male('Prince Philip').
male('Prince William').
male('Prince Louis').
male('Prince Peter').
male('Archie').

monarch('Lady camilla').
monarch('Lady Diana').
monarch('Queen Elizabeth II').
monarch('Princess Louise').
monarch('Princess Shopie').
monarch('Prince Charles').
monarch('Prince Edward').
monarch('Prince George').
monarch('Princess Anne').
monarch('Prince Harry').
monarch('Prince James').
monarch('Prince Philip').
monarch('Prince William').
monarch('Princess Charlotte').
monarch('Prince Louis').
monarch('Prince Beatrice').
monarch('Princess Eugiene').
monarch('Prince Andrew').
monarch('Princess Zara').

generation(1;2;3;4).

queen(X):-monarch(X),female(X).

prince_1(Y):-monarch(Y),male(Y),generation(1).

prince_2(Y):-monarch(Y),male(Y),generation(2).

prince_3(Y):-monarch(Y),male(Y),generation(3).

prince_4(Y):-monarch(Y),male(Y),generation(4).

princess_1(X):-monarch(X),female(X),generation(1).

princess_2(X):-monarch(X),female(X),generation(2).

princess_3(X):-monarch(X),female(X).

princess_4(X):-monarch(X),female(X),generation(4).

monarch_wife(W,H):-female(W),married(H,W).

monarch_husband(H,W):-male(H),married(H,W).

%person(P):-(male(P);female(P)).

mother(M,C):-female(M),parent(M,C).

father(F,C):-male(F),parent(F,C).

son(S,P):-male(S),parent(P,S).

daughter(D,P):-female(D),parent(P,D).

sibling(X,Y):-brother(X,Y);sister(X,Y),X\=Y.

child(X,Y):-son(X,Y);daughter(X,Y).

brother(Y,X):-male(Y),son(Y,P),child(X,P),Y\=P,X\=Y.

sister(X,Y):-female(X),daughter(X,P),child(Y,P),X\=P,X\=Y.

grandparent(G,X):- parent(P,X), parent(G,P).

grandchild(G,X):- parent(G,P),parent(P,X).

aunt(A,X):- parent(P,X), sister(A,P).

uncle(U,X):- parent(P,X), brother(U,P).


lgg_term(X,Y, X, List,List) :- X == Y, !.
lgg_term(X,Y, G, List,[s(X,Y,G)|List]) :- var(X); var(Y).

lgg_term(X,Y, G, List,List) :-
  member(s(A,B,G), List),
  A == X, B == Y, !.

lgg_term(X,Y, G, In,Out) :-
  X =.. [P|Xa], Y =.. [P|Ya],
  lgg_list(Xa,Ya, Ga, In,Out),
  G =.. [P|Ga], !.

lgg_term(X,Y, G, List,[s(X,Y,G)|List]).

% LGG on lists of equal length
lgg_list([],     [],      [],       Lst,Lst).
lgg_list([H1|T1],[H2|T2], [G|Rest], Beg,End):-
  lgg_term(H1,    H2,      G,       Beg,Mid),
  lgg_list(  T1,     T2,     Rest,  Mid,End).



family( person(philip, windsor, date(10, jun, 1921),duke_of_edinbrugh),
        person(elizabeth, windsor, date(21, apr, 1926),queen_of_england),
      [ person(charles, windsor, date(14, nov, 1948),prince_of_wales),
        person(anne, windsor, date(15, aug, 1958),princess_royal),
        person(andrew, windsor, date(19, feb, 1960),duke_of_york),

        person(edward, windsor, date(10, mar, 1964),earl_of_wessex)
      ]).


%to find firstborn
:- use_module(library(lists)).

%finding the eldest person
eldest_person(K) :-
   setof(X,A^B^Kids^(family(A,B,Kids),member(X,Kids)),L),
   eldest_in(K,L).                     % K is the eldest kid in L

eldest_in(EK,[K|Ks]) :-                % the first kid in the list
   eldest_in_(EK,Ks,K).                % is the eldest so far

eldest_in_(EK,[],EK).                  % empty list: ESF is eldest
eldest_in_(EK,[K|Ks],ESF) :-           % case 1: the Eldest So Far
   dateofbirth(K, date(_,_,KY)),
   dateofbirth(ESF, date(_,_,ESFY)),
   ESFY =< KY,                         % is elder that K ...
   eldest_in_(EK,Ks,ESF).              % hence still the ESF
eldest_in_(EK,[K|Ks],ESF) :-           % case 2: the ESF
   dateofbirth(K, date(_,_,KY)),
   dateofbirth(ESF, date(_,_,ESFY)),
   ESFY > KY,                          % is younger than K
   eldest_in_(EK,Ks,K).                % hence K is now the eldest

dateofbirth(person(_,_,Date,_), Date).


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

ancestor(A,A).
ancestor(A,D):-
  parent(A,C),
  ancestor(C,D).


cousin(Child1,Child2) :-
    grandparent(Y1,Child1),
    grandparent(Y2,Child2),
    not(sibling(Child1,Child2)),
    Y1=Y2.


samefather(X,Y) :-
   father(F,X),
   father(F,Y),
   X\=Y.

samemother(X,Y) :-
   mother(M,X),
   mother(M,Y),
   X\=Y.

sameparent(X,Y) :-
   samefather(X,Y).
sameparent(X,Y) :-
   samemother(X,Y),
   not(samefather(X,Y)).

niece(X,Y) :-
    female(X),
    parent(Z,X),
    sameparent(Z,Y),
    Z \= Y.

nephew(X,Y) :-
    male(X),
    parent(Z,X),
    sameparent(Z,Y),
    Z \= Y.



writes:-
    open('F://british_royal_family.txt',write,OS),
    X = 'Hi all',
    write(OS,X),
    close(OS),
    open('F://british_royal_family.txt',read,OS2),
    read(OS2,_).

readfacts:-
    open('F://british_royal_family.txt',read,In),
    repeat,
    read_line_to_codes(In,X),writef(" "),
    writef(X),nl,
    X=end_of_file,!,
    nl,
    close(In).



writefacts:-
    open('F://british_royal_family.txt',write,Out),
    write(Out,'Age(Peter,30)'),
    write(Out,'Skin(Smith,Black).'),
    close(Out).


writelist(integerlist).
writelist(namelist).

writelist([]).
writelist([H|T]):-
  write(H,""),
  writelist(T).


program1 :-
    open('F://british_royal_family.txt',write, Stream),
    forall(female(F), write(Stream,(F))),
    close(Stream).

program2 :-
    open('F://british_royal_family.txt',write, Stream),
    (   male(M), write(Stream, M), fail
    ;   true
    ),
    close(Stream).



%added
born('Queen Victoria', 1819).
born('Princess Alexandra of Denmark', 1844).
born('Queen Mary', 1867).
born('Mrs Simpson', 1896).
born('Lady Elizabeth Bowes-Lyon', 1900).
born('Queen Elizabeth II', 1926 ).
born('Princess Margaret', 1930).
born('Lady Diana Spencer', 1961).
born('Princess Anne', 1950).
born('Catherine Middleton', 1982).
born('Prince Albert', 1819).
born('King Edward VII', 1841 ).
born('King George V', 1865).
born('King Edward VIII', 1894).
born('King George VI', 1895).
born('Prince Philip', 1921).
born('Prince Charles', 1948).
born('Prince Andrew', 1960).
born('Prince Edward', 1964).
born('Prince William', 1982).
born('Prince Henry', 1984).
born('Prince George', 2013).
born('Princess Charlotte', 2015).
born('Viscount Linley', 1961).
born('Lady Sarah Chatto', 1964).
born('Antony Armstrong-Jones', 1930).

died('Queen Victoria', 1901).
died('Princess Alexandra of Denmark', 1925).
died('Queen Mary', 1953).
died('Mrs Simpson', 1986).
died('Lady Elizabeth Bowes-Lyon', 2002).
died('Princess Margaret', 2002).
died('Lady Diana Spencer', 1997).
died('Prince Albert', 1861).
died('King Edward VII', 1910).
died('King George V', 1936).
died('King Edward VIII', 1972).
died('King George VI', 1952).
died('Antony Armstrong-Jones', 2017).

married('Prince Albert', 'Queen Victoria').
married('King Edward VII', 'Princess Alexandra of Denmark').
married('King George V', 'Queen Mary').
married('King Edward VIII', 'Mrs Simpson').
married('King George VI', 'Lady Elizabeth Bowes-Lyon').
married('Prince Philip', 'Queen Elizabeth II').
married('Prince Charles', 'Lady Diana Spencer').
married('Prince William', 'Catherine Middleton').
married('Antony Armstrong-Jones', 'Princess Margaret').


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








%%%% for parsing questions and answers
:- op(500,xfy,'&').
:- op(510,xfy,'=>').
:- op(100,fx,'`').

%% This is the main run function that starts the questions and answers
%% of the "Marechet Mumche"
run :-
        write('>> '),           % prompt the user
        read_sent(Words),       % read a sentence
        talk(Words, Reply),     % process it with TALK
        print_reply(Reply),     % generate a reply
        run.                    % pocess more sentences

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


/* Nonterminal names:
   q       Question
   sinv    INVerted Sentence
   s       noninverted Sentence
   np      Noun Phrase
   vp      Verb Phrase
   iv      Intransitive Verb
   tv      Transitive Verb
   aux     AUXiliary verb
   rov     subject_Object Raising Verb
   optrel  OPTional RELative clause
   relpron RELative PRONoun
   whpron  WH PRONoun
   det     DETerminer
   n       Noun
   pn      Proper Noun

  Typical order of and values for arguments:

  1.    verb form:
     (main verbs) finite, nonfinite, etc.
     (auxiliaries and raising verbs) Form1-Form2
         where Forml is form of embedded VP
               Form2 is form of verb itself
  2.    FOL   logical form
  3.    gap   information:
              nogap or gap(Nonterm, Var)
                 where Nonterm is nonterminal for gap
                 Var is the LF variable that
                     the filler will bind
*/

%%% Declarative Sentences

s(S, GapInfo)   --> np(VP^S, nogap), vp(finite, VP, GapInfo).

%%% Inverted Sentences

sinv(S, GapInfo) --> aux(finite/Form, VP1^VP2),
                     np(VP2^S, nogap), vp(Form, VP1, GapInfo).

%%%     Questions

q(S => answer(X))   --> whpron, vp(finite, X^S, nogap).
q(S => answer(X))   --> whpron, sinv(S, gap(np, X)).
q(S => answer(yes)) --> sinv(S, nogap).
q(S => answer(yes)) --> [is],  np((X^S0)^S, nogap),
                  np((X^true)^exists(X,S0&true), nogap).

%%% Noun Phrases

%%  These are the parsing of the the nouns
np(NP, nogap) --> det(N2^NP), n(N1), optrel(N1^N2).
np(NP, nogap) --> pn(NP).
np((X^S)^S, gap(np, X)) --> [].

%%% Verb Phrases

%%  These are for the parsing of the verbs
vp(Form, X^S, GapInfo)  --> tv(Form, X^VP), np(VP^S, GapInfo).
vp(Form, VP, nogap)     --> iv(Form, VP).
vp(Form1, VP2, GapInfo) --> aux(Form1/Form2, VP1^VP2),
                            vp(Form2, VP1, GapInfo).
vp(Form1, VP2, GapInfo) --> rov(Form1/Form2, NP^VP1^VP2),
                            np(NP, GapInfo), vp(Form2, VP1, nogap).
vp(Form2, VP2, GapInfo) --> rov(Form1/Form2, NP^VP1^VP2),
                            np(NP, nogap), vp(Form1, VP1, GapInfo).
vp(finite, X^S, GapInfo) --> [is], np((X^P)^exists(X,S&P), GapInfo).

%%% Relative Clauses

optrel((X^S1)^(X^(S1&S2))) --> relpron, vp(finite, X^S2, nogap).
optrel((X^S1)^(X^(S1&S2))) --> relpron, s(S2, gap(np, X)).
optrel(N^N) --> [].

% Dictionary

% Verb  entry arguments:
%       1.      nonfinite form of the verb
%       2.      third person singular present tense form of the verb
%       3.      past tense form of the verb
%       4.      past participle form of the verb
%       5.      pres participle form of the verb
%       6.      logical form of the verb



iv(nonfinite,       LF) --> [IV], {iv(IV, _, _, _, _, LF)}.
iv(finite,          LF) --> [IV], {iv(_, IV, _, _, _, LF)}.
iv(finite,          LF) --> [IV], {iv(_, _, IV, _, _, LF)}.
iv(past_participle, LF) --> [IV], {iv(_, _, _, IV, _, LF)}.
iv(pres_participle, LF) --> [IV], {iv(_, _, _, _, IV, LF)}.
iv(halt, halts, halted, halted, halting,   X^ halts(X)).
iv(run,  runs,  ran,    run,    running,   X^ runs(X)).



tv(nonfinite,   LF)         --> [TV],   {tv(TV, _, _, _, _, LF)}.
tv(finite,      LF)         --> [TV],   {tv(_, TV, _, _, _, LF)}.
tv(finite,      LF)         --> [TV],   {tv(_, _, TV, _, _, LF)}.
tv(past_participle,     LF) --> [TV],   {tv(_, _, _, TV, _, LF)}.
tv(pres_participle,     LF) --> [TV],   {tv(_, _, _, _, TV, LF)}.

%%
tv(precede,   precedes,   preceded,  precedes , preceding,    X^Y^ precedes(X,Y)).
tv(inherit,   inherits,   inherited,  inherited , inheriting,    X^Y^ inherits(X,Y)).
tv(own,   owns,   owned,  owned , owning,    X^Y^ owns(X,Y)).
tv(succede,   succedes,   succeded,  succedes , succeding,    X^Y^ succedes(X,Y)).
%%

tv(write,   writes,   wrote,     written,   writing,    X^Y^ writes(X,Y)).
tv(read,    reads,    read,      read,      reading,    X^Y^ reads(X,Y)).
tv(speak,   speaks,   spoke,     spoken,    speaking,   X^Y^ speaks(X,Y)).
tv(meet,    meets,    met,       met,       meeting,    X^Y^ meets(X,Y)).
tv(concern, concerns, concerned, concerned, concerning, X^Y^ concerns(X,Y)).
tv(run,     runs,     ran,       run,       running,    X^Y^ runs(X,Y)).

rov(nonfinite   /Requires,    LF) --> [ROV], {rov(ROV, _, _, _, _, LF, Requires)}.
rov(finite      /Requires,    LF) --> [ROV], {rov(_, ROV, _, _, _, LF, Requires)}.
rov(finite      /Requires,    LF) --> [ROV], {rov(_, _, ROV, _, _, LF, Requires)}.
rov(past_participle/Requires, LF) --> [ROV], {rov(_, _, _, ROV, _, LF, Requires)}.
rov(pres_participle/Requires, LF) --> [ROV], {rov(_, _, _, _, ROV, LF, Requires)}.
rov(want,   wants,    wanted,    wanted,    wanting,
     % semantics is partial execution of
     % NP ^ VP ^ Y ^ NP( X^want(Y,X,VP(X))
      ((X^ want(Y, X, Comp))^S) ^ (X^Comp) ^ Y ^ S,
     % form of VP required:
      infinitival).



aux(Form, LF) --> [Aux], {aux(Aux, Form, LF)}.
aux(to,    infinitival/nonfinite,    VP^ VP).
aux(does,  finite/nonfinite,         VP^ VP).
aux(did,   finite/nonfinite,         VP^ VP).
aux(could, finite/nonfinite,         VP^ VP).
aux(have,  nonfinite/past_participle, VP^ VP).
aux(has,   finite/past_participle,   VP^ VP).
aux(been,  past_participle/present_participle, VP^ VP).
aux(be,    nonfinite/present_participle,       VP^ VP).

relpron       --> [RP], {relpron(RP)}.
relpron(that).
relpron(who).
relpron(whom).

whpron         --> [WH], {whpron(WH)}.
whpron(who).
whpron(whom).
whpron(what).



det(LF)   --> [D], {det(D, LF)}.
det(every,  (X^S1) ^ (X^S2) ^ all(X, S1=>S2)).
det(a,      (X^S1) ^ (X^S2) ^ exists(X, S1&S2)).
det(some,   (X^S1) ^ (X^S2) ^ exists(X, S1&S2)).

n(LF)     --> [N], {n(N, LF)}.
n(author,     X^ author(X)).
n(book,       X^ book(X)).
n(professor,  X^ professor(X)).
n(program,    X^ program(X)).
n(programmer, X^ programmer(X)).
n(student,    X^ student(X)).
n(person,     X^ person(X)).
n(language,   X^ language(X)).
%%% added
n(monarche,   X^ monarche(X)).
n(money,      X^ money(X)).
n(acting_royal, X^ acting_royal(X)).
n(gender,     X^ gender(X)).
n(male_,      X^ male_(X)).
n(female_,    X^ female_(X)).
n(prince_,    X^ prince_(X)).
n(princess_,  X^ princess_(X)).
%%%

pn((E^S)^S) --> [PN], {pn(PN, E)}.
pn(allen,    allen).
pn(bruce,    bruce).
pn(bertrand, bertrand).
pn(terry,    terry).
pn(bill,     bill).
pn(david,    david).
pn(kathy,    kathy).
pn(behshad,  behshad).
pn(shane,    shane).
pn(principia, principia).
pn(shrdlu,   shrdlu).
pn(prolog,   prolog).
pn(english,  english).
pn(chinese,  chinese).
pn(korean,   korean).
pn(swahili,  swahili).

%% added
pn(charles,  charles).
pn(elizabethII, elizabethII).
pn(camilla, camilla).
pn(anne, anne).
pn(andrew, andrew).
pn(edward, edward).
pn(harry, harry).
pn(william, william).
pn(philip, philip).
pn(charolette, charolette).
pn(louis, louis).
pn(archie, archie).
pn(eugiene, eugiene).
pn(zara, zara).
pn(isla, isla).
pn(james, james).
pn(lena, lena).
pn(pound,    pound).
%%

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

% Auxiliary Predicates

conc([], List, List).
conc([Element|Rest], List, [Element|LongRest]) :-
                        conc(Rest, List, LongRest).

%%% read_sent(Words)
%%% input ==> series of words terminated by a new line character
%%% Words <== list of the words
read_sent(Words) :- get0(Char), read_sent(Char, Words).

read_sent(C, []) :- newline(C), !.
read_sent(C, Words) :- space(C), !, get0(Char),
                        read_sent(Char, Words).
read_sent(C, [Word|Words]) :- read_word(C, Chars, Next),
                        name(Word, Chars),
                        read_sent(Next, Words).

read_word(C, [], C) :- space(C), !.
read_word(C, [], C) :- newline(C), !.
read_word(C, [C|Chars], Last) :- get0(Next),
                        read_word(Next, Chars, Last).

newline(10).
space(32).

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



%%% print_reply(Reply)
%%%
%%% Reply ==> reply generated by reply predicate
%%%           that is to be printed to the standard output.

print_reply(error(ErrorType)) :-
     write('Error: "'), write(ErrorType), write('."'), nl.

print_reply(asserted(Assertion)) :-
     write('Asserted "'), write(Assertion), write('."'), nl.
%%% added
print_reply(retract(Retraction)):-
    write('Retracted"'), write(Retraction), write('."'),nl.
%%%
print_reply(answer(Answers)) :- print_answers(Answers).

%%%     print_answer(Answers)
%%%
%%%     Answers ==> nonempty list of answers to be printed
%%%                 to the standard output separated by commas.

print_answers([Answer]) :- !, write(Answer), write('.'), nl.

print_answers([Answer|Rest]) :- write(Answer), write(', '),
                                print_reply(answer(Rest)).



































