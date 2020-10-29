
% loading the facts & relations
:- consult('facts/parentings.pl').
:- consult('facts/person_detailes.pl').
:- consult('facts/marriages.pl').

:- consult('relations/family_relations.pl').
:- consult('relations/monarchial_relations.pl').

:- consult('shared/operators.pl').

:- consult('algorithm/talk.pl').
:- consult('algorithm/clausifier.pl').

:- consult('io/read_sent.pl').
:- consult('io/reply.pl').
:- consult('io/print_reply.pl').
:- consult('io/print_answers.pl').


%% This is the main run function that starts the questions and answers
%% of the "Marechet Mumche"
run :-
        write('>> '),           % prompt the user
        read_sent(Words),       % read a sentence
        talk(Words, Reply),     % process it with TALK
        print_reply(Reply),     % generate a reply
        run.                    % pocess more sentences


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
