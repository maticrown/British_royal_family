
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
%%%
tv(born, births, birthed, births, birthing, X^Y births(X,Y)).
tv(die, dies, died, dies, dying, X^Y dies(X,Y)).
%%%
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

