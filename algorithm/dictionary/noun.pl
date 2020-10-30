% Nouns

n(LF)     --> [N], {n(N, LF)}.

n(male,      X^ male(X)).
n(female,    X^ female(X)).
n(prince,    X^ prince(X)).
n(princess,  X^ princess(X)).
n(monarch,   X^ monarch(X)).
n(parent, X^ parent(X, _)).
n(father, X^ father(X, _)).
n(mother, X^ mother(X, _)).
n(brother, X^ brother(X, _)).
n(sister, X^ sister(X, _)).
n(son, X^ son(X, _)).
n(daughter, X^ daughter(X, _)).
n(child, X^ child(X, _)).
n(grandparent, X^ grandparent(X, _)).
n(grandmother, X^ grandmother(X, _)).
n(grandfather, X^ grandfather(X, _)).
n(cousin, X^ cousin(X, _)).
n(nephew, X^ nephew(X, _)).
n(niece, X^ niece(X, _)).
n(ancestor, X^ ancestor(X, _)).

% Proper nouns

pn((E^S)^S) --> [PN], {pn(PN, E)}.

pn(anne, anne).
pn(camilla, camilla).
pn(diana, diana).
pn(elizabethII, elizabethII).
pn(louise, louise).
pn(sophie, sophie).
pn(beatrice, beatrice).
pn(eugiene, eugiene).
pn(savannah, savannah).
pn(isla, isla).
pn(zara, zara).
pn(lenaElizabeth, lenaElizabeth).
pn(charlotte, charlotte).
pn(victoria, victoria).
pn(alexandra, alexandra).
pn(mary, mary).
pn(simpson, simpson).
pn(elizabethBowesLyon, elizabethBowesLyon).
pn(margaret, margaret).
pn(margdianaSpenceraret, margdianaSpenceraret).
pn(catherine, catherine).
pn(sarah, sarah).
pn(eugeine, eugeine).
pn(miaGrace, miaGrace).
pn(lenaElizabeth, lenaElizabeth).
pn(charles, charles).
pn(edward, edward).
pn(george, george).
pn(harry, harry).
pn(james, james).
pn(philip, philip).
pn(william, william).
pn(louis, louis).
pn(peter, peter).
pn(archie, archie).
pn(andrew, andrew).
pn(albert, albert).
pn(edwardVII, edwardVII).
pn(georgeV, georgeV).
pn(edwardVIII, edwardVIII).
pn(georgeVI, georgeVI).
pn(henry, henry).
pn(viscount, viscount).
pn(andrew, andrew).
pn(antony, antony).
<<<<<<< HEAD
=======
pn(pound, pound).
>>>>>>> Original/main
