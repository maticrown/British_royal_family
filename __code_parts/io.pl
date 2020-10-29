
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