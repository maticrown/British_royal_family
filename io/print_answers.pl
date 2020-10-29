
%%%     print_answer(Answers)
%%%
%%%     Answers ==> nonempty list of answers to be printed
%%%                 to the standard output separated by commas.

print_answers([Answer]) :- !, write(Answer), write('.'), nl.

print_answers([Answer|Rest]) :- write(Answer), write(', '),
                                print_reply(answer(Rest)).

