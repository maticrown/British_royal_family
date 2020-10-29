

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

