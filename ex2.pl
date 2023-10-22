/****I, shauli kirshenzwieg (313551905) assert that the work I submitted is entirely my own.
I have not recieved any part from any other student in the class (or another source),
nor did I give parts of it for use to others. I have clearly marked in the comments
of my program any code taken from an external source. ****/

:- module(ex2, [
                 binary2decimal/2,
                 decimal2binary/2,
                 add/4,
                 lt/3,
                 leq/3,
                 sorted/2,
                 diff/3,
                 allDiff/2,
                 verify/2,
                 encode/3,
                 solve/2,
                 time/1]).

%Base Predicates
binary_list([]).
binary_list([X|Xs]):-
    member(X,[-1,1]),
    binary_list(Xs).

binNegToZero([],[]).
binNegToZero([-1|Xs],[0|Ys]):-
    binNegToZero(Xs,Ys).
binNegToZero([1|Xs],[1|Ys]):-
    binNegToZero(Xs,Ys).


binZeroToNeg([],[]).
binZeroToNeg([0|Xs],[-1|Ys]):-
    binZeroToNeg(Xs,Ys).
binZeroToNeg([1|Xs],[1|Ys]):-
    binZeroToNeg(Xs,Ys).



%TASK 1:
binary2decimal(Xs,N):-
    binNegToZero(Xs,NewXs),
    bin2dec_(NewXs,0,0,N).

bin2dec_([],Res,_,Res).
bin2dec_([X|Xs],Acc,Pow,Res):-
    ToAdd is X * 2 ** Pow,
    NewAcc is Acc + ToAdd,
    NewPow is Pow + 1,
    bin2dec_(Xs,NewAcc,NewPow,Res).

decimal2binary(N,Xs):-
    dec2bin_(N,[],Ys),
    reverse(Ys, RevYs),
    binZeroToNeg(RevYs,Xs).

dec2bin_(0,Res,Res).
dec2bin_(N,Acc,Res):-
    Reminder is N mod 2,
    NewN is (N - Reminder) / 2,
    append([Reminder],Acc,NewAcc),
    dec2bin_(NewN,NewAcc,Res).



%TASK 2:
add(Xs,Ys,Zs,Cnf):-
    binAddWithCarry(Xs,Ys,Zs,-1,Cnf).

binAddWithCarry([],[],[],C,[[-C]]).
binAddWithCarry([X|Xs],[Y|Ys],[Z|Zs],C,Cnf):-
    fullAdder(X,Y,C,Z,NewC,Cnf1),
    binAddWithCarry(Xs,Ys,Zs,NewC,Cnf2),
    append(Cnf1,Cnf2,Cnf).

fullAdder(X,Y,C,Sum,NewC,Cnf):-
    CnfSum = [[X,Y,C,-Sum],[X,-Y,-C,-Sum],[-X,Y,-C,-Sum],[-X,-Y,C,-Sum],[-X,-Y,-C,Sum],[-X,Y,C,Sum],[X,-Y,C,Sum],[X,Y,-C,Sum]],
    CnfNewC = [[X,Y,C,-NewC],[X,Y,-C,-NewC],[X,-Y,C,-NewC],[-X,Y,C,-NewC],[-X,-Y,C,NewC],[-X,Y,-C,NewC],[X,-Y,-C,NewC],[-X,-Y,-C,NewC]],
    append(CnfSum,CnfNewC,Cnf).



%TASK 3:
lt(Xs,Ys,Cnf):-
    ltWithCarry(Xs,Ys,-1,Cnf).
    
ltWithCarry([],[],Was,[[Was]]).
ltWithCarry([X|Xs],[Y|Ys],Was,Cnf):-
    Cnf = [[X,Y,-Was,Is],[X,Y,Was,-Is],[X,-Y,Is],[-X,Y,-Is],[-X,-Y,-Was,Is],[-X,-Y,Was,-Is]|Cnf2],
    ltWithCarry(Xs,Ys,Is,Cnf2).
    
leq(Xs,Ys,Cnf):-
    leqWithCarry(Xs,Ys,1,Cnf).

leqWithCarry([],[],Was,[[Was]]).
leqWithCarry([X|Xs],[Y|Ys],Was,Cnf):-
    Cnf = [[X,Y,-Was,Is],[X,Y,Was,-Is],[X,-Y,Is],[-X,Y,-Is],[-X,-Y,-Was,Is],[-X,-Y,Was,-Is]|Cnf2],
    ltWithCarry(Xs,Ys,Is,Cnf2).

sorted(Vectors,Cnf):-
    sortedList_(Vectors,Cnf).

sortedList_([],[]).
sortedList_([_|[]],[]).
sortedList_([X,Y|Vectors],Cnf):-
    lt(X,Y,Cnf1),
    sortedList_([Y|Vectors],Cnf2),
    append(Cnf1,Cnf2,Cnf).



%TASK 4:
diff(Xs,Ys,Cnf):-
    diffWithCarry(Xs,Ys,-1,Cnf).

diffWithCarry([],[],Was,[[Was]]).
diffWithCarry([X|Xs],[Y|Ys],Was,Cnf):-
    Cnf = [[-Was,Is],[X,Y,Was,-Is],[-X,-Y,Was,-Is],[X,-Y,Is],[-X,Y,Is]|Cnf2],
    diffWithCarry(Xs,Ys,Is,Cnf2).


allDiff([_|[]],[[1]]).
allDiff([X|Vectors],Cnf):-
    allDiffList_(X,Vectors,Cnf1),
    allDiff(Vectors,Cnf2),
    append(Cnf1,Cnf2,Cnf).

allDiffList_(_,[],[[1]]).
allDiffList_(X,[Y|Vectors],Cnf):-
    diff(X,Y,Cnf1),
    allDiffList_(X,Vectors,Cnf2),
    append(Cnf1,Cnf2,Cnf).



%TASK 5:
verify(Instance,Solution):-
    Instance = golomb(N,Max),
    ( not_golomb(Solution,N,Max), !, print(verify:wrong), nl )
    ;
    print(verify:ok), nl.





%TASK 6:
encode(Instance,Map,Cnf):-
    /***build the map***/
    Instance = golomb(N,Max),
    decimal2binary(Max,BinMax),
    length(BinMax,MaxLength),
    makeMap_(N,MaxLength,Map),
    /***sorted list and less than max***/
    sorted(Map,CnfSorted),
    lastElementLeqMax(Map,BinMax,CnfMax),
    append(CnfSorted,CnfMax,Cnf1),
    /***all differences are different***/
    DiffListLength is N * (N - 1) / 2,
    makeMap_(DiffListLength, MaxLength, DiffList),
    getDifferencesStart_(Map,Map,DiffList,CnfDiffs), 
    allDiff(DiffList,CnfNq),
    append(CnfDiffs,CnfNq,Cnf2),
    /***complete Cnf***/
    append(Cnf1,Cnf2,Cnf), !.
    
makeMap_(MapLength,ElementLength,Map):-
    length(Map,MapLength),
    mapLengths_(Map,ElementLength).

mapLengths_([],_).
mapLengths_([First|Rest],ElementLength):-
    length(First,ElementLength),
    mapLengths_(Rest,ElementLength).

lastElementLeqMax(List,Max,Cnf):-
    reverse(List,RevList),
    append([Last],_,RevList),
    leq(Last,Max,Cnf).

getDifferencesStart_(Map, [_|RestMap],DiffList,Cnf):-
    getDifferences_(Map,RestMap,DiffList,Cnf).

getDifferences_([_|[]],_,_,[]).
getDifferences_([_,X2|Rest],[],DiffList,Cnf):-
    getDifferences_([X2|Rest],Rest,DiffList,Cnf).
getDifferences_([X1|Rest],[Y1|RestCheck],[D|RestDiffs],Cnf):-
    add(D,X1,Y1,CnfAdd),
    getDifferences_([X1|Rest],RestCheck,RestDiffs,CnfRest),
    append(CnfAdd,CnfRest,Cnf).


%decode:
decode(Map,Solution):-
    length(Map,MapLength),
    length(Solution,MapLength),
    convertToSolution(Map,Solution).

convertToSolution([],[]).
convertToSolution([Bin|RestMap],[Dec|RestSolution]):-
    binary2decimal(Bin,Dec),
    convertToSolution(RestMap,RestSolution).


%solve:
solve(Instance, Solution) :-
    encode(Instance,Map,Cnf),
    sat(Cnf),
    decode(Map,Solution),
    verify(Instance, Solution).


%time
time(X) :-
    statistics(cputime,Time1),
    (call(X) -> writeln(true) ; writeln(false)),
    statistics(cputime,Time2),
    Time12 is Time2-Time1,
    writeln(Time12:sec).


%not_golomb:
not_golomb([]).
not_golomb([X|Xs],N,Max):-
    X =\= 0;
    ( length([X|Xs],Len), Len =\= N );
    ( reverse(Xs, RevXs), RevXs=[Last|_], Last > Max ); 
    not_increasing_list_([X|Xs]);
    (
    member(A,[X|Xs]),
    member(B,[X|Xs]),
    A < B,
    member(C,[X|Xs]),
    A =\= C,
    member(D,[X|Xs]),
    C < D,
    BA is B - A,
    DC is D - C,
    BA =:= DC
    ).

not_increasing_list_([_|[]]):- false.
not_increasing_list_([X1,X2|Xs]):-
    X1 > X2;
    not_increasing_list_([X2|Xs]).
