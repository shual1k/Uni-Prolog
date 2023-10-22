/****I, shauli kirshenzwieg (313551905) assert that the work I submitted is entirely my own.
I have not recieved any part from any other student in the class (or another source),
nor did I give parts of it for use to others. I have clearly marked in the comments
of my program any code taken from an external source. ****/

:- module(ex3, [
                 number/4,
                 add/4,
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

unary2Num(Unary,Num):-
    unaryToNumber_(Unary,0,Num).
unaryToNumber_([],Num,Num).
unaryToNumber_([X|Xs],Acc,Num):-
    (  ( X =:= 1, NewAcc is Acc + 1 ) 
       ;
       NewAcc is Acc + 0  ),
    unaryToNumber_(Xs,NewAcc,Num).
    
num2Unary(0,0,[]).
num2Unary(0,Len,[Un|Rest]):-
    Un is -1,
    NewLen is Len - 1,
    num2Unary(0,NewLen,Rest).
num2Unary(N,Len,[Un|Rest]):-
    N > 0,
    Un is 1,
    NewN is N - 1,
    NewLen is Len - 1,
    num2Unary(NewN,NewLen,Rest).



%TASK 1:
number(LB,UB,Vector,Cnf):-
    LB >= 0,
    UB >= LB,
    Len = UB,
    length(Vector,Len),
    num2Unary(LB,Len,UnaryLB),
    num2Unary(UB,Len,UnaryUB),
    makeNumber_(UnaryLB,UnaryUB,Vector,Cnf1),
    correctUnary_(Vector,Cnf2),
    append(Cnf1,Cnf2,Cnf), !.
    
makeNumber_([],[],[],[]).
makeNumber_([X|Xs],[Y|Ys],[Z|Zs],Cnf):-
    Cnf = [[-X,-Y,Z],[X,Y,-Z]|Cnf2],
    makeNumber_(Xs,Ys,Zs,Cnf2).

correctUnary_([X1,X2|[]],Cnf):-
    Cnf = [[X1,-X2]].
correctUnary_([X1,X2|Rest],Cnf):-
    Cnf = [[X1,-X2]|Cnf2],
    correctUnary_([X2|Rest],Cnf2).



%TASK 2:
add(Xs,Ys,Zs,Cnf):-
    setLengthZ_(Xs,Ys,Zs),
    append([1|Xs],[-1],XsExtra),
    append([1|Ys],[-1],YsExtra),
    append([1|Zs],[-1,-1],ZsExtra),
    unaryAddSimilar_(XsExtra,YsExtra,ZsExtra,CnfSimilar),
    correctUnary_(Zs,CnfCorrect),
    append(CnfSimilar,CnfCorrect,Cnf1),
    quadAdd_(XsExtra,YsExtra,[1|ZsExtra],CnfQuad),
    append(Cnf1,CnfQuad,Cnf) ,!.
	
setLengthZ_(Xs,Ys,Zs):-
    length(Xs,LenX),
    length(Ys,LenY),
    LenZ is LenX + LenY,
    length(Zs,LenZ).

unaryAddSimilar_([],_,_,[]).
unaryAddSimilar_([X|Xs],[Y|Ys],[Z|Zs],Cnf):-
    unaryAddSimilarSingle_(X,[Y|Ys],[Z|Zs],Cnf1),
    unaryAddSimilar_(Xs,[Y|Ys],Zs,Cnf2),
    append(Cnf1,Cnf2,Cnf).

unaryAddSimilarSingle_(_,[],_,[]).
unaryAddSimilarSingle_(X,[Y|Ys],[Z|Zs],Cnf):-
    Cnf = [[-X,-Y,Z],[X,Y,-Z]|Cnf2],
    unaryAddSimilarSingle_(X,Ys,Zs,Cnf2).

quadAdd_([_|[]],_,_,[]).
quadAdd_([X1,X2|Xs],[Y1,Y2|Ys],[Z2,Z3|Zs],Cnf):-
    quadAddSingle_([X1,X2],[Y1,Y2|Ys],[Z2,Z3|Zs],Cnf1),
    quadAdd_([X2|Xs],[Y1,Y2|Ys],[Z3|Zs],Cnf2),
    append(Cnf1,Cnf2,Cnf).

quadAddSingle_([_,_],[_|[]],_,[]).
quadAddSingle_([X1,X2],[Y1,Y2|Ys],[Z2,Z3|Zs],Cnf):-
    Cnf = [[-X1,X2,-Y1,Y2,Z2,-Z3],[-Y1,Y2,-Z2,-Z3,X1,X2],[-X1,X2,-Z2,-Z3,Y1,Y2]|Cnf2],
    quadAddSingle_([X1,X2],[Y2|Ys],[Z3|Zs],Cnf2).



%TASK 3:
diff(Xs,Ys,Cnf):-
    diffWithCarry(Xs,Ys,-1,Cnf).

diffWithCarry([],[],Was,[[Was]]).
diffWithCarry([],[Y|_],Was,Cnf):-
    Cnf = [[-Y,Is],[Y,Was,-Is],[-Was,Is]|Cnf2],
    diffWithCarry([],[],Is,Cnf2).
diffWithCarry([X|_],[],Was,Cnf):-
    Cnf = [[-X,Is],[X,Was,-Is],[-Was,Is]|Cnf2],
    diffWithCarry([],[],Is,Cnf2).
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



%TASK 4:
verify(Instance,Solution):-
    ( not_kokuro_(Solution), print(verify:wrong), nl )
    ;
    print(verify:ok), nl.



%TASK 5:
encode(Instance,Map,Cnf):-
    /***build the map***/
    Map = Instance,
    /***all elements in a block are different numbers between 1-9***/
    correctNumbersBlock_(Map,CnfNumbers),
    allDiffPerBlock_(Map,CnfDiff),
    append(CnfNumbers,CnfDiff,Cnf1),
    /***all sums correct***/
    makeUnarySums_(Instance,[],UnarySums),
    compareSums_(Map,UnarySums,CnfSums),
    /***complete Cnf***/
    append(Cnf1,CnfSums,Cnf), !.

correctNumbersBlock_([],[]).
correctNumbersBlock_([ClueBlock|RestMap],Cnf):-
    ClueBlock = (_ = UnaryBlock),
    correctNumberRange_(UnaryBlock,Cnf1),
    correctNumbersBlock_(RestMap,Cnf2),
    append(Cnf1,Cnf2,Cnf).

correctNumberRange_([],[]).
correctNumberRange_([X|Xs],Cnf):-
    number(1,9,X,Cnf1),
    correctNumberRange_(Xs,Cnf2),
    append(Cnf1,Cnf2,Cnf).

allDiffPerBlock_([],[]).
allDiffPerBlock_([ClueBlock|RestMap],Cnf):-
    ClueBlock = (_ = UnaryBlock),
    allDiff(UnaryBlock,Cnf1),
    allDiffPerBlock_(RestMap,Cnf2),
    append(Cnf1,Cnf2,Cnf).

makeUnarySums_([],Res,Res).
makeUnarySums_([ClueBlock|Rest],Acc,Res):-
    ClueBlock = (ClueSum = _),
    Len = ClueSum,
    num2Unary(ClueSum,Len,UnarySum),
    append(Acc,[UnarySum],NewAcc),
    makeUnarySums_(Rest,NewAcc,Res).

compareSums_([],[],[]).
compareSums_([ClueBlock|RestMap],[Sum|RestUnarySums],Cnf):-
    ClueBlock = (_ = UnaryBlock),
    sumBlockAndCompare_(UnaryBlock,[-1],Sum,Cnf1),
    compareSums_(RestMap,RestUnarySums,Cnf2),
    append(Cnf1,Cnf2,Cnf).

sumBlockAndCompare_([],Sum,UnarySum,Cnf):-
    equal_(Sum,UnarySum,Cnf).
sumBlockAndCompare_([X|Xs],Acc,UnarySum,Cnf):-
    add(X,Acc,NewAcc,Cnf1),
    sumBlockAndCompare_(Xs,NewAcc,UnarySum,Cnf2),
    append(Cnf1,Cnf2,Cnf).

equal_(Xs,Ys,Cnf):-
    equalWithCarry_(Xs,Ys,1,Cnf).

equalWithCarry_([],[],Was,[[Was]]).
equalWithCarry_([],[Y|_],Was,Cnf):-
    Cnf = [[-Y,-Is],[Y,-Was,Is],[Was,-Is]|Cnf2],
    equalWithCarry_([],[],Is,Cnf2).
equalWithCarry_([X|_],[],Was,Cnf):-
    Cnf = [[-X,-Is],[X,-Was,Is],[Was,-Is]|Cnf2],
    diffWithCarry([],[],Is,Cnf2).
equalWithCarry_([X|Xs],[Y|Ys],Was,Cnf):-
    Cnf = [[Was,-Is],[X,Y,-Was,Is],[-X,-Y,-Was,Is],[X,-Y,-Is],[-X,Y,-Is]|Cnf2],
    equalWithCarry_(Xs,Ys,Is,Cnf2).



%decode:
decode(Map,Solution):-
    length(Map,MapLength),
    length(Solution,MapLength),
    convertToSolution_(Map,Solution), !.

convertToSolution_([],[]).
convertToSolution_([UnaryClueBlock|RestMap],[Dec|RestSolution]):-
    UnaryClueBlock = (ClueSum = UnaryBlock),
    unaryBlockToDecBlock_(UnaryBlock,[],DecBlock),
    Dec = (ClueSum = DecBlock),
    convertToSolution_(RestMap,RestSolution).
    
unaryBlockToDecBlock_([],Res,Res).
unaryBlockToDecBlock_([Unary|RestBlock],Acc,Res):-
    unary2Num(Unary,Dec),
    append(Acc,[Dec],NewAcc),
    unaryBlockToDecBlock_(RestBlock,NewAcc,Res).



%solve:
solve(Instance, Solution):-
    encode(Instance,Map,Cnf),
    sat(Cnf),
    decode(Map,Solution),
    verify(Instance, Solution).



%time
time(X):-
    statistics(cputime,Time1),
    (call(X) -> writeln(true) ; writeln(false)),
    statistics(cputime,Time2),
    Time12 is Time2-Time1,
    writeln(Time12:sec).



%not_kokuro:
not_kokuro_([]):- false.
not_kokuro_([ClueBlock|Rest]):-
    incorrectSum_(ClueBlock);
    multipleInstance_(ClueBlock);
    not_kokuro_(Rest).
   
incorrectSum_(ClueBlock):-
    ClueBlock = (ClueSum = Block),
    sum_(Block,0,Sum),
    ClueSum =\= Sum.

sum_([],Res,Res).
sum_([X|Xs],Acc,Res):-
    NewAcc is Acc + X,
    sum_(Xs,NewAcc,Res).

multipleInstance_(ClueBlock):-
    ClueBlock = (_ = Block), 
    member(A,Block), 
    append(Pre,[A|Post],Block),
    ( member(A,Pre) ; member(A,Post) ).