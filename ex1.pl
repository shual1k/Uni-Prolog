/****I, shauli kirshenzwieg (313551905) assert that the work I submitted is entirely my own.
I have not recieved any part from any other student in the class (or another source),
nor did I give parts of it for use to others. I have clearly marked in the comments
of my program any code taken from an external source. ****/

:- module(ex1, [
                 unary_sqrt/2,
                 unary_divisor/2,
                 binary_plus/3,
                 binary_times/3,
                 gentree/3,
                 is_prime/1,
                 not_golomb/1,
				 golomb/3,
                 evaluate/2,
                 dom/1]).

%Base Predicates
nat(0).
nat(s(X)):- nat(X).

unary_plus(0,X,X).
unary_plus(s(X),Y,s(Z)):- unary_plus(X,Y,Z).

unary_times(0,X,0):- nat(X).
unary_times(s(X),Y,Z):- unary_times(X,Y,W), unary_plus(W,Y,Z).

unary_leq(0,X):- nat(X).
unary_leq(s(X),s(Y)):- unary_leq(X,Y).

unary_gt(s(X),0):-nat(X).
unary_gt(s(X),s(Y)):-unary_gt(X,Y).

incr(Cur,Nxt):- Nxt is Cur+1.



%TASK 1:
unary_sqrt(N,K):-
    power_2_sequence_(0,N,Res), %Res is the first unary number n where n^2 > N.
    unary_plus(K,s(0),Res).     %K is 1 less than Res

power_2_sequence_(Acc,N,Acc):-
    unary_times(Acc,Acc,Acc2),
    unary_gt(Acc2,N).

power_2_sequence_(Acc,N,Res):-
    unary_times(Acc,Acc,Acc2),
    unary_leq(Acc2,N),
    power_2_sequence_(s(Acc),N,Res).



%TASK 2:
%finds every two numbers R,K smaller than N where R*K=N then return K.
unary_divisor(N,K):-
    unary_leq(R,N),
    unary_leq(K,N),
    unary_times(K,R,N).



%TASK 3:       (repeating result when right number is longer)
binary_list([]).
binary_list([X|Xs]):-
    member(X,[0,1]),
    binary_list(Xs).

binary_plus(X,Y,Z):-
    binary_list(X),
    binary_list(Y),
    bitwise_plus_(0,X,Y,Z).

    %empty list carry=0:
bitwise_plus_(0,[],X,X).
bitwise_plus_(0,X,[],X).
    %empty list, carry=1:
bitwise_plus_(1,[],[0|Xs],[1|Zs]):- bitwise_plus_(0,[],Xs,Zs).
bitwise_plus_(1,[0|Xs],[],[1|Zs]):- bitwise_plus_(0,Xs,[],Zs).
bitwise_plus_(1,[],[1|Xs],[0|Zs]):- bitwise_plus_(1,[],Xs,Zs).
bitwise_plus_(1,[1|Xs],[],[0|Zs]):- bitwise_plus_(1,Xs,[],Zs).
     %both lists empty, carry=1:
bitwise_plus_(1,[],[],[1]).
     %0+0, carry=0/1:
bitwise_plus_(Carry,[0|Xs],[0|Ys],[Carry|Zs]):- bitwise_plus_(0,Xs,Ys,Zs).
     %0+1, carry=0:
bitwise_plus_(0,[1|Xs],[0|Ys],[1|Zs]):- bitwise_plus_(0,Xs,Ys,Zs).
bitwise_plus_(0,[0|Xs],[1|Ys],[1|Zs]):- bitwise_plus_(0,Xs,Ys,Zs).
     %0+1, carry=1:
bitwise_plus_(1,[0|Xs],[1|Ys],[0|Zs]):- bitwise_plus_(1,Xs,Ys,Zs).
bitwise_plus_(1,[0|Xs],[1|Ys],[0|Zs]):- bitwise_plus_(1,Xs,Ys,Zs).
     %1+1, carry=0:
bitwise_plus_(0,[1|Xs],[1|Ys],[0|Zs]):- bitwise_plus_(1,Xs,Ys,Zs).
     %1+1, carry=1:
bitwise_plus_(1,[1|Xs],[1|Ys],[1|Zs]):- bitwise_plus_(1,Xs,Ys,Zs).

/****better version of Task 3:
binary_plus(X,Y,Z):-
    binary_list(X),
    binary_list(Y),
    binary_plus_with_carry_(X,Y,0,Z).

binary_plus_with_carry_([],[],0,[]).
binary_plus_with_carry_([],[],1,[1|[]]).
binary_plus_with_carry_([X|Xs],[],C,[Z|Zs]):-
	full_adder_(X,0,C,Z,NewC),
    binary_plus_with_carry_(Xs,[],NewC,Zs).
binary_plus_with_carry_([],[Y|Ys],C,[Z|Zs]):-
	full_adder_(0,Y,C,Z,NewC),
    binary_plus_with_carry_([],Ys,NewC,Zs).
binary_plus_with_carry_([X|Xs],[Y|Ys],C,[Z|Zs]):-
	full_adder_(X,Y,C,Z,NewC),
    binary_plus_with_carry_(Xs,Ys,NewC,Zs).

full_adder_(0,0,0,0,0).
full_adder_(0,0,1,1,0).
full_adder_(0,1,0,1,0).
full_adder_(1,0,0,1,0).
full_adder_(1,1,0,0,1).
full_adder_(1,0,1,0,1).
full_adder_(0,1,1,0,1).
full_adder_(1,1,1,1,1).
****/



%TASK 4:
binary_times(X,Y,Z):-
    binary_list(X), % check X is valid binary list.
    binary_list(Y), % check Y is valid binary list.
    bitwise_times_(X,Y,Z).

bitwise_times_([],_,[]).
bitwise_times_(_,[],[]).
bitwise_times_([0|Xs],Y,Z):-
    bitwise_times_(Xs,Y,W),
    binary_plus([],[0|W],Z).
bitwise_times_([1|Xs],Y,Z):-
    bitwise_times_(Xs,Y,W),
    binary_plus(Y,[0|W],Z).



%TASK 5:
bTree(nil).
bTree(tree(L,_,R)):-
    bTree(L),
    bTree(R).

gentree(Pre,Post,Tree):-
    is_preorder_(Pre,Tree),
    is_postorder_(Post,Tree),
    bTree(Tree).

is_preorder_([],nil).
is_preorder_([C|Pre],tree(L,C,R)):-
    append(LeftList,RightList,Pre),
    is_preorder_(LeftList,L),
    is_preorder_(RightList,R).

is_postorder_([],nil).
is_postorder_(Post,tree(L,C,R)):-
    append(PostNoC,[C],Post),
    append(LeftList,RightList,PostNoC),
    is_postorder_(LeftList,L),
    is_postorder_(RightList,R).



%TASK 6:
is_prime(2).
is_prime(N):-
    number(N),
    N > 1,
    N mod 2 =\= 0,
    Lim is (sqrt(N)),
    Start is 3,
    odd_division_sequence_(Start,Lim,N).

/****divide N by every odd number between 3 and sqrt of N.
if N has a divisor K larger than its sqrt than K*R=N where R is less
than sqrt(N), so no need to search numbers larger than that.
division_sequence_(Cur, Lim, N):
  Cur = numbers we go over up to Lim in increasing order (starts at 2),
  Lim = sqrt(N).****/
odd_division_sequence_(Cur,Lim,_):-
    Cur > Lim.
odd_division_sequence_(Cur,Lim,N):-
    Cur =< Lim,
    N mod Cur =\= 0,
    Nxt is Cur + 2,
    odd_division_sequence_(Nxt,Lim,N).



%TASK 7:
not_golomb([]).
not_golomb([X|Xs]):-
    X =\= 0;
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
/**** another way for not increasing list:
not_increasing_list_(Xs):-
    append(_,[A|Rest],Xs),
	member(B,Rest),
	B =< A.
****/


%TASK 8:
golomb(N,Max,Xs):-
    N > 0,
    Max > 0,
    RestLength is N - 1,
    NewMax is Max - RestLength + 1,
    make_valid_list_(1,NewMax,RestLength,[],[0],GolombList),
    Xs = GolombList.

/****
this function recives the following:
Min = minumum number X can be
Max = maximum number X can be
N = remaining length of the list (which already includes first element 0)
DiffList = list of the differences between X and every number already on the list
Acc = accumulator of elements in the list, stats as [0]
Res = output list which will be a golomb ruler
****/
make_valid_list_(_,_,0,_,Res,Res).
make_valid_list_(Min,Max,N,DiffList,Acc,Res):-
    Min =< Max,
    between(Min,Max,X),
    NewMin is X + 1,
    NewMax is Max + 1,
    NewN is N - 1,
    %before adding the new element, check the difference between it and every number already on Acc
    difference_list_(Acc,X,DiffList,NewDiffList),
    append(Acc,[X],NewAcc),
    make_valid_list_(NewMin,NewMax,NewN,NewDiffList,NewAcc,Res).

difference_list_([],_,Res,Res).
difference_list_([X|Xs],Y,Acc,Res):-
    Diff is Y - X,
    %before adding the difference, check if such difference doesnt already exist
    no_repeats_(Diff,Acc),
    append(Acc,[Diff],NewAcc),
    difference_list_(Xs,Y,NewAcc,Res).

%equivalent to "not member"
no_repeats_(_,[]).
no_repeats_(X,[Y|Rest]):-
    X =\= Y,
    no_repeats_(X,Rest).



%TASK 9:
evaluate([],0).
evaluate([E|[]],E).
evaluate(Exp, Val):-
    valid_exp_list_(Exp), % check input is valid.
    evaluate_times_(Exp,[],ExpNoTimes), %evaluates multiplications first.
    evaluate_plus_(ExpNoTimes,0,Val).

valid_exp_list_([E|[]]):-
    number(E).
valid_exp_list_([E,Op|Rest]):-
    number(E),
    (Op = '*' ; Op = '+'),
    valid_exp_list_(Rest).

evaluate_times_([],Res,Res).
evaluate_times_([E1,Op,E2|[]],Acc,Res):- % evaluate last three expressions.
    (   Op = '*',
        E12 is E1 * E2,
        append(Acc,[E12],NewAcc),
        evaluate_times_([],NewAcc,Res)
    );
    (   Op = '+',
        append(Acc,[E1,Op,E2],NewAcc),
        evaluate_times_([],NewAcc,Res)
    ).

evaluate_times_([E1,Op,E2|Rest],Acc,Res):-
    (   Op = '*',
        E12 is E1 * E2,
        evaluate_times_([E12|Rest],Acc,Res) % replace multiplication with its result.
    );
    (   Op = '+',
        append(Acc,[E1,Op],NewAcc),
        evaluate_times_([E2|Rest],NewAcc,Res) % skipping addition for later.
    ).

evaluate_plus_([],Res,Res).
evaluate_plus_([E|[]],Acc,Res):-
    NewAcc is Acc + E,
    evaluate_plus_([],NewAcc,Res).
evaluate_plus_([E,_|Exp],Acc,Res):-
    NewAcc is Acc + E,
    evaluate_plus_(Exp,NewAcc,Res).



%TASK10:
dom([Row|RestRows]):-
    check_lengths_([Row|RestRows]),          % check that each row and the amound of rows is equal to N.
    flatten_matrix_([Row|RestRows],[],Flat), % turns the matrix into one long list.
    check_numbers_(Flat,1),                  % check that each number between 1 and n^2 is in the matrix.
    check_single_row_sum_(Row,0,Sum),
    check_all_rows_sums_(RestRows,Sum),      % check all rows have the same sum.
    transpose([Row|RestRows],Transposed),
    check_all_rows_sums_(Transposed,Sum),    % check all columns have the same sum.
    reverse([Row|RestRows],RevRowMatrix),    % reverse the order of rows in the matrix.
    get_diagonal_as_list_([Row|RestRows],0,[],Diag1),
    get_diagonal_as_list_(RevRowMatrix,0,[],Diag2),
    check_single_row_sum_(Diag1,0,Sum),
    check_single_row_sum_(Diag2,0,Sum),
    increasing_list_(Diag1),
    increasing_list_(Diag2).

check_lengths_([R|Rows]):-
    length(R,N),
    length([R|Rows],N),
    row_lengths_(Rows,N),
    N > 2.

row_lengths_([],_).
row_lengths_([R|Rows],N):-
    length(R,N),
    row_lengths_(Rows,N).

flatten_matrix_([],Res,Res).
flatten_matrix_([R|Rows],Acc,Res):-
    append(Acc,R,NewAcc),
    flatten_matrix_(Rows,NewAcc,Res).

check_numbers_([],_).
check_numbers_(Flat,Min):-
    append(Start,[N|End],Flat),
    N =:= Min,
    NewMin is Min + 1,
    append(Start,End,NewFlat),
    check_numbers_(NewFlat,NewMin).

check_single_row_sum_([],Res,Res).
check_single_row_sum_([N|RestRow],Acc,Res):-
    NewAcc is Acc + N,
    check_single_row_sum_(RestRow,NewAcc,Res).

check_all_rows_sums_([],_).
check_all_rows_sums_([Row|RestRows],Sum):-
    check_single_row_sum_(Row,0,Sum),
    check_all_rows_sums_(RestRows,Sum).

transpose(Rows,[FirstCol|Cols]):-
    makeRow(Rows,FirstCol,RestRows),
    transpose(RestRows,Cols).
transpose(Rows,[]):-
    nullRows(Rows).
makeRow([],[],[]).
makeRow([[X|RestRow]|Rows],[X|Col],[RestRow|RestRows]):-
    makeRow(Rows,Col,RestRows).
nullRows([[]|Rows]):-
    nullRows(Rows).
nullRows([]).

get_diagonal_as_list_([],_,Res,Res).
get_diagonal_as_list_([Row|RestRows],Offset,Acc,Res):-
    length(OffsetList,Offset),
    append(OffsetList,[X|_],Row),
    append(Acc,[X],NewAcc),
    NewOffset is Offset + 1,
    get_diagonal_as_list_(RestRows,NewOffset,NewAcc,Res).

increasing_list_([_|[]]).
increasing_list_([X1,X2|Xs]):-
    X1 < X2;
    increasing_list_([X2|Xs]).
