% The countdown module,

:- module(countdown,
    [
        get_countdown_rpn/3,
        get_countdown_infix/3
    ]
).

:- use_module(rpn_calculator, 
    [
        operator/1,
        get_rpn_result/2
    ]
).
:- use_module(rpn_to_infix, 
    [
        get_infix/2
    ]
).



% ----- Public API ----- %

% Solve countdown which return RPN
get_countdown_rpn(NumList, Target, RPNNotation):-
    get_valid_rpn(NumList, RPNNotation, Target).

% Solve countdown which return infix notation
get_countdown_infix(NumList, Target, InfixNotation):-
    get_countdown_rpn(NumList, Target, RPNNotation),
    get_infix(RPNNotation, InfixNotation).

% ----- End of public API ----- %


% Check if list is sorted in ascending order
is_head([Head|_], Head).

is_sorted([]).
is_sorted([_]).
is_sorted([Head|Tail]):-
    is_sorted(Tail),
    is_head(Tail, SortedListHead),
    Head =< SortedListHead.

% All item inside list is operator
all_operator([]).
all_operator([H|T]):-
    operator(H),
    all_operator(T).

% Get a combination with repetition with combination list
get_operator_combination_with_repetition_list(Len, X):-
    length(X, Len),
    all_operator(X),
    is_sorted(X).
    
% Concatenation of list
% List1 + List2 = List3
concat_list([], List, List).
concat_list([H|List1], X, [H|List3]):-
    concat_list(List1, X, List3).

% Get a concatenated list from number list then operator list
% such that len(numList) = len(operatorList) + 1
% The result will be all number_list + all possible combination of operator
% Input: ([2,7,3], X) // [2,7,3,a,a] [2,7,3,a,s] [2,7,3,a,m] ... [2,7,3,d,d]
get_number_operator_list(NumList, X):-
    length(NumList, NumListLength),
    OperatorListLength is NumListLength-1,
    get_operator_combination_with_repetition_list(OperatorListLength, OperatorList),
    concat_list(NumList, OperatorList, X).

% Get valid RPN notation
% get_valid_rpn(ListOfNumber, RPNform)
%
% get_valid_rpn([2,7,3], [2,7,3,s,m], RPNResult) // [2,7,3,a,a] [2,7,3,a,s] [2,7,3,a,m] ... [2,7,3,d,d]
% get_valid_rpn([2,7,3], [2,7,m,3,s], RPNResult) // [2,7,a,3,a] [2,7,a,3,s] [2,7,a,3,m] ... [2,7,d,3,d]
%
get_valid_rpn(NumList, RPNNotation, RPNResult):-
    get_number_operator_list(NumList, NumOperatorList),
    permutation(NumOperatorList, RPNNotation),
    get_rpn_result(RPNNotation, RPNResult). % if can calculate then the notation is valid





    
    

    
    
    
    
    





