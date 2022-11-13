/*
Calculates the math expression in Reverse polish notation or postfix notation
Where
a = +
s = -
m = *
d = /

Infix notation: 2 * (3 + 5)
Input: 2 3 5 a m
Ouput: 16

Infix notation: 2 * (7 - 3)
Input: 2 7 3 s m
Output: 8

Infix notation: (17 - 3) / 7
Input: 17 3 - 7 /
Output: 2

Infix notation: ((8 - 5) * ((4 + 2) / 3 ))
Input: 8 5 s 4 2 a 3 d m 
Ouput: 6
*/

% Predicate for making this .pl file into a module
% module(nameOfFile(by convention), listOfPublicPredicate)
:- module(rpn_calculator,
    [
        operator/1,
        get_rpn_result/2
    ]
).

% --- Public API for this module --- %

operator("a").    % Add      or '+' 
operator("s").    % Subtract or '-'
operator("m").    % Multiply or '*'
operator("d").    % Divide   or '/'

get_rpn_result(RPNNotation, Result):-
    calculate_rpn(Result, [], RPNNotation).

% --- End of Public API for this module --- %

% Helper stack predicate
% Push X onto the stack
push(X, S, [X|S]).

% Pop X off the stack
pop([X|S], X, S).


% calculate_rpn(Ans, stack_operator, rpn)
% calculate_rpn(8, [8], [])
% calculate_rpn(8, [], [2,7,3,s,m])
calculate_rpn(Result, [Result], []):- !.

/*
Incoming infix token is number
Example cases:
calculate_rpn(8, [], [2|7,3,s,m]):-
    number(2),
    push(8, [], [8]),
    !,
    calculate_rpn(8, [2], [7,3,s,m]).
-----------
calculate_rpn(8, [7,2], [3|s,m]):-
    number(3),
    push(3, [7,2], [3,7,2]),
    !,
    calculate_rpn(8, [3,7,2], [s,m]).
*/
calculate_rpn(R, S, [Num|T]):-
    number(Num),
    push(Num, S, PushedStack),
    !,
    calculate_rpn(R, PushedStack, T).

% Incoming infix token is "a" (which is addition or '+')
% (Explaination is better for subtract case)
calculate_rpn(X, S, [Operator|T]):-
    operator(Operator) = operator("a"),
    pop(S, Operand1, S1),
    pop(S1, Operand2, S2),
    Operated_result is Operand2 + Operand1,
    !,
    calculate_rpn(X, [Operated_result|S2], T).

/*
Incoming infix token is "s" (which is substract or '-')
Example case:
calculate_rpn(8, [3,7,2], [s|m]):-
    s = operator(s),
    pop([3,7,2], 3, [7,2]),
    pop([7,2], 7, [2]),
    Operator_result is 7 - 3,
    calculate_rpn(8, [Operator_result|2], [m]).
*/
calculate_rpn(X, S, [Operator|T]):-
    operator(Operator) = operator("s"),
    pop(S, Operand1, S1),
    pop(S1, Operand2, S2),
    Operated_result is Operand2 - Operand1,
    !,
    calculate_rpn(X, [Operated_result|S2], T).

% Incoming infix token is "m" (which is multiple or '*')
calculate_rpn(X, S, [Operator|T]):-
    operator(Operator) = operator("m"),
    pop(S, Operand1, S1),
    pop(S1, Operand2, S2),
    Operated_result is Operand2 * Operand1,
    !,
    calculate_rpn(X, [Operated_result|S2], T).

% Incoming infix token is "d" (which is multiple or '/')
calculate_rpn(X, S, [Operator|T]):-
    operator(Operator) = operator("d"),
    pop(S, Operand1, S1),
    pop(S1, Operand2, S2),
    Operand1 \= 0,
    Operated_result is Operand2 / Operand1,
    !,
    calculate_rpn(X, [Operated_result|S2], T).














    




