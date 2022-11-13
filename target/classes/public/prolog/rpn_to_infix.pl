/* This module is for converting valid RPN to infix notation

Input (RPN): 2 7 3 s m
Output (Infix): (2 * (7 - 3))
Calculated notation: 8


Input: 8 5 s 4 2 a 3 d m 
Output: ((8 - 5) * ((4 + 2) / 3 ))
Calculated notation: 6
*/
:- module(rpn_to_infix,
    [
        get_infix/2
    ]
).

:- use_module(rpn_calculator, 
    [
        operator/1
    ]
).

% --- Public API --- %

get_infix(RPN, Infix):-
    rpn_to_infix(RPN, [], Infix).

% --- End of Public API --- %

% Helper stack function
% Push X onto stack
push(X, Stack, [X|Stack]).
pop([X|Stack], X, Stack).

rpn_to_infix([], [Result], Result).

% When incoming RPN token is an operand (number)
% rpn_to_infix([3|s,m], [3|7,2])
rpn_to_infix([HeadRPN|RestOfRPN], Stack, Result):-
    number(HeadRPN),
    !,
    rpn_to_infix(RestOfRPN, [HeadRPN|Stack], Result).

% When incoming RPN token is an operator (add, subtract, multiply, divide)
rpn_to_infix([HeadRPN|RestOfRPN], Stack, Result):-
    rpn_calculator:operator(HeadRPN),
    pop(Stack, X, S1),
    pop(S1, Y, S2),
    push([Y,HeadRPN,X], S2, S3),
    !,
    rpn_to_infix(RestOfRPN, S3, Result).
    


