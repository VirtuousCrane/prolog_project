// ------------------- THE PROLOG PROGRAM --------------------------------
var program = String.raw`
:- use_module(library(lists)).
:- use_module(library(dom)).

operator("a").
operator("s").
operator("m").
operator("d").

get_rpn_result(RPNNotation, Result):-
    calculate_rpn(Result, [], RPNNotation).
push(X, S, [X|S]).

pop([X|S], X, S).


calculate_rpn(Result, [Result], []):- !.
calculate_rpn(R, S, [Num|T]):-
    number(Num),
    push(Num, S, PushedStack),
    !,
    calculate_rpn(R, PushedStack, T).

calculate_rpn(X, S, [Operator|T]):-
    operator(Operator) = operator("a"),
    pop(S, Operand1, S1),
    pop(S1, Operand2, S2),
    Operated_result is Operand2 + Operand1,
    !,
    calculate_rpn(X, [Operated_result|S2], T).

calculate_rpn(X, S, [Operator|T]):-
    operator(Operator) = operator("s"),
    pop(S, Operand1, S1),
    pop(S1, Operand2, S2),
    Operated_result is Operand2 - Operand1,
    !,
    calculate_rpn(X, [Operated_result|S2], T).

calculate_rpn(X, S, [Operator|T]):-
    operator(Operator) = operator("m"),
    pop(S, Operand1, S1),
    pop(S1, Operand2, S2),
    Operated_result is Operand2 * Operand1,
    !,
    calculate_rpn(X, [Operated_result|S2], T).

calculate_rpn(X, S, [Operator|T]):-
    operator(Operator) = operator("d"),
    pop(S, Operand1, S1),
    pop(S1, Operand2, S2),
    Operand1 \= 0,
    Operated_result is Operand2 / Operand1,
    !,
    calculate_rpn(X, [Operated_result|S2], T).

get_infix(RPN, Infix):-
    rpn_to_infix(RPN, [], Infix).

rpn_to_infix([], [Result], Result).

rpn_to_infix([HeadRPN|RestOfRPN], Stack, Result):-
    number(HeadRPN),
    !,
    rpn_to_infix(RestOfRPN, [HeadRPN|Stack], Result).

rpn_to_infix([HeadRPN|RestOfRPN], Stack, Result):-
    rpn_calculator:operator(HeadRPN),
    pop(Stack, X, S1),
    pop(S1, Y, S2),
    push([Y,HeadRPN,X], S2, S3),
    !,
    rpn_to_infix(RestOfRPN, S3, Result).


get_countdown_rpn(NumList, Target, RPNNotation):-
    get_valid_rpn(NumList, RPNNotation, Target).

get_countdown_infix(NumList, Target, InfixNotation):-
    get_countdown_rpn(NumList, Target, RPNNotation),
    get_infix(RPNNotation, InfixNotation).

all_operator([]).
all_operator([H|T]):-
    operator(H),
    all_operator(T).

get_operator_combination_with_repetition_list(Len, X):-
    length(X, Len), % produce list of holding variable such as [_, _, _,]
    all_operator(X),
    msort(X, X). % Check if list is sorted, does not remove duplicates

concat_list([], List, List).
concat_list([H|List1], X, [H|List3]):-
    concat_list(List1, X, List3).

get_number_operator_list(NumList, X):-
    length(NumList, NumListLength),
    OperatorListLength is NumListLength-1,
    get_operator_combination_with_repetition_list(OperatorListLength, OperatorList),
    concat_list(NumList, OperatorList, X).

get_valid_rpn(NumList, RPNNotation, RPNResult):-
    get_number_operator_list(NumList, NumOperatorList),
    permutation(NumOperatorList, RPNNotation),
    get_rpn_result(RPNNotation, RPNResult).
        `;
// -------------------------------- END ----------------------------------

// --------------------------- GAME LOGIC --------------------------------
// Variable Initialization
var input_1 = document.getElementById("inp-1");
var input_2 = document.getElementById("inp-2");
var input_3 = document.getElementById("inp-3");
var input_4 = document.getElementById("inp-4");
var input_list = [input_1, input_2, input_3, input_4];

var ans_div = document.querySelector("#answer");
var target = document.getElementById("target");
var submit_form = document.getElementById("submit-form");
var more_answer_form = document.getElementById("more-answer-form");
var answer_list = [];

submit_form.addEventListener("submit", function(f) {
    f.preventDefault(); // Prevents form from being submitted
    get_answer();
});

more_answer_form.addEventListener("submit", function(f) {
    f.preventDefault();
    get_more_answer();
});

// Tau Prolog logic
const QUERY_LIMIT = 10000000;
var session = pl.create(QUERY_LIMIT);
var more_answer_btn = document.querySelector("#more-answer");
more_answer_btn.style.display = "none";

function get_answer() {
    // Generating the question string
    var query_string = "[";
    var prolog_query = "get_countdown_infix(QUERY).";

    for (let i = 0; i < input_list.length; i++) {
        query_string += input_list[i].value;
        if (i != input_list.length - 1) {
            query_string += ",";
        }
    }

    query_string += "],";
    query_string += target.value;
    query_string += ",X";

    prolog_query = prolog_query.replace("QUERY", query_string);
    console.log(prolog_query);

    session.consult(program,{
        success: function() {
            console.log("SUCCESS");
            session.query(prolog_query, {
                success: function(goal) {
                    console.log("QUERY SUCCESS");
                    session.answer({
                        success: function(ans) {
                            console.log("ANSWER SUCCESS");
                            answer_list = [];
                            let ans_string = session.format_answer(ans);
                            ans_string = ans_string.replaceAll("[a]", "+");
                            ans_string = ans_string.replaceAll("[s]", "-");
                            ans_string = ans_string.replaceAll("[m]", "*");
                            ans_string = ans_string.replaceAll("[d]", "/");
                            ans_string = ans_string.replaceAll("[", "(");
                            ans_string = ans_string.replaceAll("]", ")");
                            ans_string = ans_string.replaceAll(",", "");

                            ans_div.innerHTML = ans_string;
                            answer_list.push(ans_string);
                            more_answer_btn.style.display = "block";
                        },
                        error: function(err) {
                            console.log("ERROR");
                            console.log(err);
                            ans_div.innerHTML = "ERROR";
                        },
                    });
                },
                error: function(err) {
                    console.log("QUERY FAILED");
                    console.log(err);
                    ans_div.innerHTML = "ERROR";
                },
            });
        }, error: function(err) {
            console.log(err);
            ans_div.innerHTML = "ERROR";
        },
    });
}

function get_more_answer() {
    session.answer({
        success: function(ans) {
            console.log("ANSWER SUCCESS");
            let ans_string = session.format_answer(ans);
            ans_string = ans_string.replaceAll("[a]", "+");
            ans_string = ans_string.replaceAll("[s]", "-");
            ans_string = ans_string.replaceAll("[m]", "*");
            ans_string = ans_string.replaceAll("[d]", "/");
            ans_string = ans_string.replaceAll("[", "(");
            ans_string = ans_string.replaceAll("]", ")");
            ans_string = ans_string.replaceAll(",", "");

            if (answer_list.includes(ans_string)) {
                get_more_answer();
            }
            ans_div.innerHTML = ans_string;
        },
        error: function(err) {
            console.log("ERROR");
            console.log(err);
            ans_div.innerHTML = "ERROR";
        },
    });
}