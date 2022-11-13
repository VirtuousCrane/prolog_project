var session = pl.create();

session.consult("                           \
    :- use_module(library(lists)).          \
                                            \
    fruit(apple). fruit(pear). fruit(banana).\
                                                \
    fruits_in(Xs, X) :- member(X, Xs), fruit(X).\
    ",{
    success: function() {
        console.log("Success");
    },
    error: function(err) {
        console.log("Error");
    }
});

session.query("fruits_in([carrot, apple, banana, broccoli], X).", {
    success: function(goal) {
        console.log(goal);
    },
    error: function(err) {
        console.log(err);
    },
})

session.answer({
    success: function(answer) {
        console.log(answer.links.X);
    },
    error: function(err) {
        console.log("ERROR");
    }
})