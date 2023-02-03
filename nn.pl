% vim: set ft=prolog:
% https://www.ic.unicamp.br/~meidanis/courses/mc336/2009s2/prolog/problemas/

% P01
my_last(X, [X]).
my_last(Y, [_X|Xs]) :- my_last(Y, Xs).

% P03
% Note: Due to is/2 (which is function-like), we cannot ask
% ?- element_at(a, [a, b, a], X).
element_at(E, [E|_], 0).
element_at(E, [_|Xs], K0) :-
    K0 > 0,
    K1 is K0 - 1,
    element_at(E, Xs, K1).

% A more general P03 with CLP(Z). With that, we can ask
% ?- element_at_clpz(a, [a, b, a], X).
:- use_module(library(clpz)).
element_at_clpz(E, [E|_], 0).
element_at_clpz(E, [_|Xs], K0) :-
    K0 #= K1 + 1,
    element_at_clpz(E, Xs, K1).

% P04
num_els(0, []).
num_els(K, [_|Xs]) :- K #= K0+1, num_els(K0, Xs).

% P05
reverse_list(Xs, Ys) :- reverse_list(Xs, [], Ys).

reverse_list([], Acc, Acc).
reverse_list([X|Xs], Acc, Ys) :- reverse_list(Xs, [X|Acc], Ys).

% P06
palindrome(Xs, Ys) :- Xs = Ys, reverse_list(Xs, Ys).

% P08
compress([], []).
compress([X|Xs], [X|Ys]) :- compress(Xs, X, Ys).

compress([], _Current, []).
compress([Current|Xs], Current, Ys) :-
    compress(Xs, Current, Ys).
compress([X|Xs], Current, [X|Ys]) :- X \= Current, compress(Xs, X, Ys).

