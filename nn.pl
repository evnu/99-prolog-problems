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

% P09
% The first variant does not use an accumulator and suffers performance-wise:
% lists with >20 elements are very slow to pack. pack/2 uses an accumulator and
% does not suffer as much.
%
% Example:
% ?- pack([a,a,a,a,b,c,c,a,a,d,e,e,e,e],X).
% X = [[a,a,a,a],[b],[c,c],[a,a],[d],[e,e,e,e]]
pack([], []).
pack([X], [[X]]) :- atom(X).
pack([X|Xs], [[X,X|S]|Ys]) :- pack(Xs, [[X|S]|Ys]).
pack([X|Xs], [[X],[O|S]|Ys]) :-
    pack(Xs, [[O|S]|Ys]),
    % It is imperative to check after the recursive call; Otherwise, the clause does
    % not work. O must be instantiated for the equality check to work properly.
    X \= O.

% pack with an accumulator

pack2([X|Xs], Ys) :- pack2(Xs, [[X]], Ys).

pack2([], Acc, Ys) :- lists:reverse(Acc, Ys).
pack2([X|Xs], [[X|Acc]|AccR], Ys) :-
    pack2(Xs, [[X,X|Acc]|AccR], Ys).
pack2([X|Xs], [[Y|Acc]|AccR], Ys) :-
    X \= Y,
    pack2(Xs, [[X], [Y|Acc]|AccR], Ys).

% This can be used to generate lists from a set of elements at random:
rmember(Els, El) :- lists:member(El, Els).
% -> Els = [a,b,c], N = 3, lists:length(Xs, N), lists:maplist(rmember(Els), Xs).
% With that, benchmarking pack/2 and pack2/2 is simple (the time module is needed):
% ?- Els = [a,b,c], N = 20, lists:length(Xs, N),
% lists:maplist(rmember(Els), Xs),
% time(pack(Xs, Ys1)),
% time(pack2(Xs, Ys2)).

% P10
% ?- encode([a,a,a,a,b,c,c,a,a,d,e,e,e,e],X).
% X = [[4,a],[1,b],[2,c],[2,a],[1,d][4,e]]
encode(Xs, Ys) :-
    pack2(Xs, Ys1),
    encode1(Ys1, Ys).

encode1([], []).
encode1([Run|Rest], [[Length, Element]|Ys]) :-
    lists:length(Run, Length),
    [Element | _] = Run,
    encode1(Rest, Ys).

% P11
% ?- encode_modified([a,a,a,a,b,c,c,a,a,d,e,e,e,e],X).
% X = [[4,a],b,[2,c],[2,a],d,[4,e]]
encode_modified(Xs, Ys) :-
    pack2(Xs, Ys1),
    encode_modified1(Ys1, Ys).

encode_modified1([], []).
encode_modified1([Run|Rest], [[Length, Element]|Ys]) :-
    lists:length(Run, Length),
    Length > 1,
    [Element | _] = Run,
    encode_modified1(Rest, Ys).
encode_modified1([[Element]|Rest], [Element|Ys]) :-
    encode_modified1(Rest, Ys).

% P12
decode_modified([], []).
decode_modified([[N, X]|Xs], [X|Ys]) :-
    N > 0,
    N1 is N - 1,
    decode_modified([[N1, X]|Xs], Ys).
decode_modified([[0, _]|Xs], Ys) :- decode_modified(Xs, Ys).
decode_modified([X|Xs], [X|Ys]) :- atom(X), decode_modified(Xs, Ys).

% P13
% ?- encode_direct([a,a,a,a,b,c,c,a,a,d,e,e,e,e],X).
% X = [[4,a],b,[2,c],[2,a],d,[4,e]]

encode_direct([X|Xs], Ys) :-
    encode_direct(Xs, [[X,1]], Ys).
encode_direct([], []).

encode_direct([X|Xs], [[X,N]|Acc], Ys) :-
    N1 is N + 1,
    encode_direct(Xs, [[X,N1]|Acc], Ys).
encode_direct([X|Xs], [[Y,N]|Acc], Ys) :-
    X \= Y,
    encode_direct(Xs, [[X,1],[Y,N]|Acc], Ys).
encode_direct([], Acc, Ys) :-
    encode_direct_reverse(Acc, [], Ys).

encode_direct_reverse([[X,N]|Xs], Acc, Ys) :-
    N > 1,
    encode_direct_reverse(Xs, [[X,N]|Acc], Ys).

encode_direct_reverse([[X,1]|Xs], Acc, Ys) :-
    encode_direct_reverse(Xs, [X|Acc], Ys).

encode_direct_reverse([], Acc, Acc).
