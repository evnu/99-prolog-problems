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

% P14 Duplicate the elements of a list
% ?- dupli([a,b,c,c,d],X).
% X = [a,a,b,b,c,c,c,c,d,d]
dupli([],[]).
dupli([X|Xs], [X,X|Ys]) :- dupli(Xs, Ys).

% P15 Duplicate the elements of a list a given number of times.
% ?- dupli([a,b,c],3,X).
% X = [a,a,a,b,b,b,c,c,c]

dupli(Xs, N, Ys) :-
    dupli(Xs, N, [], Ys).

dupli([], _, Acc, Ys) :-
    lists:reverse(Acc, Ys).
dupli([X|Xs], N, Acc0, Ys) :-
    dupli1(X, N, Acc0, Acc1),
    dupli(Xs, N, Acc1, Ys).

dupli1(_, 0, Acc, Acc) :- !.
dupli1(X, N, Acc0, FinalAcc) :-
    % Not using clpz here, as I want N to be ground. For non-ground N, we'd need to
    % remove the cut above (as a non-ground N always unifies with 0, but that is probably
    % not the answer for our question), and we can run into an endless loop when asking for
    % more than one answer.
    N > 0,
    N1 is N - 1,
    dupli1(X, N1, [X|Acc0], FinalAcc).

% P16 Drop every N'th element from a list.
% ?- drop([a,b,c,d,e,f,g,h,i,k],3,X).
% X = [a,b,d,e,g,h,k]
drop(Xs, N, Ys) :-
    drop(Xs, counter(1, N), [], Ys).

drop([], _, Acc, Ys) :-
    lists:reverse(Acc, Ys).

drop([_|Xs], counter(N, N), Acc, Ys) :-
    !,
    drop(Xs, counter(1, N), Acc, Ys).
drop([X|Xs], counter(C, N), Acc, Ys) :-
    C \= N,
    C1 is C + 1,
    drop(Xs, counter(C1, N), [X|Acc], Ys).

% P17 Split a list into two parts; the length of the first part is given.
% ?- split([a,b,c,d,e,f,g,h,i,k],3,L1,L2).
% L1 = [a,b,c]
% L2 = [d,e,f,g,h,i,k]
split(Xs, N, L1, L2) :-
    split(Xs, N, [], L1, L2).

split(L2, 0, Acc, L1, L2) :-
    !,
    lists:reverse(Acc, L1).
split([X|Xs], N, Acc, L1, L2) :-
    N1 #= N - 1,
    split(Xs, N1, [X|Acc], L1, L2).

% P18 Extract a slice from a list.
% ?- slice([a,b,c,d,e,f,g,h,i,k],3,7,L).
% X = [c,d,e,f,g]
slice(Xs, I, K, L) :-
    K1 #= K - I + 1,
    drop_until(Xs, I, Suffix),
    take_until(Suffix, K1, [], L).

drop_until(Xs, 1, Xs) :-
    !.
drop_until([_|Xs], N, Ys) :-
    N #> 1,
    N1 #= N - 1,
    drop_until(Xs, N1, Ys).

take_until(_, 0, Acc, Ys) :-
    !,
    lists:reverse(Acc, Ys).
take_until([X|Xs], N, Acc, Ys) :-
    N #> 0,
    N1 #= N - 1,
    take_until(Xs, N1, [X|Acc], Ys).

% P19 Rotate a list N places to the left.
% Examples:
% ?- rotate([a,b,c,d,e,f,g,h],3,X).
% X = [d,e,f,g,h,a,b,c]
%
% ?- rotate([a,b,c,d,e,f,g,h],-2,X).
% X = [g,h,a,b,c,d,e,f]

rotate(Xs, N, Ys) :-
    N #> 0,
    rotate_pos(Xs, N, [], Ys).

rotate(Xs, N, Ys) :-
    N #< 0,
    lists:reverse(Xs, Xs1),
    rotate_neg(Xs1, N, [], Ys1),
    lists:reverse(Ys1, Ys).

rotate_pos(Xs, 0, L, Ys) :-
    lists:reverse(L, L1),
    lists:append(Xs, L1, Ys).
rotate_pos([X|Xs], N, L, Ys) :-
    N #> 0,
    N1 #= N - 1,
    rotate_pos(Xs, N1, [X|L], Ys).

rotate_neg(Xs, 0, L, Ys) :-
    lists:reverse(L, L1),
    lists:append(Xs, L1, Ys).
rotate_neg([X|Xs], N, L, Ys) :-
    N #< 0,
    N1 #= N + 1,
    rotate_neg(Xs, N1, [X|L], Ys).

% P20 (*) Remove the K'th element from a list.
%     Example:
%     ?- remove_at(X,[a,b,c,d],2,R).
%     X = b
%     R = [a,c,d]
remove_at(X, Xs, N, R) :-
    remove_at(X, Xs, N, [], R).

remove_at(X, [Y|Ys], N, Acc, R) :-
    N #> 1,
    !,
    N1 #= N - 1,
    remove_at(X, Ys, N1, [Y|Acc], R).
remove_at(X, [X|R], 1, Acc, Ys) :-
    lists:reverse(Acc, Acc1),
    lists:append(Acc1, R, Ys).

% P21 (*) Insert an element at a given position into a list.
%     Example:
%     ?- insert_at(alfa,[a,b,c,d],2,L).
%     L = [a,alfa,b,c,d]
insert_at(E, [X|Xs], N, [X|Ys]) :-
    N #> 1,
    !,
    N1 #= N - 1,
    insert_at(E, Xs, N1, Ys).
insert_at(E, Xs, 1, [E|Xs]).

% P22 (*) Create a list containing all integers within a given range.
%     Example:
%     ?- range(4,9,L).
%     L = [4,5,6,7,8,9]
range(From, To, [From|Ns]) :-
    From #< To,
    From1 #= From + 1,
    range(From1, To, Ns).
range(N, N, [N]).
