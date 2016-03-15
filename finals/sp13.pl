% 3
zip([], [], []).
zip([H1 | T1], [H2 | T2], [[H1, H2] | Z]) :- zip(T1, T2, Z).

test(zip([1,2,3], [4,5,6], R)).
test(zip(X, Y, [[1, 3], [2, 4]])).
test(zip(X, [3, 4], R)).
test(zip(X, Y, [])).

% 4a
part([], _, [], []).
part([H | T], P, [H | L], R) :- part(T, P, L, R), H =< P.
part([H | T], P, L, [H | R]) :- part(T, P, L, R), H > P.

test(part([1,2,3,4,5,6,7,8,9], 5, R1, R2)).
test(part([1,3,5], 10, R1, R2)).
test(part(X, 4, [1,2,3], [5,6])).

% 4b
qsort([], []).
qsort([H | T], X) :-
  part(T, H, L, R),
  qsort(L, Ls), qsort(R, Rs),
  append(Ls, [H | Rs], X).
test(qsort([4,3,6,5,4,1,2], X)).

% Run all queries.
main :- forall(test(Q), (Q -> writeln(yes:Q) ; writeln(no:Q))), halt.
