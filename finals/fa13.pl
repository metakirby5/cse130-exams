% 5a
link(san_diego, seattle).
link(seattle, dallas).
link(dallas, new_york).
link(new_york, chicago).
link(new_york, seattle).
link(chicago, boston).
link(boston, san_diego).

path_2(A, B) :- link(A, C), link(C, B).

% 5b
path_3(A, B) :- path_2(A, C), link(C, B).

% 5c
path_N(A, B, N) :- N = 1, link(A, B).
path_N(A, B, N) :- N > 1, M is N - 1, path_N(A, C, M), link(C, B).

% 5d
path(A, B) :- path_helper(A, B, [A]).
path_helper(A, B, Seen) :- link(A, B), not(member(B, Seen)).
path_helper(A, B, Seen) :-
  link(A, C),
  not(member(C, Seen)),
  path_helper(C, B, [C | Seen]).
