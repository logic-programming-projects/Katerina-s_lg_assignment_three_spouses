% ----
% Задача: Три подружжя (Jealous husbands) — в Prolog (SWI / SWISH)
% ----
% 3 пари (h1-w1, h2-w2, h3-w3) мають переправитись через річку.
% Човен вміщує 1 або 2 особи.
% Умова безпеки: жодна дружина не може бути на березі з чужими чоловіками,
% якщо її власного чоловіка на цьому березі немає.
%
% Реалізовано (з коректним уникненням циклів):
%  - BFS з visited як список (чистий підхід)
%  - BFS з visited через dynamic pred (side-effects)
%  - DFS з обмеженням глибини
%  - друк рішення + порівняння часу
% ----

:- dynamic visited/1.

% ----
% Дані
% ----

people([h1,h2,h3,w1,w2,w3]).

% Початковий та цільовий стани (визначаємо одразу)
initial_state(S) :-
    people(P),
    canon_state(state(left, P, []), S).

goal_state(S) :-
    people(P),
    canon_state(state(right, [], P), S).

% Пари
couple(h1,w1).
couple(h2,w2).
couple(h3,w3).

husband(h1). husband(h2). husband(h3).
wife(w1). wife(w2). wife(w3).

% ----
% Визначення стану: сортуємо береги, щоб уникнути дублікатів через порядок
% ----

canon_state(state(Boat, L, R), state(Boat, LS, RS)) :-
    sort(L, LS),
    sort(R, RS).

% ----
% Безпека
% ----
% Берег безпечний, якщо:
%  - немає чоловіків, або
%  - немає дружин, або
%  - для кожної дружини на березі є її чоловік

safe_bank(Bank) :-
    findall(H, (member(H,Bank), husband(H)), Men),
    findall(W, (member(W,Bank), wife(W)), Women),
    ( Men = [] ->
        true
    ; Women = [] ->
        true
    ; \+ (
          member(W, Bank),
          wife(W),
          couple(H, W),
          \+ member(H, Bank)
        )
    ).

safe_state(state(_, L, R)) :-
    safe_bank(L),
    safe_bank(R).

% ----
% Вибір пасажирів (1 або 2) з уникненням повторів
% ----

passengers(Bank, [P]) :-
    member(P, Bank).

passengers(Bank, [P1,P2]) :-
    select(P1, Bank, Rest),
    member(P2, Rest),
    P1 @< P2.

% ----
% Переправлення людей між берегами
% ----

move_list([], From, From).
move_list([P|Ps], From, To) :-
    select(P, From, From1),
    move_list(Ps, From1, To).

add_list(Ps, Bank, NewBank) :-
    append(Ps, Bank, Tmp),
    sort(Tmp, NewBank).

% ----
% Перехід (neighbor): один крок човна
% ----
% Важливо: після руху канонізуємо стан (sort), щоб visited працював коректно.

neighbor(state(left, L, R), Next) :-
    passengers(L, Ps),
    move_list(Ps, L, L1),
    add_list(Ps, R, R1),
    canon_state(state(right, L1, R1), Next),
    safe_state(Next).

neighbor(state(right, L, R), Next) :-
    passengers(R, Ps),
    move_list(Ps, R, R1),
    add_list(Ps, L, L1),
    canon_state(state(left, L1, R1), Next),
    safe_state(Next).

% ----
% Метод 1: BFS (visited як список станів) — чистий
% ----
% На відміну від перевірки тільки в межах шляху, тут є справжній global visited
% (передається параметром), тому BFS не роздувається повторними станами.

solve_bfs(Path) :-
    initial_state(I),
    bfs_list([[I]], [I], Rev),
    reverse(Rev, Path).

bfs_list([[S|Rest]|_], _Visited, [S|Rest]) :-
    goal_state(G),
    S = G, !.

bfs_list([[S|Rest]|Queue], Visited, Sol) :-
    findall([N,S|Rest],
        ( neighbor(S,N),
          \+ member(N, Visited)
        ),
        NewPaths),
    states_heads(NewPaths, NewStates),
    append(Visited, NewStates, Visited1),
    append(Queue, NewPaths, Queue1),
    bfs_list(Queue1, Visited1, Sol).

states_heads([], []).
states_heads([[S|_]|Ps], [S|Ss]) :-
    states_heads(Ps, Ss).

% ----
% Метод 2: BFS (dynamic visited/1) — з побічними ефектами
% ----

init_visited :- retractall(visited(_)).
mark_visited(S) :- assertz(visited(S)).
was_visited(S) :- visited(S).

solve_bfs_dynamic(Path) :-
    init_visited,
    initial_state(I),
    mark_visited(I),
    bfs_dyn([[I]], Rev),
    reverse(Rev, Path).

bfs_dyn([[S|Rest]|_], [S|Rest]) :-
    goal_state(G),
    S = G, !.

bfs_dyn([[S|Rest]|Queue], Sol) :-
    findall([N,S|Rest],
        ( neighbor(S,N),
          \+ was_visited(N),
          mark_visited(N)
        ),
        NewPaths),
    append(Queue, NewPaths, Queue1),
    bfs_dyn(Queue1, Sol).

% ----
% Метод 3: DFS з обмеженням глибини (для демонстрації)
% ----

solve_dfs(Path) :-
    initial_state(I),
    dfs_limited(I, [I], 30, Rev),
    reverse(Rev, Path).

dfs_limited(S, Path, _Depth, Path) :-
    goal_state(G),
    S = G, !.

dfs_limited(S, Path, Depth, Sol) :-
    Depth > 0,
    neighbor(S, N),
    \+ member(N, Path),
    Depth1 is Depth - 1,
    dfs_limited(N, [N|Path], Depth1, Sol).

% ----
% Друк рішення
% ----

count_steps(Path, Steps) :-
    length(Path, Len),
    Steps is Len - 1.

print_solution(Path) :-
    write('--- Розв''язок ---'), nl,
    print_path(Path, 0),
    count_steps(Path, Steps),
    format('~nЗагальна кількість кроків: ~w~n', [Steps]).

print_path([], _).
print_path([State|Rest], N) :-
    format('~nКрок ~w:~n', [N]),
    print_state(State),
    N1 is N + 1,
    print_path(Rest, N1).

print_state(state(Boat, Left, Right)) :-
    format('  Човен: ~w~n', [Boat]),
    format('  Лівий берег:  ~w~n', [Left]),
    format('  Правий берег: ~w~n', [Right]).

% ----
% Порівняння методів (час + кроки)
% ----

compare_methods :-
    write('--- Порівняння методів ---'), nl, nl,

    write('Метод 1: BFS (visited як список)'), nl,
    statistics(runtime, [T0|_]),
    ( solve_bfs(P1) ->
        statistics(runtime, [T1|_]),
        Time1 is T1 - T0,
        count_steps(P1, S1),
        format('Час: ~w мс~nКроків: ~w~n~n', [Time1, S1])
    ; write('Розв''язок не знайдено'), nl, nl
    ),

    write('Метод 2: BFS (dynamic visited/1)'), nl,
    statistics(runtime, [T2|_]),
    ( solve_bfs_dynamic(P2) ->
        statistics(runtime, [T3|_]),
        Time2 is T3 - T2,
        count_steps(P2, S2),
        format('Час: ~w мс~nКроків: ~w~n~n', [Time2, S2])
    ; write('Розв''язок не знайдено'), nl, nl
    ),

    write('Метод 3: DFS (обмеження глибини)'), nl,
    statistics(runtime, [T4|_]),
    ( solve_dfs(P3) ->
        statistics(runtime, [T5|_]),
        Time3 is T5 - T4,
        count_steps(P3, S3),
        format('Час: ~w мс~nКроків: ~w~n~n', [Time3, S3])
    ; write('Розв''язок не знайдено (треба збільшити Depth)'), nl, nl
    ).

% ----
% Швидкий запуск
% ----

go :-
    solve_bfs(Path),
    print_solution(Path),
    nl, compare_methods.



% ----
% Запити
% ----
% 1) BFS (visited як список) + друк:
% ?- solve_bfs(Path), print_solution(Path).
%
% 2) BFS (dynamic visited/1) + друк:
% ?- solve_bfs_dynamic(Path), print_solution(Path).
%
% 3) DFS (обмеження глибини) + друк:
% ?- solve_dfs(Path), print_solution(Path).
%
% 4) Порівняння трьох методів:
% ?- compare_methods.
%
% 5) Один запуск - все одразу:
% ?- go.
% ----