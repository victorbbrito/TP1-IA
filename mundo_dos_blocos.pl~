can(move(Block, p([Oi, Oj]),p([Bi, Bj])),[clear(Block),clear(Bi),clear(Bj),on(Block,p([Oi, Oj]))]) :-
    block(Block), % bloco existe
    block(Bi),
    block(Bj),
    Bi \== Block, % bloco nao pode mover-se para si mesmo
    block(Oi), block(Oj),
    Oi \== Bi, % certificar que bloco nao esta sendo movido para a mesma posição
    Oj \== Bj, % certificar que bloco nao esta sendo movido para a mesma posição
    Block \== Oi,
    size(Block, Sb),
    size(Bi, Si),
    size(Bj, Sj),
    SizeTo is Si + Sj,
    Sb =< SizeTo + 1. % Bloco que ficar acima nao pode ultrapassar o bloco de destino (vacancia)

can(move(Block, p([Oi, Oj]),p(B,B)),[clear(Block), clear(B),on(Block, p([Oi,Oj]))]) :-
    block(Block), % bloco existe
    block(B),
    B \== Block, % bloco nao pode mover-se para si mesmo
    block(Oi), block(Oj),
    Oi \== B, % certificar que bloco nao esta sendo movido para a mesma posição
    Oj \== B,
    size(Block, SizeB),
    size(B, Sb),
    SizeB =< Sb + 2. % estabilidade

can(move(Block, From, To),[clear(Block), clear(To),on(Block, From)]) :-
    block(Block), % bloco existe
    To \== Block, % bloco nao pode mover-se para si mesmo
    From \== To, % certificar que nova posicao é diferente
    Block \== From.

adds(move(X, From, To),[on(X, To), clear(From)]) .

% deletes(Action, Relationships): Action destroy Relationships
deletes(move(X, From, To),[on(X, From), clear(To)]) .

object(X):-
    place(X);
    block(X).

impossible(on(X,X),_).

impossible(on(X,Y), Goals) :-
    member(clear(Y), Goals);
    member(on(X, Y1), Goals), Y1 \== Y;
    member(on(X1,Y), Goals), X1 \== X.

impossible(clear(X), Goals):-
    member(on(_, X), Goals) .

% A means - ends planner with goal regression
plan(State, Goals, Plan) :-
    plan(State, Goals, Plan, _).

plan(State, Goals, [], _) :-
    satisfied(State, Goals).

plan(State, Goals, Plan, _) :-
    append(PrePlan, [Action], Plan),
    select(State, Goals, Goal),
    achieves(Action, Goal),
    can(Action, _), % Ensure Action contains no variables
    preserves(Action, Goals),
    regress(Goals, Action, RegressedGoals),
    plan(State, RegressedGoals, PrePlan).

satisfied(State, Goals) :-
    delete_all(Goals, State, []).

select(State, Goals, Goal) :-
    member(Goal, Goals).

achieves(Action, Goal) :-
    adds(Action, Goals),
    member(Goal, Goals).

preserves(Action, Goals) :-
    deletes(Action, Relations),
    \+ (member(Goal, Relations), member(Goal, Goals)).

regress(Goals, Action, RegressedGoals) :-
    adds(Action, NewRelations),
    delete_all(Goals, NewRelations, RestGoals),
    can(Action, Condition),
    addnew(Condition, RestGoals, RegressedGoals).

addnew([], L, L).

addnew([Goal|_], Goals, _) :-
    impossible(Goal, Goals),
    !,
    fail .

addnew([X|L1], L2, L3) :-
    member(X, L2), !,
    addnew(L1, L2, L3) .

addnew([X|L1], L2, [X|L3]) :-
    addnew(L1, L2, L3) .

delete_all([], _, []) .

delete_all([X|L1], L2, Diff) :-
    member(X, L2), !,
    delete_all(L1, L2, Diff).

delete_all([X|L1], L2, [X|Diff]) :-
    delete_all(L1, L2, Diff).

member(X, [X|_]) .

member(X, [_|T]) :-
    member(X, T).

delete(X, [X| Tail], Tail).

delete(X, [Y| Tail], [Y| Tail1]) :-
    delete(X, Tail, Tail1).

% blocks world
block(a).
block(b).
block(c).
block(d).
place(1).
place(2).
place(3).
place(4).
place(5).
place(6).
size(a, 1).
size(b, 1).
size(c, 2).
size(d, 3).

% estado inicial i1 (situação 1)
state1([clear(3),on(c,p([1,2])), on(b,6),on(a,4), on(d,p([a,b]))]).
final([clear(1), clear(2), clear(3), on(d,p([4,6])), on(c,p([d,d])),
on(a,c),on(b,c)]).
