% Name: M.M. Drozdzynski
% College: Kings
% CRSID: md481

colour(red).
colour(blue).
colour(green).
colour(yellow).

% lessthan(+A,+B)
lessthan(A=C1, B=C2) :- colour(C1), colour(C2), A @< B. % using @< to enfore standard order of terms (alphabetical)

:- lessthan(a=red, b=blue).
:- not(lessthan(b=blue, a=red)).
:- not(lessthan(a=red, b=blues)).
:- not(lessthan(a=red, b=blues)).
:- not(lessthan(a=reds, b=blues)).

% sorts the list by dividing it two parts, soring them and then merging together
msort_colours([],[]).
msort_colours([A],[A]).
msort_colours([A,B|Rest],S) :-
  divide([A,B|Rest],L1,L2),
  msort_colours(L1,S1),
  msort_colours(L2,S2),
  merge_lists(S1,S2,S).
  
% divide a list by into two by putting the first element
% into one list, the second into the other and then
% dividing the tail
divide([],[],[]).
divide([A],[A],[]).
divide([A,B|R], [A|Ra], [B|Rb]) :-
  divide(R,Ra,Rb).
  
merge_lists([], S, S).
merge_lists(S, [], S).
merge_lists([H1|T1], [H2|T2], [H1|R]) :- lessthan(H1, H2), merge_lists(T1, [H2|T2], R).
merge_lists([H1|T1], [H2|T2], [H2|R]) :- lessthan(H2, H1), merge_lists(T2, [H1|T1], R).

:- merge_lists([], [], []).
:- merge_lists([a=red], [], [a=red]).
:- merge_lists([], [a=red], [a=red]).
:- merge_lists([a=red], [b=red], [a=red, b=red]).
:- merge_lists([b=red], [a=red], [a=red, b=red]).
:- merge_lists([a=red, c=red], [b=red], [a=red, b=red, c=red]).

:- msort_colours([], []).
:- msort_colours([a=red, b=red], [a=red, b=red]).
:- msort_colours([b=red, a=red], [a=red, b=red]).
:- msort_colours([c=red, b=red, a=red], [a=red, b=red, c=red]).

get_with_default(Key, [K=V|_], _, Value) :- Key = K, Value = V.
get_with_default(Key, [K=_|T], Default, Value) :- Key \= K, get_with_default(Key, T, Default, Value).
get_with_default(_, [], Default, Value) :- Value = Default.

:- get_with_default(a, [a=red], blue, red).
:- get_with_default(a, [b=red], blue, blue).
:- get_with_default(a, [], blue, blue).
:- not(get_with_default(a, [a=red], blue, blue)).

remove(_, [], []).
remove(Item, [H|T], ListOut) :- Item = H, ListOut = T.
remove(Item, [H|T], ListOut) :- Item \= H, remove(Item, T, R), ListOut = [H|R].

:- remove(1, [1,2,3], [2,3]).
:- remove(1, [2,1,3], [2,3]).
:- remove(1, [3,2,1], [3,2]).
:- remove(4, [1,2,3], [1,2,3]).

graph(example1, [a-b, a-c, a-d, b-c, c-d, c-e, d-e]).
graph(example2, ['WA'-'water', 'WA'-'NT', 'WA'-'SA', 'NT'-'water', 'NT'-'QLD', 'NT'-'SA', 'SA'-'water', 'QLD'-'water', 'QLD'-'NSW', 'QLD'-'SA', 'NSW'-'water', 'NSW'-'SA', 'NSW'-'VIC', 'NSW'-'ACT', 'VIC'-'water', 'VIC'-'SA', 'TAS'-'water']).

unroll([], []).
unroll([A-B|T], Nodes) :- unroll(T, R), Nodes = [A|[B|R]].

:- unroll([a-b], [a,b]).
:- unroll([a-b, a-c], [a, b, a, c]).

unique([], []).
unique([H|T], U) :- member(H, U), unique(T, U).
unique([H|T], U) :- not(member(H, U)), unique(T, R), U = [H|R].

# unique(L, U) :- sort(L, U).

:- unique([1,2,3], [1,2,3]).
:- unique([1,1,2], [1,2]).
:- unique([2,2,1], [1,2]).
:- unique([1,1,1], [1]).

nodes(Graph, Nodes) :- unroll(Graph, R), unique(R, Nodes).

:- nodes([a-b], [a, b]).
:- nodes([a-b, b-c], [a, b, c]).
:- nodes([a-b, b-c, c-a], [a, b, c]).

node(Graph, Node) :- nodes(Graph, Nodes), member(Node, Nodes).

% colour_graph_simple(+Graph,-Colours).
colour_graph_simple(Graph, Colours) :-
  nodes(Graph, Nodes),
  colour_nodes(Nodes, Colours),
  check_colours(Graph,Colours).
  
% colour_nodes(+Nodes,-Colours)
colour_nodes([], []).
colour_nodes([H|T], Colours) :- colour(C), colour_nodes(T, R), Colours = [H=C|R].

:- colour_nodes([a], [a=red]).
:- colour_nodes([a], [a=blue]).
:- colour_nodes([a, b], [a=red, b=blue]).

% check_colours(+Graph,-Colours).
check_colours([],_).
check_colours([N1-N2|Es],Colours) :-
  member(N1=C1,Colours),
  member(N2=C2,Colours),
  C1 \= C2,
  check_colours(Es,Colours).

:- check_colours([a-b], [a=red, b=blue]).
:- not(check_colours([a-b], [a=red, b=red])).
:- check_colours([a-b, b-c, c-a], [a=red, b=green, c=blue]).
:- not(check_colours([a-b, b-c, c-a], [a=red, b=green, c=red])).

%% colour_graph(+Graph,+GivenColourSets,-ColourAssignments)
colour_graph(Graph,GivenColourSets,ColourAssignments) :-
  % determine the graph nodes
  nodes(Graph,Nodes),
  % initialise the nodes initial colour sets
  initial_colour_sets(Nodes,GivenColourSets,ColourSets),
  % label each of the graph nodes with a colour
  colour_nodes(Graph,Nodes,ColourSets,ColoursFinished),
  % reorganise the terms in the colour assignments slightly
  flatten_colour_lists(ColoursFinished,ColourAssignments).

flatten_colour_lists([],[]).
flatten_colour_lists([N=[C]|RestIn],[N=C|RestOut]) :-
  flatten_colour_lists(RestIn,RestOut).

initial_colour_sets([],_,[]).
initial_colour_sets([N|Ns],GivenColourSets,[N=Value|ColourSets]) :-
  get_with_default(N,GivenColourSets,[red,green,blue,yellow],Value),
  initial_colour_sets(Ns,GivenColourSets,ColourSets).

colour_nodes(_,[],ColourSets,ColourSets).
colour_nodes(Graph,[Node|Ns],ColourSetsIn,ColourSetsOut) :-
  choose_a_node_colour(Node,Colour,ColourSetsIn,ColourSets2),
  remove_colour_from_connected_sets(Graph,Node,Colour,ColourSets2,ColourSets3),
  colour_nodes(Graph,Ns,ColourSets3,ColourSetsOut).
  
%% choose_a_node_colour(+Node, -Colour, +ColourSetsIn, -ColourSetsOut)
choose_a_node_colour(Node, Colour, [N=Colours|T], ColourSetsOut) :-
  Node = N,
  member(Colour, Colours),
  ColourSetsOut = [N=[Colour]|T].
choose_a_node_colour(Node, Colour, [N=Colours|T], ColourSetsOut) :-
  Node \= N,
  ColourSetsOut = [N=Colours|R],
  choose_a_node_colour(Node, Colour, T, R).
  
remove_colour_from_connected_sets([],_,_,ColourSets,ColourSets).
remove_colour_from_connected_sets([N-ON|Es],N,Colour,CSs,NewCSs) :-
  remove_colour_from_specific_set(Colour,ON,CSs,CSs2),
  remove_colour_from_connected_sets(Es,N,Colour,CSs2,NewCSs).
remove_colour_from_connected_sets([ON-N|Es],N,Colour,CSs,NewCSs) :-
  remove_colour_from_specific_set(Colour,ON,CSs,CSs2),
  remove_colour_from_connected_sets(Es,N,Colour,CSs2,NewCSs).
remove_colour_from_connected_sets([ON1-ON2|Es],N,Colour,CSs,NewCSs) :-
  N \= ON1,
  N \= ON2,
  remove_colour_from_connected_sets(Es,N,Colour,CSs,NewCSs).

remove_colour_from_specific_set(Colour,TargetNode,[OtherNode=CS|ColourSets],[OtherNode=CS|NewColourSets]) :-
  TargetNode \= OtherNode,
  remove_colour_from_specific_set(Colour,TargetNode, ColourSets,NewColourSets).
remove_colour_from_specific_set(Colour,TargetNode,[OtherNode=CS|ColourSets], NewColourSets) :-
  TargetNode = OtherNode,
  remove(Colour, CS, CSS),
  NewColourSets = [OtherNode=CSS|ColourSets].

% test that an arbitary valid (and invalid) colouring is (and is not) proved.
test(t1):-t_exists(example1,t_s1,[a=red,b=green,c=blue,d=green,e=red]).
test(t2):-not(t_exists(example1,t_s1,[a=red,b=green,c=blue,d=green,e=blue])).
test(t3):-t_exists(example1,t_c1,[a=red,b=green,c=blue,d=green,e=red]).
test(t4):-not(t_exists(example1,t_c1,[a=red,b=green,c=blue,d=green,e=blue])).

% test that right numbers of solutions are discovered
test(s1n):-graph(example1,G),numsol( colour_graph_simple(G,_), 96).
test(c1n):-graph(example1,G),numsol( colour_graph(G,[],_), 96).
test(c2n):-graph(example2,G),numsol( colour_graph(G,[],_), 216).

% test one complete solution for each given example
test(c2s):-graph(example2,G),findall(CSorted,(colour_graph(G,[water=[blue],'NSW'=[green],'VIC'=[red],'TAS'=[green]],C), msort_colours(C,CSorted)),L),perm(L,[['ACT'=red,'NSW'=green,'NT'=green,'QLD'=red,'SA'=yellow,'TAS'=green,'VIC'=red,'WA'=red,water=blue],['ACT'=blue,'NSW'=green,'NT'=green, 'QLD'=red,'SA'=yellow,'TAS'=green,'VIC'=red,'WA'=red,water=blue], ['ACT'=yellow,'NSW'=green,'NT'=green,'QLD'=red,'SA'=yellow, 'TAS'=green,'VIC'=red,'WA'=red,water=blue]]).
test(c1s):-graph(example1,G),findall(CSorted,(colour_graph(G, [a=[red],b=[blue]],C),msort_colours(C,CSorted)),L), perm(L, [[a=red,b=blue,c=green,d=blue,e=red],[a=red,b=blue,c=green,d=blue, e=yellow],[a=red,b=blue,c=green,d=yellow,e=red],[a=red,b=blue, c=green,d=yellow,e=blue],[a=red,b=blue,c=yellow,d=green,e=red], [a=red,b=blue,c=yellow,d=green,e=blue],[a=red,b=blue,c=yellow, d=blue,e=red],[a=red,b=blue,c=yellow,d=blue,e=green]]).
t_exists(GraphName,Predicate,ValidColouring):- graph(GraphName,Graph), call(Predicate,Graph,ColouringOut), msort_colours(ColouringOut,ValidColouring).
t_s1(Graph,ColouringOut):-colour_graph_simple(Graph,ColouringOut). t_c1(Graph,ColouringOut):-colour_graph(Graph,[],ColouringOut). t_c2(Graph,ColouringOut):-colour_graph(Graph,[water=[blue]],ColouringOut).

% Helper predicates from lectures
numsol(Predicate,NumberOfSolutions) :-
  findall(dummy,Predicate,AnsList),
  length(AnsList,NumberOfSolutions).

perm([],[]).
perm(List,[H|T]) :- take(List,H,R), perm(R,T).
take([H|T],H,T).
take([H|T],R,[H|S]) :- take(T,R,S)
.
run_tests_quiet :- foreach( clause(test(TestID),_), test(TestID) ),!.
run_tests_quiet :- run_tests,fail.
run_tests :- foreach( clause(test(TestID),_), run_test(TestID) ).
run_test(TestID) :-
  ( test(TestID) -> Result = succeeded; Result = failed ),
  format("Test ~w: ~w.~n",[TestID,Result]).

:-run_tests_quiet.