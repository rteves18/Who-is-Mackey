/*ky_is_cool.
likes(alice,bob).
likes(bob,carol).
likes(james,mary).
likes(mary,james).
love_compatible(X, Y) :- likes(X, Y), likes(Y, X).
mother(alice,lea). %alice's mother is lea
mother(john,julia).
mother(lea,alberta).
father(james,alfred).
father(lea,john).
parent(X,Y) :- father(X,Y). %If this fails, do the one below
parent(X,Y) :- mother(X,Y). 
grandparent(X,Y) :- parent(X,Z), parent(Z,Y).
color(red).
color(green).
color(blue).
color(yellow).
neighbor(first,second) :- 
	color(first), color(second), first \= second.
germany(SH, MV, HH, HB, NI, ST, BE, BB, SN, NW, HE, TH, RP, SL, BW, BY) :- 
	neighbor(SH, NI), neighbor(SH, HH), neighbor(SH, MV),
	neighbor(HH, NI),
	neighbor(MV, NI), neighbor(MV, BB),
	neighbor(NI, HB), neighbor(NI, BB), neighbor(NI, ST), neighbor(NI, TH),
	neighbor(NI, HE), neighbor(NI, NW),
	neighbor(ST, BB), neighbor(ST, SN), neighbor(ST, TH),
	neighbor(BB, BE), neighbor(BB, SN),
	neighbor(NW, HE), neighbor(NW, RP),
	neighbor(SN, TH), neighbor(SN, BY),
	neighbor(RP, SL), neighbor(RP, HE), neighbor(RP, BW),
	neighbor(HE, BW), neighbor(HE, TH), neighbor(HE, BY),
	neighbor(TH, BY),
	neighbor(BW, BY).
*/
bigger(elephant,horse).
bigger(horse,donkey).
bigger(donkey,monkey).
bigger(donkey,dog).

is_bigger(X,Y) :- bigger(X,Y).
is_bigger(X,Y) :- bigger(X,Z), is_bigger(Z,Y).
%is_bigger(X,Y) :- bigger(X,Z), bigger(Z,W), bigger(W,Y).


parent(john,paul).   /* paul is john's parent    */

parent(paul,tom).    /* tom is paul's parent     */

parent(tom,rock).
parent(rock,mary).    /* mary is tom's parent     */

ancestor(X,Y) :- parent(X,Y).
/* If Y is a parent of X, then Y is an ancestor of X */

ancestor(X,Y) :- parent(X,Z),

              ancestor(Z,Y).
 /* if Y is an ancestor of Z and Z is a parent of X,
     then Y is an ancestor of X */

concat_lists( [Elem | List1], List2, [Elem | List3]) :-
    concat_lists(List1, List2, List3). 

analyse_list( [Car | Cdr] ) :-
    write( Car ), nl, write( Cdr ).

/*remove_duplicates( List, Set ) :-
    rdHelper( List, [], Set).

rdHelper([], Acc, Acc).

rdHelper([H|T], Acc, Set) :-
    member(H, Acc), rdHelper(T, Acc, Set).

rdHelper([H|T], Acc, Set) :-
    rdHelper(T, [H|Acc], Set).*/

/* Reverse list with tail recursion */
reverse_list(List, Set) :-
    rev_helper(List, [], Set).

rev_helper([H|T], Acc, Set) :-
    rev_helper(T, [H|Acc], Set).
    rev_helper([], Acc, Acc).

/* Permutation */
permu([],[]).

permu(List, [Element|Permu]) :-
    select(List, Element, Rest),
    permu(Rest, Permu).

remove_duplicates([],[]).
remove_duplicates([H|T],Result) :-
    member(H,T), !,
    remove_duplicates(T,Result).
remove_duplicates([H|T],[H|Result]) :-
    remove_duplicates(T,Result).


/* Cut example program */
beautiful(claudia).
beautiful(sharon).
beautiful(denise).

intelligent(margaret).
intelligent(sharon).

bride(Girl) :-
    beautiful(Girl),
    intelligent(Girl).

















