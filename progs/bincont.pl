% converts a definite clause to a binary metaclause
%    where each metavariable Cont represents a "continuation"
%    and a goal G is represented by a clause :- G.

def_to_binary((H:-B),M):-!,def_to_binary0(H,B,M).
def_to_binary(H,M):-def_to_binary0(H,true,M).

def_to_binary0('@@'(H,Upper),B,(HC:-BC)) :- nonvar(H),!,
        % term_append(H,cont(ContH),HC),
        H=..FXs,
        append(FXs,[ContH],FXsC),
        HC=..FXsC,
        add_upper_continuation(B,Upper,ContH,BC).
def_to_binary0(H,B,(HC:-BC)) :-
        term_append(H,cont(Cont),HC),
        add_continuation(B,Cont,BC).            

add_upper_continuation(B,Upper,ContH,BC):-nonvar(Upper),!,
        add_continuation(Upper,ContU,ContH),
        add_continuation(B,ContU,BC).
add_upper_continuation(B,Upper,ContH,BC):-
        add_continuation((strip_continuation(ContH,Upper,ContU),B),ContU,BC).

% adds a continuation to a term

add_continuation((true,Gs),C,GC):-!,add_continuation(Gs,C,GC).
add_continuation((fail,_),C,fail(C)):-!.
add_continuation((G,Gs1),C,GC):-!,
                 add_continuation(Gs1,C,Gs2),
                 term_append(G,cont(Gs2),GC).
add_continuation(G,C,GC):-term_append(G,cont(C),GC).

 %term_append(H,cont(Cont),HC),
