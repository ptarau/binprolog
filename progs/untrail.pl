go:-
    T=f(X,X),get_neck_cut(CUT),
    for(_,1,3),nl,
    write(cut=CUT),nl,
    T=f(Y,a),
    get_deep_cut(DEEP),
    write(deep_cut=DEEP),nl,
    CUT==DEEP,
    write(=),nl,
    write(before=Y+T),nl,
    untrail_to(DEEP),!,
    write(after=Y+T),nl,
    fail.

