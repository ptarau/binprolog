% generated: 19 November 1989
% option(s): 
%
%  chat_parser
%
%  Fernando C. N. Pereira and David H. D. Warren

% changed open -> o_p_e_n to avoid name clash with Quintus builtin P.Tarau
% reprinted to avoid var occurs once warnings P.Tarau

% go/0:
go :- 
        go('BMARK_chat:').

% go/1:
go(A) :- 
        test(B),
        nl,
        write(A = [B]),
        nl.

% test/1:
test(time-A) :- 
        statistics(runtime,_),
        chat_parser,
        statistics(runtime,[_,A]).

% chat_parser/0:
chat_parser :- 
        string(A),
        determinate_say(A,_),
        fail.
chat_parser.

% string/1:
string([what,rivers,are,there,?]).
string([does,afghanistan,border,china,?]).
string([what,is,the,capital,of,upper_volta,?]).
string([where,is,the,largest,country,?]).
string([which,country,quote,s,capital,is,london,?]).
string([which,countries,are,european,?]).
string([how,large,is,the,smallest,american,country,?]).
string([what,is,the,ocean,that,borders,african,countries,and,that,borders,asian,countries,?]).
string([what,are,the,capitals,of,the,countries,bordering,the,baltic,?]).
string([which,countries,are,bordered,by,two,seas,?]).
string([how,many,countries,does,the,danube,flow,through,?]).
string([what,is,the,total,area,of,countries,south,of,the,equator,and,not,in,australasia,?]).
string([what,is,the,average,area,of,the,countries,in,each,continent,?]).
string([is,there,more,than,one,country,in,each,continent,?]).
string([is,there,some,ocean,that,does,not,border,any,country,?]).
string([what,are,the,countries,from,which,a,river,flows,into,the,black_sea,?]).

% determinate_say/2:
determinate_say(A,B) :- 
        say(A,B),  !.

% terminal/5:
terminal(B,A,A,x(_,terminal,B,C),C).
terminal(A,[A|B],B,C,C) :- 
        gap(C).

% gap/1:
gap(x(gap,_,_,_)).
gap([]).

% virtual/3:
virtual(A,x(_,nonterminal,A,B),B).

% is_pp/1:
is_pp(#(1,_,_,_)).

% is_pred/1:
is_pred(#(_,1,_,_)).

% is_chat_trace/1:
is_chat_trace(#(_,_,1,_)).

% is_adv/1:
is_adv(#(_,_,_,1)).

% chat_trace/2:
chat_trace(#(_,_,1,_),#(0,0,0,0)).

% chat_trace/1:
chat_trace(#(0,0,1,0)).

% adv/1:
adv(#(0,0,0,1)).

% empty/1:
empty(#(0,0,0,0)).

% np_all/1:
np_all(#(1,1,1,0)).

% s_all/1:
s_all(#(1,0,1,1)).

% np_no_chat_trace/1:
np_no_chat_trace(#(1,1,0,0)).

% myplus/3:
myplus(#(A,D,G,J),#(B,E,H,K),#(C,F,I,L)) :- 
        or(A,B,C),
        or(D,E,F),
        or(G,H,I),
        or(J,K,L).

% minus/3:
minus(#(A,D,G,J),#(B,E,H,K),#(C,F,I,L)) :- 
        anot(A,B,C),
        anot(D,E,F),
        anot(G,H,I),
        anot(J,K,L).

% or/3:
or(1,_,1).
or(0,1,1).
or(0,0,0).

% anot/3:
anot(A,0,A).
anot(_,1,0).

% role/3:
role(subj,_,#(1,0,0)).
role(compl,_,#(0,_,_)).
role(undef,main,#(_,0,_)).
role(undef,aux,#(0,_,_)).
role(undef,decl,_).
role(nil,_,_).

% subj_case/1:
subj_case(#(1,0,0)).

% verb_case/1:
verb_case(#(0,1,0)).

% prep_case/1:
prep_case(#(0,0,1)).

% compl_case/1:
compl_case(#(0,_,_)).

% say/2:
say(B,A) :- 
        sentence(A,B,[],[],[]).

% sentence/5:
sentence(A,B,E,C,G) :- 
        declarative(A,B,D,C,F),
        terminator('.',D,E,F,G).
sentence(A,B,E,C,G) :- 
        wh_question(A,B,D,C,F),
        terminator(?,D,E,F,G).
sentence(C,A,G,B,I) :- 
        topic(A,D,B,E),
        wh_question(C,D,F,E,H),
        terminator(?,F,G,H,I).
sentence(A,B,E,C,G) :- 
        yn_question(A,B,D,C,F),
        terminator(?,D,E,F,G).
sentence(A,B,E,C,G) :- 
        imperative(A,B,D,C,F),
        terminator(!,D,E,F,G).

% pp/8:
pp(B,C,D,E,A,A,F,G) :- 
        virtual(pp(B,C,D,E),F,G).
pp(pp(A,D),F,G,H,B,J,C,L) :- 
        prep(A,B,I,C,K),
        prep_case(E),
        np(D,_,E,_,F,G,H,I,J,K,L).

% topic/4:
topic(D,G,E,x(gap,nonterminal,pp(A,compl,B,C),I)) :- 
        pp(A,compl,B,C,D,F,E,H),
        opt_comma(F,G,H,I).

% opt_comma/4:
opt_comma(A,B,C,D) :- 
        quote((','),A,B,C,D).
opt_comma(A,A,B,B).

% declarative/5:
declarative(decl(A),B,C,D,E) :- 
        s(A,_,B,C,D,E).

% wh_question/5:
wh_question(whq(A,F),B,H,C,J) :- 
        variable_q(A,_,D,E,B,G,C,I),
        question(D,E,F,G,H,I,J).

% np/11:
np(B,C,D,E,F,G,H,A,A,I,J) :- 
        virtual(np(B,C,D,E,F,G,H),I,J).
np(np(A,C,[]),A,J,def,_,B,H,D,E,F,G) :- 
        is_pp(B),
        pers_pron(C,A,I,D,E,F,G),
        empty(H),
        role(I,decl,J).
np(np(A,C,E),A,_,D,I,B,L,F,N,G,P) :- 
        is_pp(B),
        np_head(C,A,D+H,J,E,F,M,G,O),
        np_all(K),
        np_compls(H,A,I,J,K,L,M,N,O,P).
np(part(B,H),3+C,_,indef,J,A,L,D,N,E,P) :- 
        is_pp(A),
        determiner(B,C,indef,D,F,E,G),
        quote(of,F,M,G,O),
        s_all(K),
        prep_case(I),
        np(H,3+plu,I,def,J,K,L,M,N,O,P).

% variable_q/8:
variable_q(C,A,E,B,F,G,H,x(gap,nonterminal,np(D,A,B,_,_,J,K),I)) :- 
        whq(C,A,D,E,F,G,H,I),
        chat_trace(J,K).
variable_q(D,E,compl,M,B,H,C,x(gap,nonterminal,pp(pp(A,F),compl,K,L),J)) :- 
        prep(A,B,G,C,I),
        whq(D,E,F,_,G,H,I,J),
        chat_trace(K,L),
        compl_case(M).
variable_q(B,A,compl,K,E,F,G,x(gap,nonterminal,adv_phrase(pp(C,np(A,np_head(int_det(B),[],D),[])),I,J),H)) :- 
        context_pron(C,D,E,F,G,H),
        chat_trace(I,J),
        verb_case(K).
variable_q(A,_,compl,J,B,F,C,x(gap,nonterminal,predicate(adj,value(D,wh(A)),I),H)) :- 
        quote(how,B,E,C,G),
        adj(quant,D,E,F,G,H),
        empty(I),
        verb_case(J).

% adv_phrase/7:
adv_phrase(B,C,D,A,A,E,F) :- 
        virtual(adv_phrase(B,C,D),E,F).
adv_phrase(pp(A,D),E,F,B,H,C,J) :- 
        loc_pred(A,B,G,C,I),
        pp(pp(prep(of),D),compl,E,F,G,H,I,J).

% predicate/7:
predicate(B,C,D,A,A,E,F) :- 
        virtual(predicate(B,C,D),E,F).
predicate(_,A,B,C,D,E,F) :- 
        adj_phrase(A,B,C,D,E,F).
predicate(neg,A,C,D,E,F,G) :- 
        s_all(B),
        pp(A,compl,B,C,D,E,F,G).
predicate(_,A,C,D,E,F,G) :- 
        s_all(B),
        adv_phrase(A,B,C,D,E,F,G).

% whq/8:
whq(A,B,E,undef,C,H,D,J) :- 
        int_det(A,B,C,G,D,I),
        s_all(F),
        np(E,B,_,_,subj,F,_,G,H,I,J).
whq(B,3+A,np(3+A,wh(B),[]),C,D,E,F,G) :- 
        int_pron(C,D,E,F,G).

% int_det/6:
int_det(A,3+B,C,D,E,F) :- 
        whose(A,B,C,D,E,F).
int_det(A,3+B,C,D,E,F) :- 
        int_art(A,B,C,D,E,F).

% gen_marker/4:
gen_marker(A,A,B,C) :- 
        virtual(gen_marker,B,C).
gen_marker(A,D,B,F) :- 
        quote(quote,A,C,B,E),
        an_s(C,D,E,F).

% whose/6:
whose(A,B,C,D,E,x(nogap,nonterminal,np_head0(wh(A),B,proper),x(nogap,nonterminal,gen_marker,F))) :- 
        quote(whose,C,D,E,F).

% question/7:
question(A,B,C,D,E,F,G) :- 
        subj_question(A),
        role(subj,_,B),
        s(C,_,D,E,F,G).
question(A,B,E,C,G,D,I) :- 
        fronted_verb(A,B,C,F,D,H),
        s(E,_,F,G,H,I).

% det/7:
det(B,C,D,A,A,E,F) :- 
        virtual(det(B,C,D),E,F).
det(det(G),F,H,A,B,C,D) :- 
        terminal(E,A,B,C,D),
        det(E,F,G,H).
det(generic,_,generic,A,A,B,B).

% int_art/6:
int_art(B,A,D,E,F,x(nogap,nonterminal,det(C,A,def),G)) :- 
        int_art(B,A,C,D,E,F,G).

% subj_question/1:
subj_question(subj).
subj_question(undef).

% yn_question/5:
yn_question(q(C),A,E,B,G) :- 
        fronted_verb(nil,_,A,D,B,F),
        s(C,_,D,E,F,G).

% verb_form/8:
verb_form(B,C,D,E,A,A,F,G) :- 
        virtual(verb_form(B,C,D,E),F,G).
verb_form(F,G,H,_,A,B,C,D) :- 
        terminal(E,A,B,C,D),
        verb_form(E,F,G,H).

% neg/6:
neg(B,C,A,A,D,E) :- 
        virtual(neg(B,C),D,E).
neg(aux+_,neg,A,B,C,D) :- 
        quote(not,A,B,C,D).
neg(_,pos,A,A,B,B).

% fronted_verb/6:
fronted_verb(F,H,D,K,E,x(gap,nonterminal,verb_form(A,B,C,G),x(nogap,nonterminal,neg(_,I),M))) :- 
        verb_form(A,B,C,_,D,J,E,L),
        verb_type(A,aux+_),
        role(F,G,H),
        neg(_,I,J,K,L,M).

% imperative/5:
imperative(imp(C),A,E,B,G) :- 
        imperative_verb(A,D,B,F),
        s(C,_,D,E,F,G).

% imperative_verb/4:
imperative_verb(B,C,D,x(nogap,terminal,you,x(nogap,nonterminal,verb_form(A,imp+fin,2+sin,main),E))) :- 
        verb_form(A,inf,_,_,B,C,D,E).

% s/6:
s(s(A,D,J,P),S,B,U,C,W) :- 
        subj(A,E,F,B,G,C,H),
        verb(D,E,F,I,G,L,H,M),
        empty(K),
        s_all(N),
        verb_args(F,I,J,K,O,L,T,M,V),
        minus(N,O,Q),
        myplus(N,O,R),
        verb_mods(P,Q,R,S,T,U,V,W).

% subj/7:
subj(there,_,_+be,A,B,C,D) :- 
        quote(there,A,B,C,D).
subj(A,B,_,E,F,G,H) :- 
        s_all(D),
        subj_case(C),
        np(A,B,C,_,subj,D,_,E,F,G,H).

% np_head/9:
np_head(G,H,I,J,K,A,M,B,O) :- 
        np_head0(C,D,E,A,L,B,N),
        possessive(C,D,E,F,F,G,H,I,J,K,L,M,N,O).

% np_head0/7:
np_head0(B,C,D,A,A,E,F) :- 
        virtual(np_head0(B,C,D),E,F).
np_head0(name(A),3+sin,def+proper,B,C,D,E) :- 
        name(A,B,C,D,E).
np_head0(np_head(A,F,I),3+B,C+common,D,K,E,M) :- 
        determiner(A,B,C,D,G,E,H),
        adjs(F,G,J,H,L),
        noun(I,B,J,K,L,M).
np_head0(A,B,def+proper,C,D,E,x(nogap,nonterminal,gen_marker,F)) :- 
        poss_pron(A,B,C,D,E,F).
np_head0(np_head(A,[],B),3+sin,indef+common,C,D,E,F) :- 
        quantifier_pron(A,B,C,D,E,F).

% np_compls/10:
np_compls(proper,_,_,[],_,C,A,A,B,B) :- 
        empty(C).
np_compls(common,A,B,C,D,K,F,M,G,O) :- 
        np_all(E),
        np_mods(A,B,H,C,D,I,E,J,F,L,G,N),
        relative(A,H,I,J,K,L,M,N,O).

% possessive/14:
possessive(I,H,_,[],J,L,M,N,O,P,A,R,B,T) :- 
        gen_case(A,C,B,D),
        np_head0(E,F,G,C,Q,D,S),
        possessive(E,F,G,K,[pp(poss,np(H,I,J))|K],L,M,N,O,P,Q,R,S,T).
possessive(A,B,C,D,E,A,B,C,D,E,F,F,G,G).

% gen_case/4:
gen_case(A,B,C,x(nogap,terminal,the,D)) :- 
        gen_marker(A,B,C,D).

% an_s/4:
an_s(A,B,C,D) :- 
        quote(s,A,B,C,D).
an_s(A,A,B,B).

% determiner/7:
determiner(A,B,C,D,E,F,G) :- 
        det(A,B,C,D,E,F,G).
determiner(A,B,C,D,E,F,G) :- 
        quant_phrase(A,B,C,D,E,F,G).

% quant_phrase/7:
quant_phrase(quant(A,E),F,B,C,H,D,J) :- 
        quant(A,B,C,G,D,I),
        number(E,F,G,H,I,J).

% quant/6:
quant(A,indef,B,H,C,J) :- 
        neg_adv(D,A,B,E,C,F),
        comp_adv(D,E,G,F,I),
        quote(than,G,H,I,J).
quant(H,indef,A,D,B,F) :- 
        quote(at,A,C,B,E),
        sup_adv(G,C,D,E,F),
        sup_op(G,H).
quant(the,def,A,B,C,D) :- 
        quote(the,A,B,C,D).
quant(same,indef,A,A,B,B).

% neg_adv/6:
neg_adv(A,not+A,B,C,D,E) :- 
        quote(not,B,C,D,E).
neg_adv(A,A,B,B,C,C).

% sup_op/2:
sup_op(least,not+less).
sup_op(most,not+more).

% np_mods/12:
np_mods(A,B,J,[C|K],D,M,_,O,E,Q,F,S) :- 
        np_mod(A,B,C,D,H,E,P,F,R),
        chat_trace(G),
        myplus(G,H,I),
        minus(D,I,L),
        myplus(H,D,N),
        np_mods(A,B,J,K,L,M,N,O,P,Q,R,S).
np_mods(_,_,A,A,B,B,C,C,D,D,E,E).

% np_mod/9:
np_mod(_,B,A,C,D,E,F,G,H) :- 
        pp(A,B,C,D,E,F,G,H).
np_mod(A,_,B,C,D,E,F,G,H) :- 
        reduced_relative(A,B,C,D,E,F,G,H).

% verb_mods/8:
verb_mods([A|H],B,_,K,C,M,D,O) :- 
        verb_mod(A,B,F,C,L,D,N),
        chat_trace(E),
        myplus(E,F,G),
        minus(B,G,I),
        myplus(F,B,J),
        verb_mods(H,I,J,K,L,M,N,O).
verb_mods([],_,A,A,B,B,C,C).

% verb_mod/7:
verb_mod(A,B,C,D,E,F,G) :- 
        adv_phrase(A,B,C,D,E,F,G).
verb_mod(B,A,G,C,D,E,F) :- 
        is_adv(A),
        adverb(B,C,D,E,F),
        empty(G).
verb_mod(A,B,C,D,E,F,G) :- 
        pp(A,compl,B,C,D,E,F,G).

% adjs/5:
adjs([A|D],B,F,C,H) :- 
        pre_adj(A,B,E,C,G),
        adjs(D,E,F,G,H).
adjs([],A,A,B,B).

% pre_adj/5:
pre_adj(A,B,C,D,E) :- 
        adj(_,A,B,C,D,E).
pre_adj(A,B,C,D,E) :- 
        sup_phrase(A,B,C,D,E).

% sup_phrase/5:
sup_phrase(sup(most,A),B,C,D,E) :- 
        sup_adj(A,B,C,D,E).
sup_phrase(sup(A,D),B,F,C,H) :- 
        sup_adv(A,B,E,C,G),
        adj(quant,D,E,F,G,H).

% comp_phrase/6:
comp_phrase(comp(A,B,E),H,C,J,D,L) :- 
        comp(A,B,C,I,D,K),
        np_no_chat_trace(G),
        prep_case(F),
        np(E,_,F,_,compl,G,H,I,J,K,L).

% comp/6:
comp(A,D,B,H,C,J) :- 
        comp_adv(A,B,E,C,F),
        adj(quant,D,E,G,F,I),
        quote(than,G,H,I,J).
comp(more,A,B,E,C,G) :- 
        rel_adj(A,B,D,C,F),
        quote(than,D,E,F,G).
comp(same,C,A,G,B,I) :- 
        quote(as,A,D,B,E),
        adj(quant,C,D,F,E,H),
        quote(as,F,G,H,I).

% relative/9:
relative(B,[C],A,_,D,E,F,G,H) :- 
        is_pred(A),
        rel_conj(B,_,C,D,E,F,G,H).
relative(_,[],_,A,A,B,B,C,C).

% rel_conj/8:
rel_conj(A,D,F,H,B,J,C,L) :- 
        rel(A,E,G,B,I,C,K),
        rel_rest(A,D,E,F,G,H,I,J,K,L).

% rel_rest/10:
rel_rest(F,A,B,C,_,I,D,K,E,M) :- 
        conj(A,G,B,H,C,D,J,E,L),
        rel_conj(F,G,H,I,J,K,L,M).
rel_rest(_,_,A,A,B,B,C,C,D,D).

% rel/7:
rel(C,rel(D,G),L,A,N,B,P) :- 
        o_p_e_n(A,E,B,F),
        variable(C,D,E,H,F,I),
        s(G,J,H,M,I,O),
        chat_trace(K),
        minus(J,K,L),
        close(M,N,O,P).

% variable/6:
variable(A,B,C,D,E,x(gap,nonterminal,np(np(A,wh(B),[]),A,_,_,_,G,H),F)) :- 
        quote(that,C,D,E,F),
        chat_trace(G,H).
variable(B,A,F,G,H,x(gap,nonterminal,np(C,D,E,_,_,J,K),I)) :- 
        wh(A,B,C,D,E,F,G,H,I),
        chat_trace(J,K).
variable(E,D,B,H,C,x(gap,nonterminal,pp(pp(A,F),compl,K,L),J)) :- 
        prep(A,B,G,C,I),
        wh(D,E,F,_,M,G,H,I,J),
        chat_trace(K,L),
        compl_case(M).

% wh/9:
wh(B,A,np(A,wh(B),[]),A,H,C,D,E,F) :- 
        rel_pron(G,C,D,E,F),
        role(G,decl,H).
wh(H,I,np(A,B,[pp(E,J)]),A,_,C,L,D,N) :- 
        np_head0(B,A,_+common,C,F,D,G),
        prep(E,F,K,G,M),
        wh(H,I,J,_,_,K,L,M,N).
wh(A,B,E,F,G,C,J,D,L) :- 
        whose(A,B,C,I,D,K),
        s_all(H),
        np(E,F,G,def,subj,H,_,I,J,K,L).

% reduced_relative/8:
reduced_relative(B,C,A,D,E,F,G,H) :- 
        is_pred(A),
        reduced_rel_conj(B,_,C,D,E,F,G,H).

% reduced_rel_conj/8:
reduced_rel_conj(A,D,F,H,B,J,C,L) :- 
        reduced_rel(A,E,G,B,I,C,K),
        reduced_rel_rest(A,D,E,F,G,H,I,J,K,L).

% reduced_rel_rest/10:
reduced_rel_rest(F,A,B,C,_,I,D,K,E,M) :- 
        conj(A,G,B,H,C,D,J,E,L),
        reduced_rel_conj(F,G,H,I,J,K,L,M).
reduced_rel_rest(_,_,A,A,B,B,C,C,D,D).

% reduced_rel/7:
reduced_rel(C,reduced_rel(D,G),L,A,N,B,P) :- 
        o_p_e_n(A,E,B,F),
        reduced_wh(C,D,E,H,F,I),
        s(G,J,H,M,I,O),
        chat_trace(K),
        minus(J,K,L),
        close(M,N,O,P).

% reduced_wh/6:
reduced_wh(A,B,D,I,E,x(nogap,nonterminal,np(np(A,wh(B),[]),A,N,_,_,L,M),x(nogap,nonterminal,verb_form(be,pres+fin,A,main),x(nogap,nonterminal,neg(_,C),x(nogap,nonterminal,predicate(C,F,G),K))))) :- 
        neg(_,C,D,H,E,J),
        predicate(C,F,G,H,I,J,K),
        chat_trace(L,M),
        subj_case(N).
reduced_wh(A,B,F,G,H,x(nogap,nonterminal,np(np(A,wh(B),[]),A,L,_,_,J,K),x(nogap,nonterminal,verb(C,_,D,E),I))) :- 
        participle(C,D,E,F,G,H,I),
        chat_trace(J,K),
        subj_case(L).
reduced_wh(A,B,I,J,K,x(nogap,nonterminal,np(E,F,C,G,_,M,N),x(gap,nonterminal,np(np(A,wh(B),[]),A,D,_,_,O,P),L))) :- 
        s_all(H),
        subj_case(C),
        verb_case(D),
        np(E,F,_,G,subj,H,_,I,J,K,L),
        chat_trace(M,N),
        chat_trace(O,P).

% verb/8:
verb(B,C,D,E,A,A,F,G) :- 
        virtual(verb(B,C,D,E),F,G).
verb(verb(L,A,B+fin,M,H),C,R,A,D,O,E,Q) :- 
        verb_form(F,B+fin,C,K,D,I,E,J),
        verb_type(F,G),
        neg(G,H,I,N,J,P),
        rest_verb(K,F,L,A,M,N,O,P,Q),
        verb_type(L,R).

% rest_verb/9:
rest_verb(aux,have,D,E,[perf|F],A,H,B,J) :- 
        verb_form(C,past+part,_,_,A,G,B,I),
        have(C,D,E,F,G,H,I,J).
rest_verb(aux,be,E,F,G,A,I,B,K) :- 
        verb_form(D,C,_,_,A,H,B,J),
        be(C,D,E,F,G,H,I,J,K).
rest_verb(aux,do,A,active,[],B,C,D,E) :- 
        verb_form(A,inf,_,_,B,C,D,E).
rest_verb(main,A,A,active,[],B,B,C,C).

% have/8:
have(be,E,F,G,A,I,B,K) :- 
        verb_form(D,C,_,_,A,H,B,J),
        be(C,D,E,F,G,H,I,J,K).
have(A,A,active,[],B,B,C,C).

% be/9:
be(past+part,A,A,passive,[],B,B,C,C).
be(pres+part,A,B,C,[prog],D,E,F,G) :- 
        passive(A,B,C,D,E,F,G).

% passive/7:
passive(be,A,passive,B,C,D,E) :- 
        verb_form(A,past+part,_,_,B,C,D,E),
        verb_type(A,F),
        passive(F).
passive(A,A,active,B,B,C,C).

% participle/7:
participle(verb(E,A,inf,K,B),L,A,C,G,D,I) :- 
        neg(_,B,C,F,D,H),
        verb_form(E,J,_,_,F,G,H,I),
        participle(J,A,K),
        verb_type(E,L).

% passive/1:
passive(_+trans).
passive(_+ditrans).

% participle/3:
participle(pres+part,active,[prog]).
participle(past+part,passive,[]).

% close/4:
close(A,A,B,C) :- 
        virtual(close,B,C).

% o_p_e_n/4:
o_p_e_n(A,A,B,x(gap,nonterminal,close,B)).

% verb_args/9:
verb_args(_+D,E,A,G,H,B,J,C,L) :- 
        advs(A,F,_,B,I,C,K),
        verb_args(D,E,F,G,H,I,J,K,L).
verb_args(trans,active,[arg(dir,A)],_,B,C,D,E,F) :- 
        verb_arg(np,A,B,C,D,E,F).
verb_args(ditrans,_,[arg(D,A)|E],_,G,B,I,C,K) :- 
        verb_arg(np,A,F,B,H,C,J),
        object(D,E,F,G,H,I,J,K).
verb_args(be,_,[void],A,A,B,C,D,E) :- 
        terminal(there,B,C,D,E).
verb_args(be,_,[arg(predicate,A)],_,B,C,D,E,F) :- 
        pred_conj(_,A,B,C,D,E,F).
verb_args(be,_,[arg(dir,A)],_,B,C,D,E,F) :- 
        verb_arg(np,A,B,C,D,E,F).
verb_args(have,active,[arg(dir,A)],_,B,C,D,E,F) :- 
        verb_arg(np,A,B,C,D,E,F).
verb_args(D,_,[],A,A,B,B,C,C) :- 
        no_args(D).

% object/8:
object(G,C,B,I,E,K,F,M) :- 
        adv(A),
        minus(A,B,D),
        advs(C,H,D,E,J,F,L),
        obj(G,H,B,I,J,K,L,M).

% obj/8:
obj(ind,[arg(dir,A)],_,B,C,D,E,F) :- 
        verb_arg(np,A,B,C,D,E,F).
obj(dir,[],A,A,B,B,C,C).

% pred_conj/7:
pred_conj(C,E,G,A,I,B,K) :- 
        predicate(_,D,F,A,H,B,J),
        pred_rest(C,D,E,F,G,H,I,J,K).

% pred_rest/9:
pred_rest(A,B,C,_,H,D,J,E,L) :- 
        conj(A,F,B,G,C,D,I,E,K),
        pred_conj(F,G,H,I,J,K,L).
pred_rest(_,A,A,B,B,C,C,D,D).

% verb_arg/7:
verb_arg(np,A,D,E,F,G,H) :- 
        s_all(C),
        verb_case(B),
        np(A,_,B,_,compl,C,D,E,F,G,H).

% advs/7:
advs([B|E],F,A,C,H,D,J) :- 
        is_adv(A),
        adverb(B,C,G,D,I),
        advs(E,F,A,G,H,I,J).
advs(A,A,_,B,B,C,C).

% adj_phrase/6:
adj_phrase(A,F,B,C,D,E) :- 
        adj(_,A,B,C,D,E),
        empty(F).
adj_phrase(A,B,C,D,E,F) :- 
        comp_phrase(A,B,C,D,E,F).

% no_args/1:
no_args(trans).
no_args(ditrans).
no_args(intrans).

% conj/9:
conj(conj(A,D),conj(A,E),B,C,conj(A,B,C),F,G,H,I) :- 
        conj(A,D,E,F,G,H,I).

% noun/6:
noun(F,G,A,B,C,D) :- 
        terminal(E,A,B,C,D),
        noun_form(E,F,G).

% adj/6:
adj(F,adj(A),B,C,D,E) :- 
        terminal(A,B,C,D,E),
        adj(A,F).

% prep/5:
prep(prep(A),B,C,D,E) :- 
        terminal(A,B,C,D,E),
        prep(A).

% rel_adj/5:
rel_adj(adj(F),A,B,C,D) :- 
        terminal(E,A,B,C,D),
        rel_adj(E,F).

% sup_adj/5:
sup_adj(adj(F),A,B,C,D) :- 
        terminal(E,A,B,C,D),
        sup_adj(E,F).

% comp_adv/5:
comp_adv(less,A,B,C,D) :- 
        quote(less,A,B,C,D).
comp_adv(more,A,B,C,D) :- 
        quote(more,A,B,C,D).

% sup_adv/5:
sup_adv(least,A,B,C,D) :- 
        quote(least,A,B,C,D).
sup_adv(most,A,B,C,D) :- 
        quote(most,A,B,C,D).

% rel_pron/5:
rel_pron(F,A,B,C,D) :- 
        terminal(E,A,B,C,D),
        rel_pron(E,F).

% name/5:
name(C,A,E,B,G) :- 
        opt_the(A,D,B,F),
        terminal(C,D,E,F,G),
        name(C).

% int_art/7:
int_art(A,plu,quant(same,wh(A)),B,E,C,G) :- 
        quote(how,B,D,C,F),
        quote(many,D,E,F,G).
int_art(F,G,H,A,B,C,D) :- 
        terminal(E,A,B,C,D),
        int_art(E,F,G,H).

% int_pron/5:
int_pron(F,A,B,C,D) :- 
        terminal(E,A,B,C,D),
        int_pron(E,F).

% adverb/5:
adverb(adv(A),B,C,D,E) :- 
        terminal(A,B,C,D,E),
        adverb(A).

% poss_pron/6:
poss_pron(pronoun(F),G+H,A,B,C,D) :- 
        terminal(E,A,B,C,D),
        poss_pron(E,F,G,H).

% pers_pron/7:
pers_pron(pronoun(F),G+H,I,A,B,C,D) :- 
        terminal(E,A,B,C,D),
        pers_pron(E,F,G,H,I).

% quantifier_pron/6:
quantifier_pron(F,G,A,B,C,D) :- 
        terminal(E,A,B,C,D),
        quantifier_pron(E,F,G).

% context_pron/6:
context_pron(prep(in),place,A,B,C,D) :- 
        quote(where,A,B,C,D).
context_pron(prep(at),time,A,B,C,D) :- 
        quote(when,A,B,C,D).

% number/6:
number(nb(F),G,A,B,C,D) :- 
        terminal(E,A,B,C,D),
        number(E,F,G).

% terminator/5:
terminator(F,A,B,C,D) :- 
        terminal(E,A,B,C,D),
        terminator(E,F).

% opt_the/4:
opt_the(A,A,B,B).
opt_the(A,B,C,D) :- 
        quote(the,A,B,C,D).

% conj/7:
conj(_,list,list,A,B,C,D) :- 
        terminal((','),A,B,C,D).
conj(A,list,end,B,C,D,E) :- 
        terminal(A,B,C,D,E),
        conj(A).

% loc_pred/5:
loc_pred(F,A,B,C,D) :- 
        terminal(E,A,B,C,D),
        loc_pred(E,F).

% quote/5:
quote(A,B,C,D,E) :- 
        terminal(A,B,C,D,E),
        quote(A).

% word/1:
word(A) :- 
        quote(A).
word(A) :- 
        conj(A).
word(A) :- 
        adverb(A).
word(A) :- 
        sup_adj(A,_).
word(A) :- 
        rel_adj(A,_).
word(A) :- 
        adj(A,_).
word(A) :- 
        name(A).
word(A) :- 
        terminator(A,_).
word(A) :- 
        pers_pron(A,_,_,_,_).
word(A) :- 
        poss_pron(A,_,_,_).
word(A) :- 
        rel_pron(A,_).
word(A) :- 
        verb_form(A,_,_,_).
word(A) :- 
        noun_form(A,_,_).
word(A) :- 
        prep(A).
word(A) :- 
        quantifier_pron(A,_,_).
word(A) :- 
        number(A,_,_).
word(A) :- 
        det(A,_,_,_).
word(A) :- 
        int_art(A,_,_,_).
word(A) :- 
        int_pron(A,_).
word(A) :- 
        loc_pred(A,_).

% quote/1:
quote(how).
quote(whose).
quote(there).
quote(of).
quote(quote).
quote((',')).
quote(s).
quote(than).
quote(at).
quote(the).
quote(not).
quote(as).
quote(that).
quote(less).
quote(more).
quote(least).
quote(most).
quote(many).
quote(where).
quote(when).

% conj/1:
conj(and).
conj(or).

% int_pron/2:
int_pron(what,undef).
int_pron(which,undef).
int_pron(who,subj).
int_pron(whom,compl).

% int_art/4:
int_art(what,A,_,int_det(A)).
int_art(which,A,_,int_det(A)).

% det/4:
det(the,A,the(A),def).
det(a,sin,a,indef).
det(an,sin,a,indef).
det(every,sin,every,indef).
det(some,_,some,indef).
det(any,_,any,indef).
det(all,plu,all,indef).
det(each,sin,each,indef).
det(no,_,no,indef).

% number/3:
number(A,B,C) :- 
        tr_number(A,B),
        ag_number(B,C).

% tr_number/2:
tr_number(nb(A),A).
tr_number(one,1).
tr_number(two,2).
tr_number(three,3).
tr_number(four,4).
tr_number(five,5).
tr_number(six,6).
tr_number(seven,7).
tr_number(eight,8).
tr_number(nine,9).
tr_number(ten,10).

% ag_number/2:
ag_number(1,sin).
ag_number(A,plu) :- 
        A > 1.

% quantifier_pron/3:
quantifier_pron(everybody,every,person).
quantifier_pron(everyone,every,person).
quantifier_pron(everything,every,thing).
quantifier_pron(somebody,some,person).
quantifier_pron(someone,some,person).
quantifier_pron(something,some,thing).
quantifier_pron(anybody,any,person).
quantifier_pron(anyone,any,person).
quantifier_pron(anything,any,thing).
quantifier_pron(nobody,no,person).
quantifier_pron(nothing,no,thing).

% prep/1:
prep(as).
prep(at).
prep(of).
prep(to).
prep(by).
prep(with).
prep(in).
prep(on).
prep(from).
prep(into).
prep(through).

% noun_form/3:
noun_form(A,B,plu) :- 
        noun_plu(A,B).
noun_form(A,A,sin) :- 
        noun_sin(A).
noun_form(proportion,proportion,_).
noun_form(percentage,percentage,_).

% root_form/1:
root_form(1+sin).
root_form(2+_).
root_form(1+plu).
root_form(3+plu).

% verb_root/1:
verb_root(be).
verb_root(have).
verb_root(do).
verb_root(border).
verb_root(contain).
verb_root(drain).
verb_root(exceed).
verb_root(flow).
verb_root(rise).

% regular_pres/1:
regular_pres(have).
regular_pres(do).
regular_pres(rise).
regular_pres(border).
regular_pres(contain).
regular_pres(drain).
regular_pres(exceed).
regular_pres(flow).

% regular_past/2:
regular_past(had,have).
regular_past(bordered,border).
regular_past(contained,contain).
regular_past(drained,drain).
regular_past(exceeded,exceed).
regular_past(flowed,flow).

% rel_pron/2:
rel_pron(who,subj).
rel_pron(whom,compl).
rel_pron(which,undef).

% poss_pron/4:
poss_pron(my,_,1,sin).
poss_pron(your,_,2,_).
poss_pron(his,masc,3,sin).
poss_pron(her,fem,3,sin).
poss_pron(its,neut,3,sin).
poss_pron(our,_,1,plu).
poss_pron(their,_,3,plu).

% pers_pron/5:
pers_pron(i,_,1,sin,subj).
pers_pron(you,_,2,_,_).
pers_pron(he,masc,3,sin,subj).
pers_pron(she,fem,3,sin,subj).
pers_pron(it,neut,3,sin,_).
pers_pron(we,_,1,plu,subj).
pers_pron(them,_,3,plu,subj).
pers_pron(me,_,1,sin,compl(_)).
pers_pron(him,masc,3,sin,compl(_)).
pers_pron(her,fem,3,sin,compl(_)).
pers_pron(us,_,1,plu,compl(_)).
pers_pron(them,_,3,plu,compl(_)).

% terminator/2:
terminator('.',_).
terminator(?,?).
terminator(!,!).

% name/1:
name(_).

% loc_pred/2:
loc_pred(east,prep(eastof)).
loc_pred(west,prep(westof)).
loc_pred(north,prep(northof)).
loc_pred(south,prep(southof)).

% adj/2:
adj(minimum,restr).
adj(maximum,restr).
adj(average,restr).
adj(total,restr).
adj(african,restr).
adj(american,restr).
adj(asian,restr).
adj(european,restr).
adj(great,quant).
adj(big,quant).
adj(small,quant).
adj(large,quant).
adj(old,quant).
adj(new,quant).
adj(populous,quant).

% rel_adj/2:
rel_adj(greater,great).
rel_adj(less,small).
rel_adj(bigger,big).
rel_adj(smaller,small).
rel_adj(larger,large).
rel_adj(older,old).
rel_adj(newer,new).

% sup_adj/2:
sup_adj(biggest,big).
sup_adj(smallest,small).
sup_adj(largest,large).
sup_adj(oldest,old).
sup_adj(newest,new).

% noun_sin/1:
noun_sin(average).
noun_sin(total).
noun_sin(sum).
noun_sin(degree).
noun_sin(sqmile).
noun_sin(ksqmile).
noun_sin(thousand).
noun_sin(million).
noun_sin(time).
noun_sin(place).
noun_sin(area).
noun_sin(capital).
noun_sin(city).
noun_sin(continent).
noun_sin(country).
noun_sin(latitude).
noun_sin(longitude).
noun_sin(ocean).
noun_sin(person).
noun_sin(population).
noun_sin(region).
noun_sin(river).
noun_sin(sea).
noun_sin(seamass).
noun_sin(number).

% noun_plu/2:
noun_plu(averages,average).
noun_plu(totals,total).
noun_plu(sums,sum).
noun_plu(degrees,degree).
noun_plu(sqmiles,sqmile).
noun_plu(ksqmiles,ksqmile).
noun_plu(million,million).
noun_plu(thousand,thousand).
noun_plu(times,time).
noun_plu(places,place).
noun_plu(areas,area).
noun_plu(capitals,capital).
noun_plu(cities,city).
noun_plu(continents,continent).
noun_plu(countries,country).
noun_plu(latitudes,latitude).
noun_plu(longitudes,longitude).
noun_plu(oceans,ocean).
noun_plu(persons,person).
noun_plu(people,person).
noun_plu(populations,population).
noun_plu(regions,region).
noun_plu(rivers,river).
noun_plu(seas,sea).
noun_plu(seamasses,seamass).
noun_plu(numbers,number).

% verb_form/4:
verb_form(A,A,inf,_) :- 
        verb_root(A).
verb_form(A,A,pres+fin,B) :- 
        regular_pres(A),
        root_form(B),
        verb_root(A).
verb_form(A,B,past+_,_) :- 
        regular_past(A,B).
verb_form(am,be,pres+fin,1+sin).
verb_form(are,be,pres+fin,2+sin).
verb_form(is,be,pres+fin,3+sin).
verb_form(are,be,pres+fin,_+plu).
verb_form(was,be,past+fin,1+sin).
verb_form(were,be,past+fin,2+sin).
verb_form(was,be,past+fin,3+sin).
verb_form(were,be,past+fin,_+plu).
verb_form(been,be,past+part,_).
verb_form(being,be,pres+part,_).
verb_form(has,have,pres+fin,3+sin).
verb_form(having,have,pres+part,_).
verb_form(does,do,pres+fin,3+sin).
verb_form(did,do,past+fin,_).
verb_form(doing,do,pres+part,_).
verb_form(done,do,past+part,_).
verb_form(flows,flow,pres+fin,3+sin).
verb_form(flowing,flow,pres+part,_).
verb_form(rises,rise,pres+fin,3+sin).
verb_form(rose,rise,past+fin,_).
verb_form(risen,rise,past+part,_).
verb_form(borders,border,pres+fin,3+sin).
verb_form(bordering,border,pres+part,_).
verb_form(contains,contain,pres+fin,3+sin).
verb_form(containing,contain,pres+part,_).
verb_form(drains,drain,pres+fin,3+sin).
verb_form(draining,drain,pres+part,_).
verb_form(exceeds,exceed,pres+fin,3+sin).
verb_form(exceeding,exceed,pres+part,_).

% verb_type/2:
verb_type(have,aux+have).
verb_type(be,aux+be).
verb_type(do,aux+ditrans).
verb_type(rise,main+intrans).
verb_type(border,main+trans).
verb_type(contain,main+trans).
verb_type(drain,main+intrans).
verb_type(exceed,main+trans).
verb_type(flow,main+intrans).

% adverb/1:
adverb(yesterday).
adverb(tomorrow).

