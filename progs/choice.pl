/* The predicates are called:                             */

/*    o  "choice_point(N)"    - creation of choice points    */
/*    o  "choice_point0ar(N)  - same, with 0 arg             */
/*    o  "baktrak1(N)"        - deep backtracking            */
/*    o  "baktrak2(N)"        - shallow backtracking         */

/*  N is the number of loop iterations executed  */
:-write('run with -h1000 -s6000'),nl.

go:-go('BMARK_choice:').

go(Mes):-N=10000,
  statistics(global_stack,[H1,_]),
  statistics(local_stack,[S1,_]),
  statistics(trail,[TR1,_]),
  statistics(runtime,[T1,_]),
  choice_point(N),
  baktrak1(N),
  baktrak2(N),
  statistics(runtime,[T2,_]),T is T2-T1,
  statistics(global_stack,[H2,_]),H is H2-H1,
  statistics(trail,[TR2,_]),
  statistics(local_stack,[S2,_]),S is S2-S1,TR is TR2-TR1,
  write(Mes=[time=T,stack=S,heap=H,trail=TR]),nl.

p:-[choice].

/* predicate to test creation of choice points without backtracking */
/* suggested value for N: 1000 */
/* results for  Cprolog N=1000 */
/* Tloop=5.95 Tcompens=0.98 Tnet=4.97 Klips=4.02 */

choice_point(N):-statistics(runtime,[T1|_]),
        cre_CP(N), statistics(runtime,[T2|_]),
        compens_loop(N), statistics(runtime,[T3|_]),
        print_times(T1,T2,T3,N,20).

/* predicate choice_point, but with zero argument  */
/* suggested value for N: 1000 */
/* results for Cprolog: N=1000 */
/* Tloop=3.55 Tcompens=0.98 Tnet=2.57 Klips=7.7  */


choice_point0ar(N):-statistics(runtime,[T1|_]),
        cre_CP0ar(N), statistics(runtime,[T2|_]),
        compens_loop(N), statistics(runtime,[T3|_]),
        print_times(T1,T2,T3,N,20).
/* Predicate to test the (deep) backtracking mechanism. */
/* suggested value for N: 1000 (interp), 2000(comp) */
/* results for Cprolog: N=1000  */
/* Tloop=9.63 Tcomp=1 Tnet=8.63 Klips=2.32  */

baktrak1(N)
     :- statistics(runtime,[T1|_]),
        deep_back(N),
        statistics(runtime,[T2|_]),
        compens_loop(N),
        statistics(runtime,[T3|_]),
        print_times(T1,T2,T3,N,20).


/* Predicate to test the (shallow) backtracking mechanism */
/* suggested value for N: 1000 (interp), 2000 (comp) */
/* results for Cprolog: N=1000  */
/* Tloop=3.63  Tcomp=0.95 Tnet=2.68 Klips=7.45 */

baktrak2(X)
     :- statistics(runtime,[T1|_]),
        shallow_back(X), statistics(runtime,[T2|_]),
        compens_loop(X), statistics(runtime,[T3|_]),
        print_times(T1,T2,T3,X,20).


/* compensation loop, used to measure the time spent in the loop  */
compens_loop(0).
compens_loop(X) :- Y is X - 1, compens_loop(Y).

/* loop to test choice point creation   */
cre_CP(0).
cre_CP(N):-M is N-1, ccp1(0,0,0), cre_CP(M).

cre_CP0ar(0).
cre_CP0ar(N):-M is N-1, ccp1, cre_CP0ar(M).

/* loop to test deep backtracking       */
deep_back(0).
deep_back(X) :- pd(_,_,_), Y is X - 1, deep_back(Y).

/* loop to test shallow backtracking */
shallow_back(0).
shallow_back(X) :- ps(_,a,b), Y is X - 1, shallow_back(Y).


print_times(T1,T2,T3,X,I) :-        /* prints the results */
        TT1 is T2 - T1,
        TT2 is T3 - T2,
        TT is TT1 - TT2,
        write('T overall loop:   '),write(TT1), nl,
        write('T compens loop:   '),write(TT2), nl,
        write('T net:            '),write(TT),nl,
        write('Lips:            '),
        Li is I * X,
        Lips is 1000*Li // TT,
%        KLips is Lips / 1000,
        write(Lips),nl,nl.

/*  ccp1 creates 20 choice points */
/* ccp1 is the beginning of a set of predicates  */
/* composed of 2 clauses each. Every invokation of nd0 will create */
/* a sequence of 20 choice points. The body of the clauses are     */
/* limited to one goal, thus avoiding a creation of environment    */
/* when the clause is activated. nd0, and its successors, have     */
/*   three arguments to comply with our average static analysis    */
/*   results made on more than 30 real Prolog programs.            */
/* ccpXX exists with 3 arguments, and 0 args. */

ccp1(X,Y,Z):-ccp2(X,Y,Z).
ccp1(X,Y,Z).
ccp2(X,Y,Z):-ccp3(X,Y,Z).
ccp2(X,Y,Z).
ccp3(X,Y,Z):-ccp4(X,Y,Z).
ccp3(X,Y,Z).
ccp4(X,Y,Z):-ccp5(X,Y,Z).
ccp4(X,Y,Z).
ccp5(X,Y,Z):-ccp6(X,Y,Z).
ccp5(X,Y,Z).
ccp6(X,Y,Z):-ccp7(X,Y,Z).
ccp6(X,Y,Z).
ccp7(X,Y,Z):-ccp8(X,Y,Z).
ccp7(X,Y,Z).
ccp8(X,Y,Z):-ccp9(X,Y,Z).
ccp8(X,Y,Z).
ccp9(X,Y,Z):-ccp10(X,Y,Z).
ccp9(X,Y,Z).
ccp10(X,Y,Z):-ccp11(X,Y,Z).
ccp10(X,Y,Z).
ccp11(X,Y,Z):-ccp12(X,Y,Z).
ccp11(X,Y,Z).
ccp12(X,Y,Z):-ccp13(X,Y,Z).
ccp12(X,Y,Z).
ccp13(X,Y,Z):-ccp14(X,Y,Z).
ccp13(X,Y,Z).
ccp14(X,Y,Z):-ccp15(X,Y,Z).
ccp14(X,Y,Z).
ccp15(X,Y,Z):-ccp16(X,Y,Z).
ccp15(X,Y,Z).
ccp16(X,Y,Z):-ccp17(X,Y,Z).
ccp16(X,Y,Z).
ccp17(X,Y,Z):-ccp18(X,Y,Z).
ccp17(X,Y,Z).
ccp18(X,Y,Z):-ccp19(X,Y,Z).
ccp18(X,Y,Z).
ccp19(X,Y,Z):-ccp20(X,Y,Z).
ccp19(X,Y,Z).

ccp20(X,Y,Z).
ccp20(X,Y,Z).

ccp1:-ccp2.
ccp1.
ccp2:-ccp3.
ccp2.
ccp3:-ccp4.
ccp3.
ccp4:-ccp5.
ccp4.
ccp5:-ccp6.
ccp5.
ccp6:-ccp7.
ccp6.
ccp7:-ccp8.
ccp7.
ccp8:-ccp9.
ccp8.
ccp9:-ccp10.
ccp9.
ccp10:-ccp11.
ccp10.
ccp11:-ccp12.
ccp11.
ccp12:-ccp13.
ccp12.
ccp13:-ccp14.
ccp13.
ccp14:-ccp15.
ccp14.
ccp15:-ccp16.
ccp15.
ccp16:-ccp17.
ccp16.
ccp17:-ccp18.
ccp17.
ccp18:-ccp19.
ccp18.
ccp19:-ccp20.
ccp19.

ccp20.
ccp20.


/*  deep backtracking */
/*  The call to pd creates a choice point, and invokes a      */
/*  call to q. It will fail and there will be a backtracking  */
/*  step  to try the next clause defining pd. pd has 21       */
/*  clauses,thus failure                                      */
/*  occurs 20 times                                           */

pd(X1,X2,_) :- q(X1,X2,a).
pd(X1,X2,_) :- q(X1,X2,a).
pd(X1,X2,_) :- q(X1,X2,a).
pd(X1,X2,_) :- q(X1,X2,a).
pd(X1,X2,_) :- q(X1,X2,a).
pd(X1,X2,_) :- q(X1,X2,a).
pd(X1,X2,_) :- q(X1,X2,a).
pd(X1,X2,_) :- q(X1,X2,a).
pd(X1,X2,_) :- q(X1,X2,a).
pd(X1,X2,_) :- q(X1,X2,a).
pd(X1,X2,_) :- q(X1,X2,a).
pd(X1,X2,_) :- q(X1,X2,a).
pd(X1,X2,_) :- q(X1,X2,a).
pd(X1,X2,_) :- q(X1,X2,a).
pd(X1,X2,_) :- q(X1,X2,a).
pd(X1,X2,_) :- q(X1,X2,a).
pd(X1,X2,_) :- q(X1,X2,a).
pd(X1,X2,_) :- q(X1,X2,a).
pd(X1,X2,_) :- q(X1,X2,a).
pd(X1,X2,_) :- q(X1,X2,a).
pd(X1,X2,_).

q(X1,X2,b).


/*   shallow backtracking */
/*   The ps predicate fails 20 times. The shallow backtracking   */
/*   will not restore all current state registers in Prolog      */
/*   systems which perform this optimisation, while others will. */

ps(_,X,X).
ps(_,X,X).
ps(_,X,X).
ps(_,X,X).
ps(_,X,X).
ps(_,X,X).
ps(_,X,X).
ps(_,X,X).
ps(_,X,X).
ps(_,X,X).
ps(_,X,X).
ps(_,X,X).
ps(_,X,X).
ps(_,X,X).
ps(_,X,X).
ps(_,X,X).
ps(_,X,X).
ps(_,X,X).
ps(_,X,X).
ps(_,X,X).
ps(_,_,_).
