%% ===================================================================

%% Description: Speed tests for meta predicates.

%% RCS: $Id: meta_tests.pl,v 1.1 1996/11/24 11:48:47 griffith Exp $

%%% Simple test predicate --------------------------------------------
is_a(a).

%%% Non-meta predicate -----------------------------------------------
is_a_list_1([]).
is_a_list_1([F|R]) :-
   is_a(F),
   is_a_list_1(R).

%%% Non-meta predicate with call -------------------------------------
is_a_list_2([]).
is_a_list_2([F|R]) :-
   call(is_a(F)),
   is_a_list_2(R).

%%% Meta predicate with call -----------------------------------------
map_list_1([],_G).
map_list_1([F|R],G) :-
   %% Simulate call/2.
   functor(Term,G,1),
   arg(1,Term,F),
   call(Term),
   map_list_1(R,G).

%%% Meta predicate with hook -----------------------------------------
map_list_2([],_G).
map_list_2([F|R],G) :-
   map_list_hook(G,F),
   map_list_2(R,G).

map_list_hook(is_a,X) :- is_a(X).

%%% Generate a list of atoms -----------------------------------------
gen_a_list(0,[]) :- !.
gen_a_list(N,[a|R]) :-
   N1 is N - 1,
   gen_a_list(N1,R).

%%% Test suite -------------------------------------------------------

%% For an alternative, see the definition of cpu_time/3 in "The Craft
%% of Prolog", by Richard A. O'Keefe.
%:- use_module(library(benchmark),[time/3]).

test_is_a_list_1(N) :- gen_a_list(N,L), is_a_list_1(L).
test_is_a_list_2(N) :- gen_a_list(N,L), is_a_list_2(L).
test_map_list_1(N)  :- gen_a_list(N,L), map_list_1(L,is_a).
test_map_list_2(N)  :- gen_a_list(N,L), map_list_2(L,is_a).

test_all(ListSize,Count,InternalCount) :-
   write('Non-meta predicate:'),
   time(test_is_a_list_1(ListSize),Count,InternalCount),nl,
   %%
   write('Non-meta predicate with call:'),
   time(test_is_a_list_2(ListSize),Count,InternalCount),nl,
   %%
   write('Meta-predicate with call:'),
   time(test_map_list_1(ListSize),Count,InternalCount),nl,
   %%
   write('Meta-predicate with hook:'),
   time(test_map_list_2(ListSize),Count,InternalCount).

test1 :- test_all(1000,10,10).
test2 :- test_all(10000,10,10).

% 
time(G,N,_):-
  ctime(T1),
  (for(_,1,N),G,fail;true),
  ctime(T2),T is T2-T1,
  write(T),nl.

/* RESULTS

These were performed on a Sun UltraSPARC 1
with Quintus Prolog Release 3.2 (Sun 4, SunOS 5.4).

******************************* Test 1 *******************************

Non-meta predicate:
 
Time   : 2.2 milliseconds / iteration
Time   : 2.2 milliseconds / iteration (counting gc and shift time)
Time   : 220 msec total (220 compute + 0 overhead + 0 gc + 0 shift)
Global : 8000 bytes / iteration
Global : 800092 bytes total (799980 actual + 112 overhead)
Local  : 0 bytes / iteration 
Local  : 0 total (0 actual + 0 overhead)
Gc     : 0 milliseconds for 0 gcs freeing 0 bytes
Shift  : 0 milliseconds for 0 global and 3 local shifts
 
Non-meta predicate with call:
 
Time   : 34.9 milliseconds / iteration
Time   : 36.7 milliseconds / iteration (counting gc and shift time)
Time   : 3670 msec total (3490 compute + 0 overhead + 180 gc + 0 shift)
Global : 28002 bytes / iteration
Global : 2800316 bytes total (2800204 actual + 112 overhead)
Local  : 0 bytes / iteration 
Local  : 0 total (0 actual + 0 overhead)
Gc     : 180 milliseconds for 3 gcs freeing 3301528 bytes
Shift  : 0 milliseconds for 0 global and 0 local shifts
 
Meta-predicate with call:
 
Time   : 37.8 milliseconds / iteration
Time   : 38.9 milliseconds / iteration (counting gc and shift time)
Time   : 3890 msec total (3780 compute + 0 overhead + 110 gc + 0 shift)
Global : 28001 bytes / iteration
Global : 2800248 bytes total (2800136 actual + 112 overhead)
Local  : 0 bytes / iteration 
Local  : 0 total (0 actual + 0 overhead)
Gc     : 110 milliseconds for 2 gcs freeing 2204484 bytes
Shift  : 0 milliseconds for 0 global and 0 local shifts
 
Meta-predicate with hook:
 
Time   : 2.8 milliseconds / iteration
Time   : 3.3 milliseconds / iteration (counting gc and shift time)
Time   : 330 msec total (280 compute + 0 overhead + 50 gc + 0 shift)
Global : 8001 bytes / iteration
Global : 800180 bytes total (800068 actual + 112 overhead)
Local  : 0 bytes / iteration 
Local  : 0 total (0 actual + 0 overhead)
Gc     : 50 milliseconds for 1 gcs freeing 1100352 bytes
Shift  : 0 milliseconds for 0 global and 0 local shifts

******************************* Test 2 *******************************

Non-meta predicate:
 
Time   : 23.0 milliseconds / iteration
Time   : 28.7 milliseconds / iteration (counting gc and shift time)
Time   : 2870 msec total (2300 compute + 0 overhead + 570 gc + 0 shift)
Global : 80004 bytes / iteration
Global : 8000540 bytes total (8000428 actual + 112 overhead)
Local  : 0 bytes / iteration 
Local  : 0 total (0 actual + 0 overhead)
Gc     : 570 milliseconds for 5 gcs freeing 7122860 bytes
Shift  : 0 milliseconds for 0 global and 4 local shifts
 
Non-meta predicate with call:
 
Time   : 348.5 milliseconds / iteration
Time   : 365.5 milliseconds / iteration (counting gc and shift time)
Time   : 36550 msec total (34850 compute + 0 overhead + 1700 gc + 0 shift)
Global : 280018 bytes / iteration
Global : 28001868 bytes total (28001756 actual + 112 overhead)
Local  : 0 bytes / iteration 
Local  : 0 total (0 actual + 0 overhead)
Gc     : 1700 milliseconds for 17 gcs freeing 27608664 bytes
Shift  : 0 milliseconds for 0 global and 0 local shifts
 
Meta-predicate with call:
 
Time   : 375.3 milliseconds / iteration
Time   : 392.5 milliseconds / iteration (counting gc and shift time)
Time   : 39250 msec total (37530 compute + 0 overhead + 1720 gc + 0 shift)
Global : 280019 bytes / iteration
Global : 28001988 bytes total (28001876 actual + 112 overhead)
Local  : 0 bytes / iteration 
Local  : 0 total (0 actual + 0 overhead)
Gc     : 1720 milliseconds for 18 gcs freeing 29244744 bytes
Shift  : 0 milliseconds for 0 global and 0 local shifts
 
Meta-predicate with hook:
 
Time   : 28.1 milliseconds / iteration
Time   : 32.3 milliseconds / iteration (counting gc and shift time)
Time   : 3230 msec total (2810 compute + 0 overhead + 420 gc + 0 shift)
Global : 80004 bytes / iteration
Global : 8000504 bytes total (8000392 actual + 112 overhead)
Local  : 0 bytes / iteration 
Local  : 0 total (0 actual + 0 overhead)
Gc     : 420 milliseconds for 4 gcs freeing 6387768 bytes
Shift  : 0 milliseconds for 0 global and 0 local shifts

*/

