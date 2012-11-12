go:-tbug.

% SOLVED - by ensuring unique ids
% exhibits tcall/4 bug at small scale

tbug:-quiet(1),bgtask,bgtask.

bgtask:-
  println(making_bg_task),
  create_engine(1000,300,300,Engine),
  load_engine(Engine,topcall(loop(300)),_),
  ask_thread(Engine,Thread),
  sleep(4),
  ENGINE_ID=7,
  get_engine_prop(Engine,ENGINE_ID,ID),
  destroy_engine(Engine),
  println(destroying(engine(Engine),id(ID),thread(Thread))).

loop(N):-
   for(I,1,N),
     println(wait_step(I)),
     sleep(1),
   fail.
loop(_).

badcall(G):-catch(metacall(G),_,true).

/*
 - the bug seems to come from the fact
that assumptions use an engine number - which
if the engines memory address is reused will refer
to dead memory areas
=> make assumptions use always new numbers - or make
engine ids alswas new - independently of their
recycling mechanism
*/

/* pbench: also in assertbm - FIXED - by making sure assumptions are ignored
in bb_gc

index took
 user cpu (5148-380)/364431 = 0.0130834 ms/iteration
4 ??? warning: *** bad stamp or empty slot in set/3 or rm/2 ***
>>> blackboard overflow, left only(126476): culprit(bb_put_1([1206000,1212588],4
,cont_marker(_x109229),f(3,_x2322)))
*** discarding inconsistent blackboard content
*** BB_OVERFLOW:
addq(user,(null)/0 ??? warning: *** bad data in args(1) of name/2 ***
(addq(user,(null)/0 ??? warning: *** bad data in args(1) of name/2 ***
*/