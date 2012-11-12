% Requires BinProlog 5.40

% Datalog `preprocessor' : made obsolete by the possibility
% to override BinProlog's generic Assumption Grammar mechanism.
% well, the DCG preprocessor is made obsolete too :-)

% CODE

% this defines advancement in Datalog `phrase' given as a set of w/3 facts

w(X):-
  dcg_val(From),
  w(From,To,X),
  dcg_def(To).

% this recognizes a phrase From..To

dlg_phrase(From,To):-dcg_def(From),axiom,dcg_val(To).

% DATA

% grammar

axiom:-ng,v.

ng:-a,n.

a:-w(the).
a:-w(a).

n:-w(cat).
n:-w(dog).

v:-w(walks).
v:-w(sleeps).

% recognizing a sentence

% input phrase in Datalog form

w(0,1,the).
w(1,2,cat).
w(2,3,walks).

% TEST

test:-dlg_phrase(0,3).

% ?-test. % will answer yes

/*
?-reconsult(dlg).
..
?-trace(test).

Call: test
 !!! clause: test/0
 Call: dlg_phrase(0,3)
  !!! clause: dlg_phrase/2
  Call: dcg_def(0)
   !!! compiled(dcg_def/1)
  Exit: dcg_def(0)
  Call: axiom
   !!! clause: axiom/0
   Call: ng
    !!! clause: ng/0
    Call: a
     !!! clause: a/0
     Call: w(the)
      !!! clause: w/1
      Call: dcg_val(_x4907)
       !!! compiled(dcg_val/1)
      Exit: dcg_val(0)
      Call: w(0,_x4903,the)
       !!! clause: w/3
      Exit: w(0,1,the)
      Call: dcg_def(1)
       !!! compiled(dcg_def/1)
      Exit: dcg_def(1)
     Exit: w(the)
    Exit: a
    Call: n
     !!! clause: n/0
     Call: w(cat)
      !!! clause: w/1
      Call: dcg_val(_x6533)
       !!! compiled(dcg_val/1)
      Exit: dcg_val(1)
      Call: w(1,_x6529,cat)
       !!! clause: w/3
      Exit: w(1,2,cat)
      Call: dcg_def(2)
       !!! compiled(dcg_def/1)
      Exit: dcg_def(2)
     Exit: w(cat)
    Exit: n
   Exit: ng
   Call: v
    !!! clause: v/0
    Call: w(walks)
     !!! clause: w/1
     Call: dcg_val(_x8159)
      !!! compiled(dcg_val/1)
     Exit: dcg_val(2)
     Call: w(2,_x8155,walks)
      !!! clause: w/3
     Exit: w(2,3,walks)
     Call: dcg_def(3)
      !!! compiled(dcg_def/1)
     Exit: dcg_def(3)
    Exit: w(walks)
   Exit: v
  Exit: axiom
  Call: dcg_val(3)
   !!! compiled(dcg_val/1)
  Exit: dcg_val(3)
 Exit: dlg_phrase(0,3)
Exit: test

*/
