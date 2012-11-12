% Backproagation algorithm for Art. Neural Network training
% Author: Paul Tarau 1993

go:-statistics(runtime,_),train,statistics(runtime,[_,T]),write(time=T),nl.

out_check(100).

% data
	input(1,1,0.03). input(1,2,0.03).
	input(2,1,0.97). input(2,2,0.03).
	input(3,1,0.03). input(3,2,0.97).
	input(4,1,0.97). input(4,2,0.97).

	output(1,1,0.03). output(1,2,0.03).
	output(2,1,0.03). output(2,2,0.97).
	output(3,1,0.03). output(3,2,0.97).
	output(4,1,0.97). output(4,2,0.03).
	
sum([],S0,S0):-!.
sum([X|Xs],S1,S3):-
	S2 is S1+X,
	sum(Xs,S2,S3).

def2(N,I,J,X):-K is 5*I+J,bb_def(N,K,X).
val2(N,I,J,X):-K is 5*I+J,val(N,K,X).
put2(N,I,J,X):-K is 5*I+J,bb_set(N,K,X).

def_weight(I,J,K,X):-L is 3*I+J,bb_def(L,K,X).
get_weight(I,J,K,X):-L is 3*I+J,val(L,K,X).
put_weight(I,J,K,X):-L is 3*I+J,bb_set(L,K,X).

initialize :-
	init_err_out
;	init_weight
;	true.

init_err_out:-
	for(I,0,2),
	  for(J,0,2),
	    def2(err,I,J,0.0),
	    def2(out,I,J,0.97),
	fail.

init_weight:-	
	for(I,1,2),
	  for(J,1,2),
	    for(K,0,2),
	      random(100,R),Rand is R/100.0,
	      W is 0.2*Rand-0.1,
	      def_weight(I,J,K,W),
	fail.
	  	
forward_pass(Row,Col):-
	findall(O,forward_calc(Row,Col,O),Os),
	sum(Os,0.0,Sum),NSum is -Sum,exp(NSum,Exp),
        Out is 1.0/(1.0+Exp),
	put2(out,Row,Col,Out),
	put2(err,Row,Col,0.0).

forward_calc(Row,Col,Sum):-
	Row1 is Row-1,
	for(J,0,2),
	val2(out,Row1,J,Out1),
	get_weight(Row,Col,J,W),
	Sum is W*Out1.

backward_data(Row,Col,Row1,OutFactor):-
	Row1  is  Row-1,
	val2(out,Row,Col,Out),
	val2(err,Row,Col,Err),
	OutFactor is Err*Out*(1-Out).

backward_calc(Row,Col,Row1,OutFactor):-
	for(J,1,2),
	  val2(err,Row1,J,OldE),
	  get_weight(Row,Col,J,W),
	  NewE is OldE+OutFactor*W,
	  put2(err,Row1,J,NewE),
	fail.
backward_calc(Row,Col,Row1,OutFactor):-
	for(J,0,2),
	  get_weight(Row,Col,J,OldW),
	  val2(out,Row1,J,Out1),
	  NewW is OldW+0.95*OutFactor*Out1,
	  put_weight(Row,Col,J,NewW),
	fail.
backward_calc(_Row,_Col,_Row1,_OutFactor).
	
backward_pass(Row,Col):-
	backward_data(Row,Col,Row1,OutFactor),
	backward_calc(Row,Col,Row1,OutFactor).

err_squared(Sqr):-
	for(J,0,2),
	val2(err,2,J,Err),
	Sqr is Err*Err.

iterate(Step,ErrSqr):-
	get_input(Step)
;	forward_pass
;	calculate_errors(Step)
;	backward_pass
;	final_error(ErrSqr).

get_input(Step):-
	for(J,1,2),
	  input(Step,J,Output0),
	  put2(out,0,J,Output0),
	fail.

forward_pass:-
	for(Row,1,2),
	  for(Col,1,2),
	    forward_pass(Row,Col),
	fail.

calculate_errors(Step):-
	for(J,1,2),
	  output(Step,J,DesiredOut),
	  val2(out,2,J,Out),
	  NewErr is DesiredOut-Out,
	  put2(err,2,J,NewErr),
	fail.

backward_pass:-
	for(Row,-2,-1),
	  for(Col,1,2),
	    DownToRow is -Row,
	    backward_pass(DownToRow,Col),
	fail.

final_error(ErrSqr):-
	findall(E,err_squared(E),Es),
	sum(Es,0.0,ErrSqr).

big_error(Iter,Sqr):-
	for(Step,1,4),
	iterate(Step,Sqr),
	show_cells(Iter,Step,Sqr),
	Sqr> 5.0e-3.

	
learn :- 
	for(Iter,0,32000),
	  findall(E,big_error(Iter,E),[]).

show_in_out:-
	write('Input:'),nl,
	for(I,1,4),
	  nl,
	  for(J,1,2),
	    input(I,J,Inp),
	    write(Inp),write(' '),
	fail
;
	nl,nl,write('Desired Output:'),nl,
	for(I,1,4),
	  nl,
	  for(J,1,2),
	    output(I,J,Out),
	    write(Out),write(' '),
	fail	
;
	nl,nl,
	write('SquaredError  Output'),
	nl.

show_cells(Iter,Step,ErrSqr):-
        out_check(When),
        Iter mod When =:= 0,
	show_cells1(Iter,Step,ErrSqr)
;
        true.

show_cells1(Iter,Step,ErrSqr):-
	(
		N is Iter*4+Step,   
		nl,write(N-[ErrSqr]),
		print_cells
	;
		Step =:= 4, nl
	),
	fail.


print_cells:-
		for(J,1,2),
		  val2(out,2,J,Out),
		  write(' '),write(Out),
		fail
;	true.

show_weights:-
	nl,write('Final weights'),nl,
	for(I,1,2),
	  for(J,1,2),
	    nl,
	    for(K,0,2),
	      get_weight(I,J,K,W),
	      write(W),write(' '),
	fail
; 	
	nl.    

rtest:-for(_,1,30),random(20,R),write(R),nl,fail.

random(Max,R):-random(X), R is X mod Max.


train:-
      random_seed(103),
	initialize,
	show_in_out,
	learn,
	show_weights.

