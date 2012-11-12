% PURE TERM-CREATION (PUT) INSTRUCTION SUBSET

% begin chunk
c_spec(c_chunk_variable, Name, c_chunk_variable(Name)):-
% ttyprint(c_chunk_variable=Name),  % $$$
  c_chunk_begin(Name).

% end chunk
c_skip(2).

c_spec_end(c_chunk_value,Op, _, F, _Len , c_chunk_value(O,No1)):-
  c_skip(Extra),
  c_check0(No), No1 is No+Extra,
  c_chunk_end,
  get_offset(O),
%  currpred(Pr/Ar-_),ttyprint(c_chunk_value(Op,size(No1),pred(Pr,Ar))),
  make_bcode(c_chunk_value,Op,0,F,No1).


% inside the chunk: C-instruction

/*
FORMAT:
  - name
  - opcode
  - An register
  - F: functor or Integer
  - N: arity
  - C-instruction: a C-macro to go in the *.h file
  - continuation action (with side effects!) to be or not to be performed
*/

c_instr(put_structure,_,An,F,N, put_structure(O,No,An)):-
   newH_newP(F,N,No,O).

c_instr(put_constant,_, An,F,N, Instr):-
   c_aux_put_integer(An,F,N,Instr).

c_instr(push_variable,_, An, _, _, 'push_variable()'):-
  newH_oldP(O),
  def(offset,An,O).

c_instr(push_structure,_,An,F,N,push_structure(O,No,VarO)):-
   newH_newP(F,N,No,O),
   val(offset,An,VarO),
   rm(offset,An).

c_instr(write_value,_, An, _, _, write_value(O,An)):-
  newH_oldP(O).

c_instr(write_variable,_, An, _, _, write_variable(O,An)):-
  newH_oldP(O).

c_instr(write_constant,_, _, F, N, Instr):-
  c_aux_write_integer(F,N,Instr).

c_instr(push_constant,_, _, F, N, Instr):-
  c_aux_write_integer(F,N,Instr).

c_instr(get_variable,_, An, _, Ai, move_reg(An,Ai)):-
  c_check.

c_instr(put_value,_, An, _, Ai, move_reg(An,Ai)):-
  c_check.

c_instr(put_variable,_, An, _, Ai, put_variable(O,An,Ai)):-
  newH_oldP(O).

c_instr(write_void,_, Times, _ , _, write_void(O,Times)):-
  c_wvoid(Times,O).

c_instr(push_cut,_, _, _, _, push_cut(O)):-
  newH_oldP(O).

% END OF pure PUT... instructions

c_instr(put_cut,_, _, _, _, 'put_cut()'):-
 c_check.

c_instr(get_cut,_, _, _, _, 'get_cut()'):-
  c_check.

c_instr(load_constant,_, An, F, N, Instr):-
  c_aux_load_integer(An,F,N,Instr).

c_instr(load_variable,_, An, _, Ai, load_variable(O,An,Ai)):-
  newH_oldP(O).

c_instr(load_value,_, An, _, Ai, load_value(An,Ai)):-
  c_check.

c_instr(get_value,_, An, _, Ai, get_value(An,Ai)):-
  c_check.

c_instr(get_constant,_, An, F, N, Instr):-
  c_aux_get_integer(An,F,N,Instr).

c_instr(get_structure,_, An,F,N, get_structure(O,No,An)):-
  oldH_newP(F,N,No,O).

c_instr(unify_variable,_,An,_,_,     unify_variable(O,An)):-
  oldH_oldP(O).

c_instr(unify_value,_,   An,_,_,     unify_value(O,An)):-
  oldH_oldP(O).

c_instr(unify_constant,_,_,F,N,Instr):-
  c_aux_unify_integer(F,N,Instr).

c_instr(unify_void,_, Times, _ , _,  unify_void(O,Times)):-
  oldH_oldP(O).

c_instr(inline_variable,_,_,_,No,Instr):-
  c_check,
  c_check_iv(No,Instr).

c_instr(arith_variable,Base,An,N,No,Instr):-
  c_check_av(No,OpCode,An,N,Instr),
  c_opcode(Base,No,OpCode).
c_instr(arith_value,Base,An,N,No,Instr):-
  c_check_av(No,OpCode,An,N,Instr),
  c_opcode(Base,No,OpCode).

c_check_av(0,OpCode,An,N,arith_op(+,OpCode,An,N,O)):-oldH_oldP(O).
c_check_av(1,OpCode,An,N,arith_op(-,OpCode,An,N,O)):-oldH_oldP(O).
c_check_av(2,OpCode,An,N,arith_op(*,OpCode,An,N,O)):-oldH_oldP(O).
c_check_av(3,OpCode,An,N,arith_op('%',OpCode,An,N,O)):-oldH_oldP(O).
c_check_av(4,OpCode,An,N,div_3(OpCode,An,N)).
c_check_av(5,OpCode,An,N,fdiv_3(OpCode,An,N,O)):-oldH_oldP(O).
c_check_av(6,_OpCode,An,N,random_1(An,N)).
c_check_av(7,_OpCode,An,N,get0_1(An,N)).
c_check_av(8,_OpCode,_An,_N,'put0_1()').

c_check_av(9,OpCode,_An,_N,rel_op('<',OpCode)).
c_check_av(10,OpCode,_An,_N,rel_op('>',OpCode)).
c_check_av(11,OpCode,_An,_N,rel_op('<=',OpCode)).

c_check_av(12,OpCode,_An,_N,rel_op('>=',OpCode)).
c_check_av(13,OpCode,_An,_N,rel_op('==',OpCode)).
c_check_av(14,OpCode,_An,_N,rel_op('!=',OpCode)).

c_check_av(15,_OpCode,An,N,int_only_op('<<',An,N)).
c_check_av(16,_OpCode,An,N,int_only_op('>>',An,N)).
c_check_av(17,_OpCode,An,N,int_only_op('&',An,N)).
c_check_av(18,_OpCode,An,N,int_only_op('|',An,N)).
c_check_av(19,_OpCode,An,N,int_only_op('^',An,N)).

c_check_av(20,_OpCode,An,N,l_neg_3(An,N)).
c_check_av(21,_OpCode,An,N,compare0_3(An,N)).
c_check_av(22,_OpCode,An,N,arg_3(An,N)).
c_check_av(23,_OpCode,_An,_N,'setarg_3()').
c_check_av(24,_OpCode,_An,_N,'change_arg_3()').

c_check_av(25,_OpCode,_An,_N,'def_3()').
c_check_av(26,_OpCode,_An,_N,'rm_2()').
c_check_av(27,_OpCode,_An,_N,'set_3()').
c_check_av(28,_OpCode,An,N,val_3(An,N)).
c_check_av(29,_OpCode,An,N,lval_3(An,N)).
c_check_av(30,_OpCode,An,N,symcat_3(An,N)).

%c_check_av(31,_OpCode,An,N,dcg_connect_1(An,N)).
%c_check_av(32,_OpCode,An,N,list2term_2(An,N)).
%c_check_av(33,_OpCode,An,N,term2list_2(An,N)).
%c_check_av(34,_OpCode,An,N,add_instr_4(An,N)).

antigenic(arith_variable,An,_):-An>30.
antigenic(arith_value,An,_):-An>30.
antigenic('builtin_?',An,_):-An>=0.

c_opcode(Base,No,OpCode):-c_check,OpCode is Base+No.

c_check_iv(0,'fail_0()').
c_check_iv(1,'cwrite_1()').
c_check_iv(2,'cnl_1()').
c_check_iv(3,'var_1()').
c_check_iv(4,'nonvar_1()').
c_check_iv(5,'integer_1()').
c_check_iv(6,'atomic_1()').
c_check_iv(7,'is_compiled_1()').

