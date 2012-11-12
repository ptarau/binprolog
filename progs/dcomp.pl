make_static(F/N):-
    functor(H,F,N),
    is_asserted(H),
    make_undefined(H),
    make_static1(F/N),
    !.
make_static(FN):-
    errmes(unable_to,make_static(FN)).

restore_dynamic(F/N):-
  functor(H,F,N),
  enable(F/N),
  make_undefined(H),
  !.
restore_dynamic(FN):-
  errmes(unable_to,restore_dynamic(FN)).

make_static1(F/N):-
  vget0(code_top,Top),
  vget0(code_oldtop,OldTop),
  vset(code_oldtop,Top),
  make_static0(F/N), 
  'prolog:terminate_file'(mem,1),
  disable(F/N), % if we disable before, it becomes strongly linked!!!
  vset(code_oldtop,OldTop).
  
make_static0(F/N):-
  a_clause(F/N,C),
   'prolog:maincomp'(mem,C),
  fail.
make_static0(_).

a_clause(F/N,(H:-B)):-
   functor(H,F,N),
   clause(H,B).

make_undefined(H):-is_compiled(H),!,override(2,H,fail).
make_undefined(_).
  
disable(FN):-
  current_db(DB),
  db_disable(DB,FN).

enable(FN):-
  current_db(DB),
  db_enable(DB,FN).
  
db_disable(DB,F/N):-
  functor(P,F,N),
  val(DB,P,Adr),
  def(P,DB,Adr),
  rm(DB,P).

db_enable(DB,F/N):-
  functor(P,F,N),
  val(P,DB,Adr),
  def(DB,P,Adr),
  rm(P,DB).


% -----------------
test:-
  consult(nrev30),
  make_static(app/3),
  list_asm(app,3,20),
  make_static(nrev/2),
  list_asm(nrev,2,20),
  go.

g1:-
   vget0(code_top,Top),
   vget0(code_oldtop,OldTop),
   vset(code_oldtop,Top),
   ( translate_clause((a(13):-true),mem)
   ; translate_clause((a(14):-true),mem)
   ; 'prolog:terminate_file'(mem,1)
   ),
   vset(code_oldtop,OldTop),
   list_asm(a,1,10).


g2:-
   vget0(code_top,Top),
   vget0(code_oldtop,OldTop),
   vset(code_oldtop,Top),
   ( translate_clause((b(X):-a(X)),mem)
   ; translate_clause((b(15):-true),mem)
   ; 'prolog:terminate_file'(mem,1)
   ),
   vset(code_oldtop,OldTop),
   list_asm(b,1,10).

bug:-
   assert(buggy(1)),
   vget0(code_top,Top),
   vget0(code_oldtop,OldTop),
   vset(code_oldtop,Top),
   ( translate_clause((buggy(2):-true),mem)
   ; 'prolog:terminate_file'(mem,1)
   ),
   vset(code_oldtop,OldTop),
   list_asm(buggy,1,10).

