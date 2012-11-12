:-[library(xref)].

get_author(X):-default(author(X),X='Document generated with prodoc.pro').

xref(File):-xref(File,1).

xref(File,WithHead):-
  ctime(T1),
  make_cmd([File,'.tex'],Outfile),
  tell(Outfile),
  get_author(Author),
  make_cmd(['Documentation and references for Prolog file: ',File],Cmd),
  (WithHead=:=1->latex_begin(Cmd,Author);true),
  init_xref(File),
  xref_checks(File),
  latex_call_graph(File),
  (WithHead=:=1->latex_end;true),
  told,
  ctime(T2),T is T2-T1,
  quietmes(time(xref(File)=T)).


sec(Sec,Name,Label):-
  write('\'),write(Sec),write('{'),write(Name),
  write('} \label{'),write(Label),write('}'),nl.

def(FN):-
  label(FN,Label),
  sec(subsubsection,'Predicate:',Label),
  vfixed(FN),nl.

ref(FN):-
  label(FN,Label),
  write('\item \ref{'),write(Label),write('} '),
  vfixed(FN),nl.

label(FN,Label):-ascii_name(FN,Label).
 
%vfixed(Name):-write('\verb|'),write(Name),write('|').
 
vfixed(Name):-
  nl,write('\begin{verbatim}'),nl,write(Name),nl,
  write('\end{verbatim}'),nl.

info2latex(File,FN):-
  bp_info_or_comment(File,FN,Text,Exs)->
  write('{\small \begin{verbatim}'),nl,
  show_info(text,Text),
  show_info(examples,Exs),
  write('\end{verbatim}}'),nl
; true.

bp_info_or_comment(_Db,FN,Text,Exs):-bp_info(FN,Text,Exs).
bp_info_or_comment(Db,FN,Text,''):-
  to_bp_comment(FN,G,_),functor(G,F,N),
  db_filtered_clause(Db,F/N,H,_),
  arg(1,H,Xs),
  atom_codes(Text,Xs).

latex_call_graph(File):-
  sec(subsection,'Cross reference and info on predicates',bppreds),
  db_proc(File,FN),
  ttyprin('.'),
  nl,def(FN),
  info2latex(File,FN),
  show_items(File,FN),
  fail
; nl.

show_items(File,FN):-
  get_callees(File,FN,Callees),
  ( Callees=[]->true
  ;
    write('\paragraph{Calls:} '),nl,
    write('\begin{itemize}'),nl,
    forall(member(C,Callees),ref(C)),
    write('\end{itemize}'),nl
  ),
  get_callers(File,FN,Callers),
  ( Callers=[]->true
  ;
    write('\paragraph{Is called from:} '),nl,
    write('\begin{itemize}'),nl,
    forall(member(C,Callers),ref(C)),
    write('\end{itemize}'),nl
  ).

latex_begin(Title,Author):-
  nl,write('\documentstyle{article}'),nl,
  write('\begin{document}'),nl,nl,
  write('\title{'),write(Title),write('}'),nl,nl,
  write('\author{'),write(Author),write('}'),nl,nl,
  write('\maketitle'),nl,nl,
  sec(section,'Appendix',appendix),nl,nl.


latex_end:-
  nl,write('\end{document}'),nl,nl.
  
check(Goal):-
   arg(2,Goal,FN),
   findall(FN,Goal,Xs),Xs\==[]->
   ( write('\begin{itemize}'),nl,
     member(FN,Xs),
     ref(FN),
     fail
   ; write('\end{itemize}'),nl
   )
   ; nl.

check_undef(DB):-check(undefined(DB,_)).
check_unused(DB):-check(unused(DB,_)).

xref_checks(File):-
  sec(subsection,
     'Undefined predicates, possibly intended to be imported',undef),
  check_undef(File),
  sec(subsection,
     'Unused predicates, possibly intended to be exported',unused),
  check_unused(File).

