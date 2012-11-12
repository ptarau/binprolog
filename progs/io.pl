/* 

% moved to read.pl

popen(Cmd,read,Stream):-!,
  open_stream(1,Cmd,r,var(CStream)),
  Stream='$stream'(CStream,seemark,Cmd).
popen(Cmd,write,Stream):-
  open_stream(1,Cmd,w,var(CStream)),
  Stream='$stream'(CStream,tellmark,Cmd).

pclose(Stream):-close_f_or_p(1,Stream).

open(File,Mode,'$stream'(CStream,IOMark,File)):-
  mode2c(Mode,CMode,IOMark,_,_),
  user_file(File,IOMark,UserFile),
  open_stream(0,UserFile,CMode,var(CStream)).

close(user):-!.
close(F):-atomic(F),get_stream(F,S),!,close_f_or_p(0,S).
close(X):-close_f_or_p(0,X).

get_stream(F,'$stream'(Name,IOMark,F)):-
  ( IOMark=seemark
  ; IOMark=tellmark
  ),
  tval(IOMark,F,S),
  (S=var(Name);S=Name/0),
  !.

close_f_or_p(_,'$stream'(_,_,user)):-!.
close_f_or_p(No,Stream):-
  Stream='$stream'(CStream,IOMark,CFile),
  current_stream(IOMark,Stream0),
  ( Stream==Stream0->
    set_default_stream(IOMark)
  ; true
  ),
  tlet(IOMark,CFile,closed_file/0),
  close_stream(No,var(CStream)),
 !.
close_f_or_p(No,Stream):-
  errmes(unable_to_close(No),Stream).

set_default_stream(seemark):-!,
  open(user,read,Stream),set_input(Stream).
set_default_stream(tellmark):-
  open(user,write,Stream),set_output(Stream).

set_stream(IOMark,Stream):-
  Stream='$stream'(CStream,IOMark,CFile),
  mode2c(_,_,IOMark,IOFile,IOFunc),!,
  vset(IOFile,var(CStream)), % follows see_tell convention
  vset(IOFunc,CFile/0),
  ( CFile==user->true
  ; tlet(IOMark,CFile,var(CStream))
  ).

current_stream(IOMark,Stream):-
  mode2c(_,_,IOMark,IOFile,IOFunc),
  vget0(IOFile,var(CStream)),
  vget0(IOFunc,CFile/0),!,
  Stream='$stream'(CStream,IOMark,CFile).


set_input(Stream):-set_stream(seemark,Stream).
set_output(Stream):-set_stream(tellmark,Stream).

current_input(Stream):-current_stream(seemark,Stream).
current_output(Stream):-current_stream(tellmark,Stream).

user_file(user,seemark,F):-!,F=user_input.
user_file(user,tellmark,F):-!,F=user_output.
user_file(F,_,F).

mode2c(read,rb,seemark,seefile,seefunc).
mode2c(write,wb,tellmark,tellfile,tellfunc).
mode2c(append,ab,tellmark,tellfile,tellfunc).

*/

init_mes:-retractall(a_mes(_)).

mes(X):-assert(a_mes(X)).

go:-
    init_mes,
    open(zut,append,S),
    set_output(S),nl,write(hello),nl,write(bye),nl,
    current_output(X),mes(stream_before_close(S=X)),
    telling(FX),mes(telling_before_close=FX),
    telling(A),tell(A),
    close(S),
%    close(zut),
    telling(FY),mes(telling_after_close=FY),
    current_output(Y),mes(stream_after_closing=Y),
    listing.


exec2list(Cmd,Xs):-
   popen(Cmd,read,Stream),
   set_input(Stream),
   findall(X,
     file2char(X),
   Xs),
   pclose(Stream).

file2char(X):-get0(X),(X == -1,!,fail;true).
file2char(X):-file2char(X).

write_list(Xs):-member(X,Xs),put(X),fail.
write_list(_).

g:-exec2list(ls,Xs),write_list(Xs),nl.
