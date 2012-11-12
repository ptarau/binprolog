
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% code testing stuff in the Mobile threads paper...

go:-write('
type 
    this_port(9300)=>>trust. % passwordless unrestricted server
and 
    go_client
it 2 different windows
').


% assumes a server has been started on the same machine with:
%
% ?-port(9300)=>>trust.
%
% Program to be fetched over the network 
% and run on target server
%
a(1).
a(2).
b(X):-a(X).
work_there:-b(X),write('X'=X),nl,fail.

% main code sending a moving thread to server
go_client:-
  % port where the target server runs
  port(9300)=>>password(none)=>>
    wrap_thread((  % code subject to movment
      write(here),nl, % action on `base'
      move_thread,    % thread movement
      work_there      % action on target
  )), 
  % code to be executed when `back'
  write(back),nl.
% end of program

/* Interaction on TARGET
?-this_port(9300)=>>trust.
.....
X = 1
X = 2

% Interaction on BASE
?-go.
here
running_server(port(9390),password(pw_5473))
consulting(../progs/tmob.pl)
consulted(../progs/tmob.pl)
time(consulting = 30,quick_compiling = 0,static_space = 0)
server_done(port(9390),password(pw_5473))
back
yes
*/


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% test data

go1:-capture_cont_for(dd),write(back),nl.

dd:-aa,b,cc,
 call_with_cont(pp_clause),
 ee,ff.

aa.
b:-pp_clause(b).
cc:-pp_clause(cc).
ee:-pp_clause(unexpected_ee).
ff:-pp_clause(unexpected_ff).

go2:-prolog:consume_cont(pp_clause,End),a,b,c,d,e,f,End.

a_fact(number,99).
a_fact(word,hello).

go3:-
  port(9300)=>
  wrap_thread((
    write(here),nl,
    move_thread,
    default_code(C),default_port(P),write(there_from(C,P)),nl,
    out(working(there)),
    remote_run((write(message_on_base_server),nl)),
    say(seeing_a_shark),
    (a_fact(X,Y),write(X=Y),nl,fail;true)
  )),
 write(back),nl.



test :- N=3,
  ctime(T1),han(N,9100,9200,9300),ctime(T2),T is T2-T1,write(T),nl.

han(N,_,_,_) :- N=<0,!.
han(N,From,Via,To) :- N>0,
        N1 is N - 1,
        han(N1,From,To,Via),
        write(start_moving(From,To)),nl,
        wrap_thread(move_disk(From,To)),
        write(bak_from_moving(From,To)),nl,
        han(N1,Via,From,To).

move_disk(From,To):-
        go_place(From),
        write(get(disk,from(From))),nl,
        go_place(To),
        write(put(disk,to(To))),nl.

go_place(Port):-port(Port)=>>move_thread.

go5:-
   han(3,9100,9200,9300),
   write(back),nl.
