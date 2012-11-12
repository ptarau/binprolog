% Shorthands for an old Solaris 2.x system - please adapt

speak(What):-speak(What,speak).

play(File):-play(File,audioplay).

record(File):-record(File,audiorecord).

netsurf:-netsurf(netscape,'').

binpro_site:-netsurf(netscape,'ftp://clement@info.umoncton.ca/BinProlog').

to_binpro(Message):-
  mail(
    'BinProlog',
    'binprolog@info.umoncton.ca',
    'from a BinProlog process',
    Message).

% Some multi-media calls from BinProlog

do(ToDo):-
  'make_cmd'(ToDo,Cmd),
  write('DOING: '),write(Cmd),nl,
  system(Cmd).

speak(What,Speaker):-
  do(['echo ',What," | ",Speaker]).

play(File,Player):-
  do([Player,' ',File]).

record(File,Recorder):-
  do([Recorder,' ',File]).

netsurf(Surfer,StartFile):-
  do([Surfer,' ',StartFile]).

mail_file(To,Subject,File):-
  do(['/usr/ucb/mail -s "',Subject,'" ',To,' < ',File]).

mail(To,Subject,Message):-
  TempFile='message.tmp',
  tell(TempFile),
    write(Message),nl,
  told,
  mail_file(To,Subject,TempFile),
  do(['rm -f ',TempFile]).

mail(Name,Email,Subject,Message):-
   'make_cmd'([Name,
',

',Message],
   DearMessage),
   mail(Email,Subject,DearMessage).


% ---------- BinProlog mailer for Windows ----------------------------------
% this assumes blat.exe free PC mailer is in the path
% get blat.exe from http://gepasi.dbs.aber.ac.uk/softw/Blat.html

send(To,Subject,Message):-
  send(To,Subject,Message,isp,'binnetcorp@binnetcorp.com').
 
send_file(To,Subject,File,Server,From):-
  do(['blat ',File, ' -s "',Subject,'" -t ',To,' -server ',Server,' -f ',From]).

send(To,Subject,Message,Server,From):-
  TempFile='message.txt',
  tell(TempFile),
    write(Message),nl,
  told,
  send_file(To,Subject,TempFile,Server,From),
  do(['del ',TempFile]).

send(Name,Email,Subject,Message,Server,From):-
   'make_cmd'([Name,
',

',Message],
   DearMessage),
   send(Email,Subject,DearMessage,Server,From).

mtest:-send(
   'tarau@cs.sfu.ca',
   'blat mail test',
'
This is a test.
BinProlog sends mail using blat.exe
',
    isp, % server
    'binnetcorp@binnetcorp.com' % from
).

%---------------------------------------------------


% at - batch job scheduler
% ?-at_test.

at_test:-
   at('now + 5 minutes',
      '/bin/audioplay /usr/demo/SOUND/sounds/rooster.au').

at(Time,Action):-
  TempFile='at.tmp',
  tell(TempFile),
    write(Action),nl,
  told,
  do(['at -f ',TempFile,' ',Time])
 ,do(['rm -f ',TempFile])
.

  
% add here your own multi-media friends
