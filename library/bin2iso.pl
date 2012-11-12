fail_if(X):- \+ X.


/*
% iso_bin.pl
% All ISO built-in predicates (according to ISO 13211).
% This file contains some implementations for predicates etc that are missing
% in BinProlog 5.00.
% Sven Hartrumpf 1994 - 1996
/*

Non-standard predicates etc whose names should be freed by removing them
from BinProlog.

abolish/2
assert/1
float/2   (should be named real/2)
flush/0
get/1
get0/1
integer/2
name/2    (should be split into atom_codes/2 and number_codes/2)
pow/3
put/1
read_term/2 (should become a part of the missing predicate read_term/3)

Operator mod should have priority 400 (not 300) and mode yfx (not xfx).
Operator \ should have priority 200 (not 500) and mode fy (not fx).
Operator - should have priority 200 (not 500) and mode fy (not fx).
Operator ** should have mode xfx (not yfx).
Operator @ should have priority 100 (not 50) and mode xfx (not fx).
Operator : should have priority 50 (not 900) and mode xfx (not yfx).
*/


% standard operators

:- op(1200,  fx, (?-)).
:- op( 400, yfx, rem).
:- op( 200, xfx, **).
:- op( 100, xfx, @).
:- op(  50, xfx, :).



%% predicates that are missing in BinProlog and are implemented in this file

% atom_chars/2
% atom_chars(+atom,+list)
% atom_chars(+atom,-list)
% atom_chars(-atom,+list)

/* now part of BinProlog
*/

% atom_codes/2
% atom_codes(+atom,+list)
% atom_codes(+atom,-list)
% atom_codes(-atom,+list)

% atom_concat/3
% atom_concat(?atom,?atom,+atom)
% atom_concat(+atom,+atom,-atom)

atom_concat(A, B, C) :-
  atom(C),
  !,
  name(C, C_string),
  append(A_string, B_string, C_string),
  name(A, A_string),
  name(B, B_string).
/*
atom_concat(A, B, C) :-
  name(A, A_string),
  name(B, B_string),
  append(A_string, B_string, C_string),
  name(C, C_string).
*/
% replaced with much faster bultin - Paul Tarau
atom_concat(A, B, C) :- namecat('',A,B,C).

% atom_length/2
% atom_length(+atom,?integer)

atom_length(A, L) :-
  atom_codes(A, Codes),
  length(Codes, L).


% char_code/2
% char_code(+character,+character_code)
% char_code(+character,-character_code)
% char_code(-character,+character_code)

char_code(Character, Code) :-
  name(Character, [Code]).


% get_char/1
% get_char(?character)

get_char(Character) :-
  get_code(Code),
  atom_codes(Character, [Code]).


% nl/1
% nl(@stream_or_alias)

nl(S) :-
  current_output(Old),
  set_output(S),
  nl,
  current_output(Old).


% number_chars/2
% number_chars(+number, ?list)
% number_chars(-number, +list)
% Note: now part of BinProlog

% number_codes/2
% number_codes(+number,+list)
% number_codes(+number,-list)
% number_codes(-number,+list)
% Note: now part of BinProlog

% put_char/1
% put_char(@character)

put_char(Character) :-
  current_output(S),
  put_char(S, Character).


% read/2
% read(@stream_or_alias,?term)

read(S, Term) :-
  current_input(Old),
  set_input(S),
  read(Term),
  current_input(Old).


% read_term/3
% read_term(@stream_or_alias,?term,+read_options_list)
% Comment: Only one option supported.

read_term(S, Term, []) :-
  !,
  current_input(Old),
  set_input(S),
  read(Term),
  set_input(Old).

read_term(S, Term, [variable_names(Vars)]) :-
  current_input(Old),
  set_input(S),
  read_term(Term, Vars),
  set_input(Old).


% real/1
% real(@term)

real(X) :-
  float(X).


% write/2
% write(@stream_or_alias,@term)

write(Out, T) :-
  current_output(Old),
  set_output(Out),
  write(T),
  set_output(Old).


% writeq/2
% writeq(@stream_or_alias,@term)

writeq(Out, T) :-
  current_output(Old),
  set_output(Out),
  writeq(T),
  set_output(Old).


% write_term/2
% write_term(@term,@write_options_list)
% Comment: No options supported.

write_term(Term, []) :-
  write(Term).


% write_term/3
% write_term(@stream_or_alias,@term,@write_options_list)
% Comment: No options supported.

write_term(Out, Term, []) :-
  current_ouput(Old),
  set_output(Out),
  write(Term),
  set_output(Old).



%% predicates that are missing in BinProlog and are not implemented in this file

% at_end_of_stream/0
% at_end_of_stream

% at_end_of_stream/1
% at_end_of_stream(@stream_or_alias)

% catch/3
% catch(+body_term, ?term, ?term)

% char_conversion/2
% char_conversion(@character,@character)

% close/2
% close(@stream_or_alias, @close_options)

close(Stream,Options):-
  nonvar(Options),member(force(true),Options)->
  quiet(Q),quiet(100),(close(Stream)->true;true),quiet(Q)
; close(Stream).

% current_char_conversion/2
% current_char_conversion(?character,?character)

% current_prolog_flag/2
% current_prolog_flag(?flag,?term)

% get_byte/1
% get_byte(?in_byte)

% get_byte/2
% get_byte(@stream_or_alias, ?in_byte)

% number_chars/2
% number_chars(+number,+list)
% number_chars(+number,-list)
% number_chars(-number,+list)

% open/4
% open(@source_sink,@io_mode,-stream,@io_options)

open(File,Mode,Stream,_options):-open(File,Mode,Stream).

% peek_byte/1
% peek_byte(?in_byte)

% peek_byte/2
% peek_byte(@stream_or_alias, ?in_byte)

% peek_char/1
% peek_char(?character)

% peek_char/2
% peek_char(@stream_or_alias, ?character)

% peek_code/1
% peek_code(?integer)

% peek_code/2
% peek_code(@stream_or_alias, ?integer)

% put_byte/1
% put_byte(+byte)

% put_byte/2
% put_byte(@stream_or_alias, +byte)

% set_prolog_flag/2
% set_prolog_flag(@flag,@term)

% set_stream_position/2
% set_stream_position(@stream_or_alias,@stream_position)

% stream_property/2
% stream_property(?stream,?stream_property)

% sub_atom/5
% sub_atom(+atom,?integer,?integer,?integer,?atom)

% throw/1
% throw(+term)

% unify_with_occurs_check/2
% unify_with_occurs_check(?term,?term)

% write_canonical/1
% write_canonical(@term)

% write_canonical/2
% write_canonical(@stream_or_alias,@term)



%% predicates that lack some important features

% set_input/1
% set_input(@stream_or_alias)

% alias user_input not implemented


% set_output/1
% set_output(@stream_or_alias)

% alias user_output not implemented


%% incorrect predicates

% atomic(2.3) should succeed
% compound(1.1) should fail
% get_char/2 causes errors


%% Arithmetic part

% rem/2

rem(X, Y, R) :-
  R is X - (floor(X / Y) * Y).


%% incorrect arithmetic functors

% float/1: should always return a float

% truncate/1: truncate(-4.2) should be -4 (?)


%% missing arithmetic functors

% float_fractional_part/1

% float_integer_part/1
