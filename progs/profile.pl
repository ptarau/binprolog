% example of BinProlog Internet profile
% usful if BinProlog is not able to
% detect everything about your machine's 
% network environment

% example

master_server('139.103.16.4',7000).
login(paul).
password(evrika).
this_host('139.103.16.4').
host('139.103.16.4').
%proxy_server('139.103.16.2'). % put this only if you really have it!

% To meet other users: open  2 windows running BinProlog
% do in the first one:   ?-listen.
% do in the second one:  ?-chat.
