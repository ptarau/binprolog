% File   : pereira.pl
% Author : Fernando Pereira / modified by Jacques Noye
%	functor and call tests added by Micha Meier
% Date   : 26 August 88
% Pereira benchmark suite
note:-write('use bp -h40000 -t20000 -s4000 -b0 -d19 for BinProlog'),nl.
:- dynamic ua/3.
:- dynamic dix/2.

% Trivial predicates for use in controls.

dummy.

dummy(_).

dummy(_, _).

dummy(_, _, _).

% The actual benchmarks

% 1. 100 determinate tail calls

p100(a).
p99(a) :- p100(a).
p98(a) :- p99(a).
p97(a) :- p98(a).
p96(a) :- p97(a).
p95(a) :- p96(a).
p94(a) :- p95(a).
p93(a) :- p94(a).
p92(a) :- p93(a).
p91(a) :- p92(a).
p90(a) :- p91(a).
p89(a) :- p90(a).
p88(a) :- p89(a).
p87(a) :- p88(a).
p86(a) :- p87(a).
p85(a) :- p86(a).
p84(a) :- p85(a).
p83(a) :- p84(a).
p82(a) :- p83(a).
p81(a) :- p82(a).
p80(a) :- p81(a).
p79(a) :- p80(a).
p78(a) :- p79(a).
p77(a) :- p78(a).
p76(a) :- p77(a).
p75(a) :- p76(a).
p74(a) :- p75(a).
p73(a) :- p74(a).
p72(a) :- p73(a).
p71(a) :- p72(a).
p70(a) :- p71(a).
p69(a) :- p70(a).
p68(a) :- p69(a).
p67(a) :- p68(a).
p66(a) :- p67(a).
p65(a) :- p66(a).
p64(a) :- p65(a).
p63(a) :- p64(a).
p62(a) :- p63(a).
p61(a) :- p62(a).
p60(a) :- p61(a).
p59(a) :- p60(a).
p58(a) :- p59(a).
p57(a) :- p58(a).
p56(a) :- p57(a).
p55(a) :- p56(a).
p54(a) :- p55(a).
p53(a) :- p54(a).
p52(a) :- p53(a).
p51(a) :- p52(a).
p50(a) :- p51(a).
p49(a) :- p50(a).
p48(a) :- p49(a).
p47(a) :- p48(a).
p46(a) :- p47(a).
p45(a) :- p46(a).
p44(a) :- p45(a).
p43(a) :- p44(a).
p42(a) :- p43(a).
p41(a) :- p42(a).
p40(a) :- p41(a).
p39(a) :- p40(a).
p38(a) :- p39(a).
p37(a) :- p38(a).
p36(a) :- p37(a).
p35(a) :- p36(a).
p34(a) :- p35(a).
p33(a) :- p34(a).
p32(a) :- p33(a).
p31(a) :- p32(a).
p30(a) :- p31(a).
p29(a) :- p30(a).
p28(a) :- p29(a).
p27(a) :- p28(a).
p26(a) :- p27(a).
p25(a) :- p26(a).
p24(a) :- p25(a).
p23(a) :- p24(a).
p22(a) :- p23(a).
p21(a) :- p22(a).
p20(a) :- p21(a).
p19(a) :- p20(a).
p18(a) :- p19(a).
p17(a) :- p18(a).
p16(a) :- p17(a).
p15(a) :- p16(a).
p14(a) :- p15(a).
p13(a) :- p14(a).
p12(a) :- p13(a).
p11(a) :- p12(a).
p10(a) :- p11(a).
p9(a) :- p10(a).
p8(a) :- p9(a).
p7(a) :- p8(a).
p6(a) :- p7(a).
p5(a) :- p6(a).
p4(a) :- p5(a).
p3(a) :- p4(a).
p2(a) :- p3(a).
p1(a) :- p2(a).

% 2. 63 determinate nontail calls, 64 determinate tail calls.

q127(a).
q126(a).
q125(a).
q124(a).
q123(a).
q122(a).
q121(a).
q120(a).
q119(a).
q118(a).
q117(a).
q116(a).
q115(a).
q114(a).
q113(a).
q112(a).
q111(a).
q110(a).
q109(a).
q108(a).
q107(a).
q106(a).
q105(a).
q104(a).
q103(a).
q102(a).
q101(a).
q100(a).
q99(a).
q98(a).
q97(a).
q96(a).
q95(a).
q94(a).
q93(a).
q92(a).
q91(a).
q90(a).
q89(a).
q88(a).
q87(a).
q86(a).
q85(a).
q84(a).
q83(a).
q82(a).
q81(a).
q80(a).
q79(a).
q78(a).
q77(a).
q76(a).
q75(a).
q74(a).
q73(a).
q72(a).
q71(a).
q70(a).
q69(a).
q68(a).
q67(a).
q66(a).
q65(a).
q64(a).
q63(a) :- q126(a), q127(a).
q62(a) :- q124(a), q125(a).
q61(a) :- q122(a), q123(a).
q60(a) :- q120(a), q121(a).
q59(a) :- q118(a), q119(a).
q58(a) :- q116(a), q117(a).
q57(a) :- q114(a), q115(a).
q56(a) :- q112(a), q113(a).
q55(a) :- q110(a), q111(a).
q54(a) :- q108(a), q109(a).
q53(a) :- q106(a), q107(a).
q52(a) :- q104(a), q105(a).
q51(a) :- q102(a), q103(a).
q50(a) :- q100(a), q101(a).
q49(a) :- q98(a), q99(a).
q48(a) :- q96(a), q97(a).
q47(a) :- q94(a), q95(a).
q46(a) :- q92(a), q93(a).
q45(a) :- q90(a), q91(a).
q44(a) :- q88(a), q89(a).
q43(a) :- q86(a), q87(a).
q42(a) :- q84(a), q85(a).
q41(a) :- q82(a), q83(a).
q40(a) :- q80(a), q81(a).
q39(a) :- q78(a), q79(a).
q38(a) :- q76(a), q77(a).
q37(a) :- q74(a), q75(a).
q36(a) :- q72(a), q73(a).
q35(a) :- q70(a), q71(a).
q34(a) :- q68(a), q69(a).
q33(a) :- q66(a), q67(a).
q32(a) :- q64(a), q65(a).
q31(a) :- q62(a), q63(a).
q30(a) :- q60(a), q61(a).
q29(a) :- q58(a), q59(a).
q28(a) :- q56(a), q57(a).
q27(a) :- q54(a), q55(a).
q26(a) :- q52(a), q53(a).
q25(a) :- q50(a), q51(a).
q24(a) :- q48(a), q49(a).
q23(a) :- q46(a), q47(a).
q22(a) :- q44(a), q45(a).
q21(a) :- q42(a), q43(a).
q20(a) :- q40(a), q41(a).
q19(a) :- q38(a), q39(a).
q18(a) :- q36(a), q37(a).
q17(a) :- q34(a), q35(a).
q16(a) :- q32(a), q33(a).
q15(a) :- q30(a), q31(a).
q14(a) :- q28(a), q29(a).
q13(a) :- q26(a), q27(a).
q12(a) :- q24(a), q25(a).
q11(a) :- q22(a), q23(a).
q10(a) :- q20(a), q21(a).
q9(a) :- q18(a), q19(a).
q8(a) :- q16(a), q17(a).
q7(a) :- q14(a), q15(a).
q6(a) :- q12(a), q13(a).
q5(a) :- q10(a), q11(a).
q4(a) :- q8(a), q9(a).
q2(a) :- q4(a), q5(a).
q3(a) :- q6(a), q7(a).
q1(a) :- q2(a), q3(a).

% 3. Construct one 100 element list, nonrecursively.

% 4. Walk down a 100 element list, nonrecursively

% 5. Walk down a 100 element list, recursively

% 6. Walk down N 100 copies of the same 100 element list, recursively.

wlr([]).
wlr([_|L]) :- wlr(L).

wlr([], []).
wlr([_|L1], [_|L2]) :- wlr(L1, L2).

wlr([], [], [], []).
wlr([_|L1], [_|L2], [_|L3], [_|L4]) :- wlr(L1, L2, L3, L4).

wlr([], [], [], [], [], [], [], []).
wlr([_|L1], [_|L2], [_|L3], [_|L4], [_|L5], [_|L6], [_|L7], [_|L8]) :-
   wlr(L1, L2, L3, L4, L5, L6, L7, L8).

wlr([], [], [], [], [], [], [], [], [], [], [], [], [], [], [], []).
wlr([_|L1], [_|L2], [_|L3], [_|L4], [_|L5], [_|L6], [_|L7], [_|L8],
    [_|L9], [_|L10], [_|L11], [_|L12], [_|L13], [_|L14], [_|L15], [_|L16]) :-
   wlr(L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, L11, L12, L13, L14, L15, L16).

args(1).  args(2).  args(4).  args(8).  args(16).

args(1, L) :- wlr(L).
args(2, L) :- wlr(L, L).
args(4, L) :- wlr(L, L, L, L).
args(8, L) :- wlr(L, L, L, L, L, L, L, L).
args(16, L) :- wlr(L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L).

% Nonrecursive list cruncher

r101([]).
r100([100|R]) :- r101(R).
r99([99|R]) :- r100(R).
r98([98|R]) :- r99(R).
r97([97|R]) :- r98(R).
r96([96|R]) :- r97(R).
r95([95|R]) :- r96(R).
r94([94|R]) :- r95(R).
r93([93|R]) :- r94(R).
r92([92|R]) :- r93(R).
r91([91|R]) :- r92(R).
r90([90|R]) :- r91(R).
r89([89|R]) :- r90(R).
r88([88|R]) :- r89(R).
r87([87|R]) :- r88(R).
r86([86|R]) :- r87(R).
r85([85|R]) :- r86(R).
r84([84|R]) :- r85(R).
r83([83|R]) :- r84(R).
r82([82|R]) :- r83(R).
r81([81|R]) :- r82(R).
r80([80|R]) :- r81(R).
r79([79|R]) :- r80(R).
r78([78|R]) :- r79(R).
r77([77|R]) :- r78(R).
r76([76|R]) :- r77(R).
r75([75|R]) :- r76(R).
r74([74|R]) :- r75(R).
r73([73|R]) :- r74(R).
r72([72|R]) :- r73(R).
r71([71|R]) :- r72(R).
r70([70|R]) :- r71(R).
r69([69|R]) :- r70(R).
r68([68|R]) :- r69(R).
r67([67|R]) :- r68(R).
r66([66|R]) :- r67(R).
r65([65|R]) :- r66(R).
r64([64|R]) :- r65(R).
r63([63|R]) :- r64(R).
r62([62|R]) :- r63(R).
r61([61|R]) :- r62(R).
r60([60|R]) :- r61(R).
r59([59|R]) :- r60(R).
r58([58|R]) :- r59(R).
r57([57|R]) :- r58(R).
r56([56|R]) :- r57(R).
r55([55|R]) :- r56(R).
r54([54|R]) :- r55(R).
r53([53|R]) :- r54(R).
r52([52|R]) :- r53(R).
r51([51|R]) :- r52(R).
r50([50|R]) :- r51(R).
r49([49|R]) :- r50(R).
r48([48|R]) :- r49(R).
r47([47|R]) :- r48(R).
r46([46|R]) :- r47(R).
r45([45|R]) :- r46(R).
r44([44|R]) :- r45(R).
r43([43|R]) :- r44(R).
r42([42|R]) :- r43(R).
r41([41|R]) :- r42(R).
r40([40|R]) :- r41(R).
r39([39|R]) :- r40(R).
r38([38|R]) :- r39(R).
r37([37|R]) :- r38(R).
r36([36|R]) :- r37(R).
r35([35|R]) :- r36(R).
r34([34|R]) :- r35(R).
r33([33|R]) :- r34(R).
r32([32|R]) :- r33(R).
r31([31|R]) :- r32(R).
r30([30|R]) :- r31(R).
r29([29|R]) :- r30(R).
r28([28|R]) :- r29(R).
r27([27|R]) :- r28(R).
r26([26|R]) :- r27(R).
r25([25|R]) :- r26(R).
r24([24|R]) :- r25(R).
r23([23|R]) :- r24(R).
r22([22|R]) :- r23(R).
r21([21|R]) :- r22(R).
r20([20|R]) :- r21(R).
r19([19|R]) :- r20(R).
r18([18|R]) :- r19(R).
r17([17|R]) :- r18(R).
r16([16|R]) :- r17(R).
r15([15|R]) :- r16(R).
r14([14|R]) :- r15(R).
r13([13|R]) :- r14(R).
r12([12|R]) :- r13(R).
r11([11|R]) :- r12(R).
r10([10|R]) :- r11(R).
r9([9|R]) :- r10(R).
r8([8|R]) :- r9(R).
r7([7|R]) :- r8(R).
r6([6|R]) :- r7(R).
r5([5|R]) :- r6(R).
r4([4|R]) :- r5(R).
r3([3|R]) :- r4(R).
r2([2|R]) :- r3(R).
r1([1|R]) :- r2(R).

% 7. Construct a term with 100 nodes, nonrecursively

% 8. Walk down a term with 100 nodes, nonrecursively.

% 9. Walk down a term with 100 nodes, recursively.

wtr(nil).
wtr(f(_,R)) :- wtr(R).

% Nonrecursive term cruncher

s101(nil).
s100(f(100, R)) :- s101(R).
s99(f(99, R)) :- s100(R).
s98(f(98, R)) :- s99(R).
s97(f(97, R)) :- s98(R).
s96(f(96, R)) :- s97(R).
s95(f(95, R)) :- s96(R).
s94(f(94, R)) :- s95(R).
s93(f(93, R)) :- s94(R).
s92(f(92, R)) :- s93(R).
s91(f(91, R)) :- s92(R).
s90(f(90, R)) :- s91(R).
s89(f(89, R)) :- s90(R).
s88(f(88, R)) :- s89(R).
s87(f(87, R)) :- s88(R).
s86(f(86, R)) :- s87(R).
s85(f(85, R)) :- s86(R).
s84(f(84, R)) :- s85(R).
s83(f(83, R)) :- s84(R).
s82(f(82, R)) :- s83(R).
s81(f(81, R)) :- s82(R).
s80(f(80, R)) :- s81(R).
s79(f(79, R)) :- s80(R).
s78(f(78, R)) :- s79(R).
s77(f(77, R)) :- s78(R).
s76(f(76, R)) :- s77(R).
s75(f(75, R)) :- s76(R).
s74(f(74, R)) :- s75(R).
s73(f(73, R)) :- s74(R).
s72(f(72, R)) :- s73(R).
s71(f(71, R)) :- s72(R).
s70(f(70, R)) :- s71(R).
s69(f(69, R)) :- s70(R).
s68(f(68, R)) :- s69(R).
s67(f(67, R)) :- s68(R).
s66(f(66, R)) :- s67(R).
s65(f(65, R)) :- s66(R).
s64(f(64, R)) :- s65(R).
s63(f(63, R)) :- s64(R).
s62(f(62, R)) :- s63(R).
s61(f(61, R)) :- s62(R).
s60(f(60, R)) :- s61(R).
s59(f(59, R)) :- s60(R).
s58(f(58, R)) :- s59(R).
s57(f(57, R)) :- s58(R).
s56(f(56, R)) :- s57(R).
s55(f(55, R)) :- s56(R).
s54(f(54, R)) :- s55(R).
s53(f(53, R)) :- s54(R).
s52(f(52, R)) :- s53(R).
s51(f(51, R)) :- s52(R).
s50(f(50, R)) :- s51(R).
s49(f(49, R)) :- s50(R).
s48(f(48, R)) :- s49(R).
s47(f(47, R)) :- s48(R).
s46(f(46, R)) :- s47(R).
s45(f(45, R)) :- s46(R).
s44(f(44, R)) :- s45(R).
s43(f(43, R)) :- s44(R).
s42(f(42, R)) :- s43(R).
s41(f(41, R)) :- s42(R).
s40(f(40, R)) :- s41(R).
s39(f(39, R)) :- s40(R).
s38(f(38, R)) :- s39(R).
s37(f(37, R)) :- s38(R).
s36(f(36, R)) :- s37(R).
s35(f(35, R)) :- s36(R).
s34(f(34, R)) :- s35(R).
s33(f(33, R)) :- s34(R).
s32(f(32, R)) :- s33(R).
s31(f(31, R)) :- s32(R).
s30(f(30, R)) :- s31(R).
s29(f(29, R)) :- s30(R).
s28(f(28, R)) :- s29(R).
s27(f(27, R)) :- s28(R).
s26(f(26, R)) :- s27(R).
s25(f(25, R)) :- s26(R).
s24(f(24, R)) :- s25(R).
s23(f(23, R)) :- s24(R).
s22(f(22, R)) :- s23(R).
s21(f(21, R)) :- s22(R).
s20(f(20, R)) :- s21(R).
s19(f(19, R)) :- s20(R).
s18(f(18, R)) :- s19(R).
s17(f(17, R)) :- s18(R).
s16(f(16, R)) :- s17(R).
s15(f(15, R)) :- s16(R).
s14(f(14, R)) :- s15(R).
s13(f(13, R)) :- s14(R).
s12(f(12, R)) :- s13(R).
s11(f(11, R)) :- s12(R).
s10(f(10, R)) :- s11(R).
s9(f(9, R)) :- s10(R).
s8(f(8, R)) :- s9(R).
s7(f(7, R)) :- s8(R).
s6(f(6, R)) :- s7(R).
s5(f(5, R)) :- s6(R).
s4(f(4, R)) :- s5(R).
s3(f(3, R)) :- s4(R).
s2(f(2, R)) :- s3(R).
s1(f(1, R)) :- s2(R).

% 10. 99 shallow failures; no assumption on indexing

b1(_X, _Y) :- fail.
b1(_X, _Y) :- fail.
b1(_X, _Y) :- fail.
b1(_X, _Y) :- fail.
b1(_X, _Y) :- fail.
b1(_X, _Y) :- fail.
b1(_X, _Y) :- fail.
b1(_X, _Y) :- fail.
b1(_X, _Y) :- fail.
b1(_X, _Y) :- fail.
b1(_X, _Y) :- fail.
b1(_X, _Y) :- fail.
b1(_X, _Y) :- fail.
b1(_X, _Y) :- fail.
b1(_X, _Y) :- fail.
b1(_X, _Y) :- fail.
b1(_X, _Y) :- fail.
b1(_X, _Y) :- fail.
b1(_X, _Y) :- fail.
b1(_X, _Y) :- fail.
b1(_X, _Y) :- fail.
b1(_X, _Y) :- fail.
b1(_X, _Y) :- fail.
b1(_X, _Y) :- fail.
b1(_X, _Y) :- fail.
b1(_X, _Y) :- fail.
b1(_X, _Y) :- fail.
b1(_X, _Y) :- fail.
b1(_X, _Y) :- fail.
b1(_X, _Y) :- fail.
b1(_X, _Y) :- fail.
b1(_X, _Y) :- fail.
b1(_X, _Y) :- fail.
b1(_X, _Y) :- fail.
b1(_X, _Y) :- fail.
b1(_X, _Y) :- fail.
b1(_X, _Y) :- fail.
b1(_X, _Y) :- fail.
b1(_X, _Y) :- fail.
b1(_X, _Y) :- fail.
b1(_X, _Y) :- fail.
b1(_X, _Y) :- fail.
b1(_X, _Y) :- fail.
b1(_X, _Y) :- fail.
b1(_X, _Y) :- fail.
b1(_X, _Y) :- fail.
b1(_X, _Y) :- fail.
b1(_X, _Y) :- fail.
b1(_X, _Y) :- fail.
b1(_X, _Y) :- fail.
b1(_X, _Y) :- fail.
b1(_X, _Y) :- fail.
b1(_X, _Y) :- fail.
b1(_X, _Y) :- fail.
b1(_X, _Y) :- fail.
b1(_X, _Y) :- fail.
b1(_X, _Y) :- fail.
b1(_X, _Y) :- fail.
b1(_X, _Y) :- fail.
b1(_X, _Y) :- fail.
b1(_X, _Y) :- fail.
b1(_X, _Y) :- fail.
b1(_X, _Y) :- fail.
b1(_X, _Y) :- fail.
b1(_X, _Y) :- fail.
b1(_X, _Y) :- fail.
b1(_X, _Y) :- fail.
b1(_X, _Y) :- fail.
b1(_X, _Y) :- fail.
b1(_X, _Y) :- fail.
b1(_X, _Y) :- fail.
b1(_X, _Y) :- fail.
b1(_X, _Y) :- fail.
b1(_X, _Y) :- fail.
b1(_X, _Y) :- fail.
b1(_X, _Y) :- fail.
b1(_X, _Y) :- fail.
b1(_X, _Y) :- fail.
b1(_X, _Y) :- fail.
b1(_X, _Y) :- fail.
b1(_X, _Y) :- fail.
b1(_X, _Y) :- fail.
b1(_X, _Y) :- fail.
b1(_X, _Y) :- fail.
b1(_X, _Y) :- fail.
b1(_X, _Y) :- fail.
b1(_X, _Y) :- fail.
b1(_X, _Y) :- fail.
b1(_X, _Y) :- fail.
b1(_X, _Y) :- fail.
b1(_X, _Y) :- fail.
b1(_X, _Y) :- fail.
b1(_X, _Y) :- fail.
b1(_X, _Y) :- fail.
b1(_X, _Y) :- fail.
b1(_X, _Y) :- fail.
b1(_X, _Y) :- fail.
b1(_X, _Y) :- fail.
b1(_X, _Y) :- fail.
b1(_X, 100).

shallow :- b1(_X, 100).	% initially b(_X, 100) and assumption of no indexing
			% on second argument

% 11. 99 deep failures; assumes no indexing on 2nd argument

b(_X, 1).
b(_X, 2).
b(_X, 3).
b(_X, 4).
b(_X, 5).
b(_X, 6).
b(_X, 7).
b(_X, 8).
b(_X, 9).
b(_X, 10).
b(_X, 11).
b(_X, 12).
b(_X, 13).
b(_X, 14).
b(_X, 15).
b(_X, 16).
b(_X, 17).
b(_X, 18).
b(_X, 19).
b(_X, 20).
b(_X, 21).
b(_X, 22).
b(_X, 23).
b(_X, 24).
b(_X, 25).
b(_X, 26).
b(_X, 27).
b(_X, 28).
b(_X, 29).
b(_X, 30).
b(_X, 31).
b(_X, 32).
b(_X, 33).
b(_X, 34).
b(_X, 35).
b(_X, 36).
b(_X, 37).
b(_X, 38).
b(_X, 39).
b(_X, 40).
b(_X, 41).
b(_X, 42).
b(_X, 43).
b(_X, 44).
b(_X, 45).
b(_X, 46).
b(_X, 47).
b(_X, 48).
b(_X, 49).
b(_X, 50).
b(_X, 51).
b(_X, 52).
b(_X, 53).
b(_X, 54).
b(_X, 55).
b(_X, 56).
b(_X, 57).
b(_X, 58).
b(_X, 59).
b(_X, 60).
b(_X, 61).
b(_X, 62).
b(_X, 63).
b(_X, 64).
b(_X, 65).
b(_X, 66).
b(_X, 67).
b(_X, 68).
b(_X, 69).
b(_X, 70).
b(_X, 71).
b(_X, 72).
b(_X, 73).
b(_X, 74).
b(_X, 75).
b(_X, 76).
b(_X, 77).
b(_X, 78).
b(_X, 79).
b(_X, 80).
b(_X, 81).
b(_X, 82).
b(_X, 83).
b(_X, 84).
b(_X, 85).
b(_X, 86).
b(_X, 87).
b(_X, 88).
b(_X, 89).
b(_X, 90).
b(_X, 91).
b(_X, 92).
b(_X, 93).
b(_X, 94).
b(_X, 95).
b(_X, 96).
b(_X, 97).
b(_X, 98).
b(_X, 99).
b(_X, 100).

deep :- b(_X, Y), Y = 100.

% 12. Push 100 choice points
% Assumes no super-clever (multipredicate) optimizer

c100(a).
c100(a).
c99(a).
c99(a) :- c100(a).
c98(a).
c98(a) :- c99(a).
c97(a).
c97(a) :- c98(a).
c96(a).
c96(a) :- c97(a).
c95(a).
c95(a) :- c96(a).
c94(a).
c94(a) :- c95(a).
c93(a).
c93(a) :- c94(a).
c92(a).
c92(a) :- c93(a).
c91(a).
c91(a) :- c92(a).
c90(a).
c90(a) :- c91(a).
c89(a).
c89(a) :- c90(a).
c88(a).
c88(a) :- c89(a).
c87(a).
c87(a) :- c88(a).
c86(a).
c86(a) :- c87(a).
c85(a).
c85(a) :- c86(a).
c84(a).
c84(a) :- c85(a).
c83(a).
c83(a) :- c84(a).
c82(a).
c82(a) :- c83(a).
c81(a).
c81(a) :- c82(a).
c80(a).
c80(a) :- c81(a).
c79(a).
c79(a) :- c80(a).
c78(a).
c78(a) :- c79(a).
c77(a).
c77(a) :- c78(a).
c76(a).
c76(a) :- c77(a).

c75(a).
c75(a) :- c76(a).
c74(a).
c74(a) :- c75(a).
c73(a).
c73(a) :- c74(a).
c72(a).
c72(a) :- c73(a).
c71(a).
c71(a) :- c72(a).
c70(a).
c70(a) :- c71(a).
c69(a).
c69(a) :- c70(a).
c68(a).
c68(a) :- c69(a).
c67(a).
c67(a) :- c68(a).
c66(a).
c66(a) :- c67(a).
c65(a).
c65(a) :- c66(a).
c64(a).
c64(a) :- c65(a).
c63(a).
c63(a) :- c64(a).
c62(a).
c62(a) :- c63(a).
c61(a).
c61(a) :- c62(a).
c60(a).
c60(a) :- c61(a).
c59(a).
c59(a) :- c60(a).
c58(a).
c58(a) :- c59(a).
c57(a).
c57(a) :- c58(a).
c56(a).
c56(a) :- c57(a).
c55(a).
c55(a) :- c56(a).
c54(a).
c54(a) :- c55(a).
c53(a).
c53(a) :- c54(a).
c52(a).
c52(a) :- c53(a).
c51(a).
c51(a) :- c52(a).
c50(a).
c50(a) :- c51(a).
c49(a).
c49(a) :- c50(a).
c48(a).
c48(a) :- c49(a).
c47(a).
c47(a) :- c48(a).
c46(a).
c46(a) :- c47(a).
c45(a).
c45(a) :- c46(a).
c44(a).
c44(a) :- c45(a).
c43(a).
c43(a) :- c44(a).
c42(a).
c42(a) :- c43(a).
c41(a).
c41(a) :- c42(a).
c40(a).
c40(a) :- c41(a).


c39(a).
c39(a) :- c40(a).
c38(a).
c38(a) :- c39(a).
c37(a).
c37(a) :- c38(a).
c36(a).
c36(a) :- c37(a).
c35(a).
c35(a) :- c36(a).
c34(a).
c34(a) :- c35(a).
c33(a).
c33(a) :- c34(a).
c32(a).
c32(a) :- c33(a).
c31(a).
c31(a) :- c32(a).
c30(a).
c30(a) :- c31(a).
c29(a).
c29(a) :- c30(a).
c28(a).
c28(a) :- c29(a).
c27(a).
c27(a) :- c28(a).
c26(a).
c26(a) :- c27(a).
c25(a).
c25(a) :- c26(a).
c24(a).
c24(a) :- c25(a).
c23(a).
c23(a) :- c24(a).
c22(a).
c22(a) :- c23(a).
c21(a).
c21(a) :- c22(a).
c20(a).
c20(a) :- c21(a).
c19(a).
c19(a) :- c20(a).
c18(a).
c18(a) :- c19(a).
c17(a).
c17(a) :- c18(a).
c16(a).
c16(a) :- c17(a).
c15(a).
c15(a) :- c16(a).
c14(a).
c14(a) :- c15(a).
c13(a).
c13(a) :- c14(a).
c12(a).
c12(a) :- c13(a).
c11(a).
c11(a) :- c12(a).
c10(a).
c10(a) :- c11(a).
c9(a).
c9(a) :- c10(a).
c8(a).
c8(a) :- c9(a).
c7(a).
c7(a) :- c8(a).
c6(a).
c6(a) :- c7(a).
c5(a).
c5(a) :- c6(a).
c4(a).
c4(a) :- c5(a).
c3(a).
c3(a) :- c4(a).
c2(a).
c2(a) :- c3(a).
c1(a).
c1(a) :- c2(a).

choice :- c1(a), !.

% 13. Create 100 choice points and trail 100 variables

trail :- t1(_X), !.

t100(b).
t100(a).
t99(b).
t99(a) :- t100(_X).
t98(b).
t98(a) :- t99(_X).
t97(b).
t97(a) :- t98(_X).
t96(b).
t96(a) :- t97(_X).
t95(b).
t95(a) :- t96(_X).
t94(b).
t94(a) :- t95(_X).
t93(b).
t93(a) :- t94(_X).
t92(b).
t92(a) :- t93(_X).
t91(b).
t91(a) :- t92(_X).
t90(b).
t90(a) :- t91(_X).
t89(b).
t89(a) :- t90(_X).
t88(b).
t88(a) :- t89(_X).
t87(b).
t87(a) :- t88(_X).
t86(b).
t86(a) :- t87(_X).
t85(b).
t85(a) :- t86(_X).
t84(b).
t84(a) :- t85(_X).
t83(b).
t83(a) :- t84(_X).
t82(b).
t82(a) :- t83(_X).
t81(b).
t81(a) :- t82(_X).
t80(b).
t80(a) :- t81(_X).
t79(b).
t79(a) :- t80(_X).
t78(b).
t78(a) :- t79(_X).
t77(b).
t77(a) :- t78(_X).
t76(b).
t76(a) :- t77(_X).
t75(b).
t75(a) :- t76(_X).
t74(b).
t74(a) :- t75(_X).
t73(b).
t73(a) :- t74(_X).
t72(b).
t72(a) :- t73(_X).
t71(b).
t71(a) :- t72(_X).
t70(b).
t70(a) :- t71(_X).
t69(b).
t69(a) :- t70(_X).
t68(b).
t68(a) :- t69(_X).
t67(b).
t67(a) :- t68(_X).
t66(b).
t66(a) :- t67(_X).
t65(b).
t65(a) :- t66(_X).
t64(b).
t64(a) :- t65(_X).
t63(b).
t63(a) :- t64(_X).
t62(b).
t62(a) :- t63(_X).
t61(b).
t61(a) :- t62(_X).
t60(b).
t60(a) :- t61(_X).
t59(b).
t59(a) :- t60(_X).
t58(b).
t58(a) :- t59(_X).
t57(b).
t57(a) :- t58(_X).
t56(b).
t56(a) :- t57(_X).
t55(b).
t55(a) :- t56(_X).
t54(b).
t54(a) :- t55(_X).
t53(b).
t53(a) :- t54(_X).
t52(b).
t52(a) :- t53(_X).
t51(b).
t51(a) :- t52(_X).
t50(b).
t50(a) :- t51(_X).
t49(b).
t49(a) :- t50(_X).
t48(b).
t48(a) :- t49(_X).
t47(b).
t47(a) :- t48(_X).
t46(b).
t46(a) :- t47(_X).
t45(b).
t45(a) :- t46(_X).
t44(b).
t44(a) :- t45(_X).
t43(b).
t43(a) :- t44(_X).
t42(b).
t42(a) :- t43(_X).
t41(b).
t41(a) :- t42(_X).
t40(b).
t40(a) :- t41(_X).
t39(b).
t39(a) :- t40(_X).
t38(b).
t38(a) :- t39(_X).
t37(b).
t37(a) :- t38(_X).
t36(b).
t36(a) :- t37(_X).
t35(b).
t35(a) :- t36(_X).
t34(b).
t34(a) :- t35(_X).
t33(b).
t33(a) :- t34(_X).
t32(b).
t32(a) :- t33(_X).
t31(b).
t31(a) :- t32(_X).
t30(b).
t30(a) :- t31(_X).
t29(b).
t29(a) :- t30(_X).
t28(b).
t28(a) :- t29(_X).
t27(b).
t27(a) :- t28(_X).
t26(b).
t26(a) :- t27(_X).
t25(b).
t25(a) :- t26(_X).
t24(b).
t24(a) :- t25(_X).
t23(b).
t23(a) :- t24(_X).
t22(b).
t22(a) :- t23(_X).
t21(b).
t21(a) :- t22(_X).
t20(b).
t20(a) :- t21(_X).
t19(b).
t19(a) :- t20(_X).
t18(b).
t18(a) :- t19(_X).
t17(b).
t17(a) :- t18(_X).
t16(b).
t16(a) :- t17(_X).
t15(b).
t15(a) :- t16(_X).
t14(b).
t14(a) :- t15(_X).
t13(b).
t13(a) :- t14(_X).
t12(b).
t12(a) :- t13(_X).
t11(b).
t11(a) :- t12(_X).
t10(b).
t10(a) :- t11(_X).
t9(b).
t9(a) :- t10(_X).
t8(b).
t8(a) :- t9(_X).
t7(b).
t7(a) :- t8(_X).
t6(b).
t6(a) :- t7(_X).
t5(b).
t5(a) :- t6(_X).
t4(b).
t4(a) :- t5(_X).
t3(b).
t3(a) :- t4(_X).
t2(b).
t2(a) :- t3(_X).
t1(a) :- t2(_X).
t1(b).

% 14. Unify terms that are small in space but textually large.

equal(X, X).

term64(X1) :-
   X1 = f(X2, X2),
   X2 = f(X4, X4),
   X4 = f(X8, X8),
   X8 = f(X16, X16),
   X16 = f(X32, X32),
   X32 = f(X64, X64).

term4096(X1) :-
   X1 = f(X2, X2),
   X2 = f(X4, X4),
   X4 = f(X8, X8),
   X8 = f(X16, X16),
   X16 = f(X32, X32),
   X32 = f(X64, X64),
   X64 = f(X128, X128),
   X128 = f(X256, X256),
   X256 = f(X512, X512),
   X512 = f(X1024, X1024),
   X1024 = f(X2048, X2048),
   X2048 = f(X4096, X4096).

% 15. Do 100 integer additions nonrecursively,
% avoiding obvious compiler optimizations.

a100(M, K, P) :- P is M + K.
a99(M, K, P) :- N is M + K, a100(N, 100, P).
a98(M, K, P) :- N is M + K, a99(N, 99, P).
a97(M, K, P) :- N is M + K, a98(N, 98, P).
a96(M, K, P) :- N is M + K, a97(N, 97, P).
a95(M, K, P) :- N is M + K, a96(N, 96, P).
a94(M, K, P) :- N is M + K, a95(N, 95, P).
a93(M, K, P) :- N is M + K, a94(N, 94, P).
a92(M, K, P) :- N is M + K, a93(N, 93, P).
a91(M, K, P) :- N is M + K, a92(N, 92, P).
a90(M, K, P) :- N is M + K, a91(N, 91, P).
a89(M, K, P) :- N is M + K, a90(N, 90, P).
a88(M, K, P) :- N is M + K, a89(N, 89, P).
a87(M, K, P) :- N is M + K, a88(N, 88, P).
a86(M, K, P) :- N is M + K, a87(N, 87, P).
a85(M, K, P) :- N is M + K, a86(N, 86, P).
a84(M, K, P) :- N is M + K, a85(N, 85, P).
a83(M, K, P) :- N is M + K, a84(N, 84, P).
a82(M, K, P) :- N is M + K, a83(N, 83, P).
a81(M, K, P) :- N is M + K, a82(N, 82, P).
a80(M, K, P) :- N is M + K, a81(N, 81, P).
a79(M, K, P) :- N is M + K, a80(N, 80, P).
a78(M, K, P) :- N is M + K, a79(N, 79, P).
a77(M, K, P) :- N is M + K, a78(N, 78, P).
a76(M, K, P) :- N is M + K, a77(N, 77, P).
a75(M, K, P) :- N is M + K, a76(N, 76, P).
a74(M, K, P) :- N is M + K, a75(N, 75, P).
a73(M, K, P) :- N is M + K, a74(N, 74, P).
a72(M, K, P) :- N is M + K, a73(N, 73, P).
a71(M, K, P) :- N is M + K, a72(N, 72, P).
a70(M, K, P) :- N is M + K, a71(N, 71, P).
a69(M, K, P) :- N is M + K, a70(N, 70, P).
a68(M, K, P) :- N is M + K, a69(N, 69, P).
a67(M, K, P) :- N is M + K, a68(N, 68, P).
a66(M, K, P) :- N is M + K, a67(N, 67, P).
a65(M, K, P) :- N is M + K, a66(N, 66, P).
a64(M, K, P) :- N is M + K, a65(N, 65, P).
a63(M, K, P) :- N is M + K, a64(N, 64, P).
a62(M, K, P) :- N is M + K, a63(N, 63, P).
a61(M, K, P) :- N is M + K, a62(N, 62, P).
a60(M, K, P) :- N is M + K, a61(N, 61, P).
a59(M, K, P) :- N is M + K, a60(N, 60, P).
a58(M, K, P) :- N is M + K, a59(N, 59, P).
a57(M, K, P) :- N is M + K, a58(N, 58, P).
a56(M, K, P) :- N is M + K, a57(N, 57, P).
a55(M, K, P) :- N is M + K, a56(N, 56, P).
a54(M, K, P) :- N is M + K, a55(N, 55, P).
a53(M, K, P) :- N is M + K, a54(N, 54, P).
a52(M, K, P) :- N is M + K, a53(N, 53, P).
a51(M, K, P) :- N is M + K, a52(N, 52, P).
a50(M, K, P) :- N is M + K, a51(N, 51, P).
a49(M, K, P) :- N is M + K, a50(N, 50, P).
a48(M, K, P) :- N is M + K, a49(N, 49, P).
a47(M, K, P) :- N is M + K, a48(N, 48, P).
a46(M, K, P) :- N is M + K, a47(N, 47, P).
a45(M, K, P) :- N is M + K, a46(N, 46, P).
a44(M, K, P) :- N is M + K, a45(N, 45, P).
a43(M, K, P) :- N is M + K, a44(N, 44, P).
a42(M, K, P) :- N is M + K, a43(N, 43, P).
a41(M, K, P) :- N is M + K, a42(N, 42, P).
a40(M, K, P) :- N is M + K, a41(N, 41, P).
a39(M, K, P) :- N is M + K, a40(N, 40, P).
a38(M, K, P) :- N is M + K, a39(N, 39, P).
a37(M, K, P) :- N is M + K, a38(N, 38, P).
a36(M, K, P) :- N is M + K, a37(N, 37, P).
a35(M, K, P) :- N is M + K, a36(N, 36, P).
a34(M, K, P) :- N is M + K, a35(N, 35, P).
a33(M, K, P) :- N is M + K, a34(N, 34, P).
a32(M, K, P) :- N is M + K, a33(N, 33, P).
a31(M, K, P) :- N is M + K, a32(N, 32, P).
a30(M, K, P) :- N is M + K, a31(N, 31, P).
a29(M, K, P) :- N is M + K, a30(N, 30, P).
a28(M, K, P) :- N is M + K, a29(N, 29, P).
a27(M, K, P) :- N is M + K, a28(N, 28, P).
a26(M, K, P) :- N is M + K, a27(N, 27, P).
a25(M, K, P) :- N is M + K, a26(N, 26, P).
a24(M, K, P) :- N is M + K, a25(N, 25, P).
a23(M, K, P) :- N is M + K, a24(N, 24, P).
a22(M, K, P) :- N is M + K, a23(N, 23, P).
a21(M, K, P) :- N is M + K, a22(N, 22, P).
a20(M, K, P) :- N is M + K, a21(N, 21, P).
a19(M, K, P) :- N is M + K, a20(N, 20, P).
a18(M, K, P) :- N is M + K, a19(N, 19, P).
a17(M, K, P) :- N is M + K, a18(N, 18, P).
a16(M, K, P) :- N is M + K, a17(N, 17, P).
a15(M, K, P) :- N is M + K, a16(N, 16, P).
a14(M, K, P) :- N is M + K, a15(N, 15, P).
a13(M, K, P) :- N is M + K, a14(N, 14, P).
a12(M, K, P) :- N is M + K, a13(N, 13, P).
a11(M, K, P) :- N is M + K, a12(N, 12, P).
a10(M, K, P) :- N is M + K, a11(N, 11, P).
a9(M, K, P) :- N is M + K, a10(N, 10, P).
a8(M, K, P) :- N is M + K, a9(N, 9, P).
a7(M, K, P) :- N is M + K, a8(N, 8, P).
a6(M, K, P) :- N is M + K, a7(N, 7, P).
a5(M, K, P) :- N is M + K, a6(N, 6, P).
a4(M, K, P) :- N is M + K, a5(N, 5, P).
a3(M, K, P) :- N is M + K, a4(N, 4, P).
a2(M, K, P) :- N is M + K, a3(N, 3, P).
a1(M, K, P) :- N is M + K, a2(N, 2, P).

% 16. 100 floating additions

fa100(M, K, P) :- P is M + K.
fa99(M, K, P) :- N is M + K, fa100(N, 100.1, P).
fa98(M, K, P) :- N is M + K, fa99(N, 99.1, P).
fa97(M, K, P) :- N is M + K, fa98(N, 98.1, P).
fa96(M, K, P) :- N is M + K, fa97(N, 97.1, P).
fa95(M, K, P) :- N is M + K, fa96(N, 96.1, P).
fa94(M, K, P) :- N is M + K, fa95(N, 95.1, P).
fa93(M, K, P) :- N is M + K, fa94(N, 94.1, P).
fa92(M, K, P) :- N is M + K, fa93(N, 93.1, P).
fa91(M, K, P) :- N is M + K, fa92(N, 92.1, P).
fa90(M, K, P) :- N is M + K, fa91(N, 91.1, P).
fa89(M, K, P) :- N is M + K, fa90(N, 90.1, P).
fa88(M, K, P) :- N is M + K, fa89(N, 89.1, P).
fa87(M, K, P) :- N is M + K, fa88(N, 88.1, P).
fa86(M, K, P) :- N is M + K, fa87(N, 87.1, P).
fa85(M, K, P) :- N is M + K, fa86(N, 86.1, P).
fa84(M, K, P) :- N is M + K, fa85(N, 85.1, P).
fa83(M, K, P) :- N is M + K, fa84(N, 84.1, P).
fa82(M, K, P) :- N is M + K, fa83(N, 83.1, P).
fa81(M, K, P) :- N is M + K, fa82(N, 82.1, P).
fa80(M, K, P) :- N is M + K, fa81(N, 81.1, P).
fa79(M, K, P) :- N is M + K, fa80(N, 80.1, P).
fa78(M, K, P) :- N is M + K, fa79(N, 79.1, P).
fa77(M, K, P) :- N is M + K, fa78(N, 78.1, P).
fa76(M, K, P) :- N is M + K, fa77(N, 77.1, P).
fa75(M, K, P) :- N is M + K, fa76(N, 76.1, P).
fa74(M, K, P) :- N is M + K, fa75(N, 75.1, P).
fa73(M, K, P) :- N is M + K, fa74(N, 74.1, P).
fa72(M, K, P) :- N is M + K, fa73(N, 73.1, P).
fa71(M, K, P) :- N is M + K, fa72(N, 72.1, P).
fa70(M, K, P) :- N is M + K, fa71(N, 71.1, P).
fa69(M, K, P) :- N is M + K, fa70(N, 70.1, P).
fa68(M, K, P) :- N is M + K, fa69(N, 69.1, P).
fa67(M, K, P) :- N is M + K, fa68(N, 68.1, P).
fa66(M, K, P) :- N is M + K, fa67(N, 67.1, P).
fa65(M, K, P) :- N is M + K, fa66(N, 66.1, P).
fa64(M, K, P) :- N is M + K, fa65(N, 65.1, P).
fa63(M, K, P) :- N is M + K, fa64(N, 64.1, P).
fa62(M, K, P) :- N is M + K, fa63(N, 63.1, P).
fa61(M, K, P) :- N is M + K, fa62(N, 62.1, P).
fa60(M, K, P) :- N is M + K, fa61(N, 61.1, P).
fa59(M, K, P) :- N is M + K, fa60(N, 60.1, P).
fa58(M, K, P) :- N is M + K, fa59(N, 59.1, P).
fa57(M, K, P) :- N is M + K, fa58(N, 58.1, P).
fa56(M, K, P) :- N is M + K, fa57(N, 57.1, P).
fa55(M, K, P) :- N is M + K, fa56(N, 56.1, P).
fa54(M, K, P) :- N is M + K, fa55(N, 55.1, P).
fa53(M, K, P) :- N is M + K, fa54(N, 54.1, P).
fa52(M, K, P) :- N is M + K, fa53(N, 53.1, P).
fa51(M, K, P) :- N is M + K, fa52(N, 52.1, P).
fa50(M, K, P) :- N is M + K, fa51(N, 51.1, P).
fa49(M, K, P) :- N is M + K, fa50(N, 50.1, P).
fa48(M, K, P) :- N is M + K, fa49(N, 49.1, P).
fa47(M, K, P) :- N is M + K, fa48(N, 48.1, P).
fa46(M, K, P) :- N is M + K, fa47(N, 47.1, P).
fa45(M, K, P) :- N is M + K, fa46(N, 46.1, P).
fa44(M, K, P) :- N is M + K, fa45(N, 45.1, P).
fa43(M, K, P) :- N is M + K, fa44(N, 44.1, P).
fa42(M, K, P) :- N is M + K, fa43(N, 43.1, P).
fa41(M, K, P) :- N is M + K, fa42(N, 42.1, P).
fa40(M, K, P) :- N is M + K, fa41(N, 41.1, P).
fa39(M, K, P) :- N is M + K, fa40(N, 40.1, P).
fa38(M, K, P) :- N is M + K, fa39(N, 39.1, P).
fa37(M, K, P) :- N is M + K, fa38(N, 38.1, P).
fa36(M, K, P) :- N is M + K, fa37(N, 37.1, P).
fa35(M, K, P) :- N is M + K, fa36(N, 36.1, P).
fa34(M, K, P) :- N is M + K, fa35(N, 35.1, P).
fa33(M, K, P) :- N is M + K, fa34(N, 34.1, P).
fa32(M, K, P) :- N is M + K, fa33(N, 33.1, P).
fa31(M, K, P) :- N is M + K, fa32(N, 32.1, P).
fa30(M, K, P) :- N is M + K, fa31(N, 31.1, P).
fa29(M, K, P) :- N is M + K, fa30(N, 30.1, P).
fa28(M, K, P) :- N is M + K, fa29(N, 29.1, P).
fa27(M, K, P) :- N is M + K, fa28(N, 28.1, P).
fa26(M, K, P) :- N is M + K, fa27(N, 27.1, P).
fa25(M, K, P) :- N is M + K, fa26(N, 26.1, P).
fa24(M, K, P) :- N is M + K, fa25(N, 25.1, P).
fa23(M, K, P) :- N is M + K, fa24(N, 24.1, P).
fa22(M, K, P) :- N is M + K, fa23(N, 23.1, P).
fa21(M, K, P) :- N is M + K, fa22(N, 22.1, P).
fa20(M, K, P) :- N is M + K, fa21(N, 21.1, P).
fa19(M, K, P) :- N is M + K, fa20(N, 20.1, P).
fa18(M, K, P) :- N is M + K, fa19(N, 19.1, P).
fa17(M, K, P) :- N is M + K, fa18(N, 18.1, P).
fa16(M, K, P) :- N is M + K, fa17(N, 17.1, P).
fa15(M, K, P) :- N is M + K, fa16(N, 16.1, P).
fa14(M, K, P) :- N is M + K, fa15(N, 15.1, P).
fa13(M, K, P) :- N is M + K, fa14(N, 14.1, P).
fa12(M, K, P) :- N is M + K, fa13(N, 13.1, P).
fa11(M, K, P) :- N is M + K, fa12(N, 12.1, P).
fa10(M, K, P) :- N is M + K, fa11(N, 11.1, P).
fa9(M, K, P) :- N is M + K, fa10(N, 10.1, P).
fa8(M, K, P) :- N is M + K, fa9(N, 9.1, P).
fa7(M, K, P) :- N is M + K, fa8(N, 8.1, P).
fa6(M, K, P) :- N is M + K, fa7(N, 7.1, P).
fa5(M, K, P) :- N is M + K, fa6(N, 6.1, P).
fa4(M, K, P) :- N is M + K, fa5(N, 5.1, P).
fa3(M, K, P) :- N is M + K, fa4(N, 4.1, P).
fa2(M, K, P) :- N is M + K, fa3(N, 3.1, P).
fa1(M, K, P) :- N is M + K, fa2(N, 2.1, P).

% 17. 100 calls to arg at position N

arg100(N, T, R) :- arg(N, T, R).
arg99(N, T, R) :- arg(N, T, X), arg100(N, X, R).
arg98(N, T, R) :- arg(N, T, X), arg99(N, X, R).
arg97(N, T, R) :- arg(N, T, X), arg98(N, X, R).
arg96(N, T, R) :- arg(N, T, X), arg97(N, X, R).
arg95(N, T, R) :- arg(N, T, X), arg96(N, X, R).
arg94(N, T, R) :- arg(N, T, X), arg95(N, X, R).
arg93(N, T, R) :- arg(N, T, X), arg94(N, X, R).
arg92(N, T, R) :- arg(N, T, X), arg93(N, X, R).
arg91(N, T, R) :- arg(N, T, X), arg92(N, X, R).
arg90(N, T, R) :- arg(N, T, X), arg91(N, X, R).
arg89(N, T, R) :- arg(N, T, X), arg90(N, X, R).
arg88(N, T, R) :- arg(N, T, X), arg89(N, X, R).
arg87(N, T, R) :- arg(N, T, X), arg88(N, X, R).
arg86(N, T, R) :- arg(N, T, X), arg87(N, X, R).
arg85(N, T, R) :- arg(N, T, X), arg86(N, X, R).
arg84(N, T, R) :- arg(N, T, X), arg85(N, X, R).
arg83(N, T, R) :- arg(N, T, X), arg84(N, X, R).
arg82(N, T, R) :- arg(N, T, X), arg83(N, X, R).
arg81(N, T, R) :- arg(N, T, X), arg82(N, X, R).
arg80(N, T, R) :- arg(N, T, X), arg81(N, X, R).
arg79(N, T, R) :- arg(N, T, X), arg80(N, X, R).
arg78(N, T, R) :- arg(N, T, X), arg79(N, X, R).
arg77(N, T, R) :- arg(N, T, X), arg78(N, X, R).
arg76(N, T, R) :- arg(N, T, X), arg77(N, X, R).
arg75(N, T, R) :- arg(N, T, X), arg76(N, X, R).
arg74(N, T, R) :- arg(N, T, X), arg75(N, X, R).
arg73(N, T, R) :- arg(N, T, X), arg74(N, X, R).
arg72(N, T, R) :- arg(N, T, X), arg73(N, X, R).
arg71(N, T, R) :- arg(N, T, X), arg72(N, X, R).
arg70(N, T, R) :- arg(N, T, X), arg71(N, X, R).
arg69(N, T, R) :- arg(N, T, X), arg70(N, X, R).
arg68(N, T, R) :- arg(N, T, X), arg69(N, X, R).
arg67(N, T, R) :- arg(N, T, X), arg68(N, X, R).
arg66(N, T, R) :- arg(N, T, X), arg67(N, X, R).
arg65(N, T, R) :- arg(N, T, X), arg66(N, X, R).
arg64(N, T, R) :- arg(N, T, X), arg65(N, X, R).
arg63(N, T, R) :- arg(N, T, X), arg64(N, X, R).
arg62(N, T, R) :- arg(N, T, X), arg63(N, X, R).
arg61(N, T, R) :- arg(N, T, X), arg62(N, X, R).
arg60(N, T, R) :- arg(N, T, X), arg61(N, X, R).
arg59(N, T, R) :- arg(N, T, X), arg60(N, X, R).
arg58(N, T, R) :- arg(N, T, X), arg59(N, X, R).
arg57(N, T, R) :- arg(N, T, X), arg58(N, X, R).
arg56(N, T, R) :- arg(N, T, X), arg57(N, X, R).
arg55(N, T, R) :- arg(N, T, X), arg56(N, X, R).
arg54(N, T, R) :- arg(N, T, X), arg55(N, X, R).
arg53(N, T, R) :- arg(N, T, X), arg54(N, X, R).
arg52(N, T, R) :- arg(N, T, X), arg53(N, X, R).
arg51(N, T, R) :- arg(N, T, X), arg52(N, X, R).
arg50(N, T, R) :- arg(N, T, X), arg51(N, X, R).
arg49(N, T, R) :- arg(N, T, X), arg50(N, X, R).
arg48(N, T, R) :- arg(N, T, X), arg49(N, X, R).
arg47(N, T, R) :- arg(N, T, X), arg48(N, X, R).
arg46(N, T, R) :- arg(N, T, X), arg47(N, X, R).
arg45(N, T, R) :- arg(N, T, X), arg46(N, X, R).
arg44(N, T, R) :- arg(N, T, X), arg45(N, X, R).
arg43(N, T, R) :- arg(N, T, X), arg44(N, X, R).
arg42(N, T, R) :- arg(N, T, X), arg43(N, X, R).
arg41(N, T, R) :- arg(N, T, X), arg42(N, X, R).
arg40(N, T, R) :- arg(N, T, X), arg41(N, X, R).
arg39(N, T, R) :- arg(N, T, X), arg40(N, X, R).
arg38(N, T, R) :- arg(N, T, X), arg39(N, X, R).
arg37(N, T, R) :- arg(N, T, X), arg38(N, X, R).
arg36(N, T, R) :- arg(N, T, X), arg37(N, X, R).
arg35(N, T, R) :- arg(N, T, X), arg36(N, X, R).
arg34(N, T, R) :- arg(N, T, X), arg35(N, X, R).
arg33(N, T, R) :- arg(N, T, X), arg34(N, X, R).
arg32(N, T, R) :- arg(N, T, X), arg33(N, X, R).
arg31(N, T, R) :- arg(N, T, X), arg32(N, X, R).
arg30(N, T, R) :- arg(N, T, X), arg31(N, X, R).
arg29(N, T, R) :- arg(N, T, X), arg30(N, X, R).
arg28(N, T, R) :- arg(N, T, X), arg29(N, X, R).
arg27(N, T, R) :- arg(N, T, X), arg28(N, X, R).
arg26(N, T, R) :- arg(N, T, X), arg27(N, X, R).
arg25(N, T, R) :- arg(N, T, X), arg26(N, X, R).
arg24(N, T, R) :- arg(N, T, X), arg25(N, X, R).
arg23(N, T, R) :- arg(N, T, X), arg24(N, X, R).
arg22(N, T, R) :- arg(N, T, X), arg23(N, X, R).
arg21(N, T, R) :- arg(N, T, X), arg22(N, X, R).
arg20(N, T, R) :- arg(N, T, X), arg21(N, X, R).
arg19(N, T, R) :- arg(N, T, X), arg20(N, X, R).
arg18(N, T, R) :- arg(N, T, X), arg19(N, X, R).
arg17(N, T, R) :- arg(N, T, X), arg18(N, X, R).
arg16(N, T, R) :- arg(N, T, X), arg17(N, X, R).
arg15(N, T, R) :- arg(N, T, X), arg16(N, X, R).
arg14(N, T, R) :- arg(N, T, X), arg15(N, X, R).
arg13(N, T, R) :- arg(N, T, X), arg14(N, X, R).
arg12(N, T, R) :- arg(N, T, X), arg13(N, X, R).
arg11(N, T, R) :- arg(N, T, X), arg12(N, X, R).
arg10(N, T, R) :- arg(N, T, X), arg11(N, X, R).
arg9(N, T, R) :- arg(N, T, X), arg10(N, X, R).
arg8(N, T, R) :- arg(N, T, X), arg9(N, X, R).
arg7(N, T, R) :- arg(N, T, X), arg8(N, X, R).
arg6(N, T, R) :- arg(N, T, X), arg7(N, X, R).
arg5(N, T, R) :- arg(N, T, X), arg6(N, X, R).
arg4(N, T, R) :- arg(N, T, X), arg5(N, X, R).
arg3(N, T, R) :- arg(N, T, X), arg4(N, X, R).
arg2(N, T, R) :- arg(N, T, X), arg3(N, X, R).
arg1(N, T, R) :- arg(N, T, X), arg2(N, X, R).

fill_nary_term(0, _, _) :- !.
fill_nary_term(N, SubTerm, Term) :-
   N > 0, M is N - 1,
   arg(N, Term, SubTerm),
   fill_nary_term(M, SubTerm, Term).

nary_term(N, SubTerm, Term) :-
   functor(Term, f, N),
   fill_nary_term(N, SubTerm, Term).

complex_nary_term(0, N, N) :- !.
complex_nary_term(I, N, Term) :-
   I > 0, J is I - 1,
   complex_nary_term(J, N, SubTerm),
   nary_term(N, SubTerm, Term).

% 18. 100 indexed calls; some systems may require extra declarations to
% put an index on the first argument.

ix(1) :- ix(10000).
ix(4).
ix(9) :- ix(4).
ix(16) :- ix(9).
ix(25) :- ix(16).
ix(36) :- ix(25).
ix(49) :- ix(36).
ix(64) :- ix(49).
ix(81) :- ix(64).
ix(100) :- ix(81).
ix(121) :- ix(100).
ix(144) :- ix(121).
ix(169) :- ix(144).
ix(196) :- ix(169).
ix(225) :- ix(196).
ix(256) :- ix(225).
ix(289) :- ix(256).
ix(324) :- ix(289).
ix(361) :- ix(324).
ix(400) :- ix(361).
ix(441) :- ix(400).
ix(484) :- ix(441).
ix(529) :- ix(484).
ix(576) :- ix(529).
ix(625) :- ix(576).
ix(676) :- ix(625).
ix(729) :- ix(676).
ix(784) :- ix(729).
ix(841) :- ix(784).
ix(900) :- ix(841).
ix(961) :- ix(900).
ix(1024) :- ix(961).
ix(1089) :- ix(1024).
ix(1156) :- ix(1089).
ix(1225) :- ix(1156).
ix(1296) :- ix(1225).
ix(1369) :- ix(1296).
ix(1444) :- ix(1369).
ix(1521) :- ix(1444).
ix(1600) :- ix(1521).
ix(1681) :- ix(1600).
ix(1764) :- ix(1681).
ix(1849) :- ix(1764).
ix(1936) :- ix(1849).
ix(2025) :- ix(1936).
ix(2116) :- ix(2025).
ix(2209) :- ix(2116).
ix(2304) :- ix(2209).
ix(2401) :- ix(2304).
ix(2500) :- ix(2401).
ix(2601) :- ix(2500).
ix(2704) :- ix(2601).
ix(2809) :- ix(2704).
ix(2916) :- ix(2809).
ix(3025) :- ix(2916).
ix(3136) :- ix(3025).
ix(3249) :- ix(3136).
ix(3364) :- ix(3249).
ix(3481) :- ix(3364).
ix(3600) :- ix(3481).
ix(3721) :- ix(3600).
ix(3844) :- ix(3721).
ix(3969) :- ix(3844).
ix(4096) :- ix(3969).
ix(4225) :- ix(4096).
ix(4356) :- ix(4225).
ix(4489) :- ix(4356).
ix(4624) :- ix(4489).
ix(4761) :- ix(4624).
ix(4900) :- ix(4761).
ix(5041) :- ix(4900).
ix(5184) :- ix(5041).
ix(5329) :- ix(5184).
ix(5476) :- ix(5329).
ix(5625) :- ix(5476).
ix(5776) :- ix(5625).
ix(5929) :- ix(5776).
ix(6084) :- ix(5929).
ix(6241) :- ix(6084).
ix(6400) :- ix(6241).
ix(6561) :- ix(6400).
ix(6724) :- ix(6561).
ix(6889) :- ix(6724).
ix(7056) :- ix(6889).
ix(7225) :- ix(7056).
ix(7396) :- ix(7225).
ix(7569) :- ix(7396).
ix(7744) :- ix(7569).
ix(7921) :- ix(7744).
ix(8100) :- ix(7921).
ix(8281) :- ix(8100).
ix(8464) :- ix(8281).
ix(8649) :- ix(8464).
ix(8836) :- ix(8649).
ix(9025) :- ix(8836).
ix(9216) :- ix(9025).
ix(9409) :- ix(9216).
ix(9604) :- ix(9409).
ix(9801) :- ix(9604).
ix(10000) :- ix(9801).

% 19. Make 1000 asserts of unit clauses

create_units(I, N, []) :- I > N, !.
create_units(I, N, [ua(K, X, f(K, X))|Rest]) :-
    K is I * (1 + I//100),
    J is I + 1,
    create_units(J, N, Rest).

assert_clauses([]).
assert_clauses([Clause|Rest]) :-
   assert(Clause),
   assert_clauses(Rest).

% 20. Access 100 dynamically-created clauses with 1st arg. instantiated

dix_clauses(I, N, []) :- I > N, !.
dix_clauses(I, N, [dix(P, Q) | L]) :-
   I =< N,
   P is I*I,
   R is 1 + (I+N-2) mod N,
   Q is R*R,
   J is I + 1,
   dix_clauses(J, N, L).

access_dix(Start, End) :-
   dix(Start, Where),
 ( Where = End -> true ; access_dix(Where, End) ).

% 21. Access 100 dynamic unit clauses (2nd argument instantiated)

access_back(Start, End) :-
   dix(Where, Start),
 ( Where = End -> true ; access_back(Where, End) ).

% 22. Setof and bagof

pr(99, 1).
pr(98, 2).
pr(97, 3).
pr(96, 4).
pr(95, 5).
pr(94, 6).
pr(93, 7).
pr(92, 8).
pr(91, 9).
pr(90, 10).
pr(89, 11).
pr(88, 12).
pr(87, 13).
pr(86, 14).
pr(85, 15).
pr(84, 16).
pr(83, 17).
pr(82, 18).
pr(81, 19).
pr(80, 20).
pr(79, 21).
pr(78, 22).
pr(77, 23).
pr(76, 24).
pr(75, 25).
pr(74, 26).
pr(73, 27).
pr(72, 28).
pr(71, 29).
pr(70, 30).
pr(69, 31).
pr(68, 32).
pr(67, 33).
pr(66, 34).
pr(65, 35).
pr(64, 36).
pr(63, 37).
pr(62, 38).
pr(61, 39).
pr(60, 40).
pr(59, 41).
pr(58, 42).
pr(57, 43).
pr(56, 44).
pr(55, 45).
pr(54, 46).
pr(53, 47).
pr(52, 48).
pr(51, 49).
pr(50, 50).
pr(49, 51).
pr(48, 52).
pr(47, 53).
pr(46, 54).
pr(45, 55).
pr(44, 56).
pr(43, 57).
pr(42, 58).
pr(41, 59).
pr(40, 60).
pr(39, 61).
pr(38, 62).
pr(37, 63).
pr(36, 64).
pr(35, 65).
pr(34, 66).
pr(33, 67).
pr(32, 68).
pr(31, 69).
pr(30, 70).
pr(29, 71).
pr(28, 72).
pr(27, 73).
pr(26, 74).
pr(25, 75).
pr(24, 76).
pr(23, 77).
pr(22, 78).
pr(21, 79).
pr(20, 80).
pr(19, 81).
pr(18, 82).
pr(17, 83).
pr(16, 84).
pr(15, 85).
pr(14, 86).
pr(13, 87).
pr(12, 88).
pr(11, 89).
pr(10, 90).
pr(9, 91).
pr(8, 92).
pr(7, 93).
pr(6, 94).
pr(5, 95).
pr(4, 96).
pr(3, 97).
pr(2, 98).
pr(1, 99).
pr(0, 100).

%23a calls to functor/3 with second and third argument uninstantiated

functors(0).
functors(4).

functor_in(T) :-
	functor(T, _, _),
	functor(T, _, _),
	functor(T, _, _),
	functor(T, _, _),
	functor(T, _, _),
	functor(T, _, _),
	functor(T, _, _),
	functor(T, _, _),
	functor(T, _, _),
	functor(T, _, _),
	functor(T, _, _),
	functor(T, _, _),
	functor(T, _, _),
	functor(T, _, _),
	functor(T, _, _),
	functor(T, _, _),
	functor(T, _, _),
	functor(T, _, _),
	functor(T, _, _),
	functor(T, _, _).

%23b calls to functor/3 with first argument uninstantiated

functor_out(F, N) :-
	functor(_, F, N),
	functor(_, F, N),
	functor(_, F, N),
	functor(_, F, N),
	functor(_, F, N),
	functor(_, F, N),
	functor(_, F, N),
	functor(_, F, N),
	functor(_, F, N),
	functor(_, F, N),
	functor(_, F, N),
	functor(_, F, N),
	functor(_, F, N),
	functor(_, F, N),
	functor(_, F, N),
	functor(_, F, N),
	functor(_, F, N),
	functor(_, F, N),
	functor(_, F, N),
	functor(_, F, N).

%23c calls to functor/3 with all arguments instantiated

functor_unify(T, F, N) :-
	functor(T, F, N),
	functor(T, F, N),
	functor(T, F, N),
	functor(T, F, N),
	functor(T, F, N),
	functor(T, F, N),
	functor(T, F, N),
	functor(T, F, N),
	functor(T, F, N),
	functor(T, F, N),
	functor(T, F, N),
	functor(T, F, N),
	functor(T, F, N),
	functor(T, F, N),
	functor(T, F, N),
	functor(T, F, N),
	functor(T, F, N),
	functor(T, F, N),
	functor(T, F, N),
	functor(T, F, N).

%24 measures the metacall
call_test(G) :-
	call(G),
	call(G),
	call(G),
	call(G),
	call(G),
	call(G),
	call(G),
	call(G),
	call(G),
	call(G).

call_dummy(number(_)) :-
	number(1),
	number(1),
	number(1),
	number(1),
	number(1),
	number(1),
	number(1),
	number(1),
	number(1),
	number(1).
call_dummy(_ == _) :-
	a == a,
	a == a,
	a == a,
	a == a,
	a == a,
	a == a,
	a == a,
	a == a,
	a == a,
	a == a.
call_dummy(dummy) :-
	dummy,
	dummy,
	dummy,
	dummy,
	dummy,
	dummy,
	dummy,
	dummy,
	dummy,
	dummy,
	dummy.
call_dummy(dummy(_, _, _)) :-
	dummy(_, _, _),
	dummy(_, _, _),
	dummy(_, _, _),
	dummy(_, _, _),
	dummy(_, _, _),
	dummy(_, _, _),
	dummy(_, _, _),
	dummy(_, _, _),
	dummy(_, _, _),
	dummy(_, _, _).

call_goal(number(1)).
call_goal(a == a).
call_goal(dummy).
call_goal(dummy(_, _, _)).

%%25 measures output
%
%write_1(File, Term) :-
%	open(File, write, S),
%	write(S, Term), write(S, '.'), nl(S),
%	close(S).
%
%write_2(File, Term) :-
%	open(File, write, S),
%	writeq(S, Term), write(S, '.'), nl(S),
%	writeq(S, Term), write(S, '.'), nl(S),
%	close(S).
%
%%26 measures input
%
%read_1(File, Term) :-
%	open(File, read, S),
%	read(S, Term),
%	close(S).
%
%read_2(File, Term) :-
%	open(File, read, S),
%	read(S, Term),
%	read(S, Term),
%	close(S).

% Queries, dummy control predicates and programs

% Queries

% 1
bench_mark(tail_call_atom_atom, 6000, p1(a), dummy(a)).

% 2
bench_mark(binary_call_atom_atom, 6000, q1(a), dummy(a)).

% 3
bench_mark(cons_list, 6000, r1(L), dummy(L)).

% 4
bench_mark(walk_list, 6000, r1(L), dummy(L)) :- r1(L).

% 5
bench_mark(walk_list_rec, 6000, wlr(L), dummy(L)) :- r1(L).

% 6
bench_mark(args(N), 6000, args(N, L), dummy(N, L)) :- args(N), r1(L).

% 7
bench_mark(cons_term, 6000, s1(T), dummy(T)).

% 8
bench_mark(walk_term, 6000, s1(T), dummy(T)) :- s1(T).

% 9
bench_mark(walk_term_rec, 6000, wtr(T), dummy(T)) :- s1(T).

% 10
bench_mark(shallow_backtracking, 6000, shallow, dummy).

% 11
bench_mark(deep_backtracking, 6000, deep, dummy).

% 12
bench_mark(choice_point, 6000, choice, dummy).

% 13
bench_mark(trail_variables, 6000, trail, dummy).

% 14
bench_mark(medium_unify, 12000, equal(Term1, Term2), dummy(Term1, Term2)) :-
   term64(Term1),
   term64(Term2).
bench_mark(deep_unify, 300, equal(Term1, Term2), dummy(Term1, Term2)) :-
   term4096(Term1),
   term4096(Term2).

% 15
bench_mark(integer_add, 3000, a1(0, 1, R), dummy(0, 1, R)).

% 16
bench_mark(floating_add, 3000, fa1(0.1, 1.1, R), dummy(0.1, 1.1, R)).

% 17
bench_mark(arg(N), 6000, arg1(N, Term, R), dummy(N, Term, R)) :-
   args(N),
   complex_nary_term(100, N, Term).

% 18
bench_mark(index, 6000, ix(1), dummy(1)).

% 19
bench_mark(assert_unit, 3, assert_clauses(L), dummy(L)) :-
   abolish(ua,3),
   create_units(1, 1000, L).

% 20
bench_mark(access_unit, 300, access_dix(1, 1), dummy(1, 1)) :-
   abolish(dix, 2),
   dix_clauses(1, 100, L),
   assert_clauses(L).

% 21
bench_mark(slow_access_unit, 30, access_back(1, 1), dummy(1, 1)) :-
   abolish(dix, 2),
   dix_clauses(1, 100, L),
   assert_clauses(L).

% 22
bench_mark(setof, 30, setof(X, Y^pr(X, Y), S), dummy(X, Y^pr(X, Y), S)).
bench_mark(pair_setof, 30,
   setof((X,Y), pr(X, Y), S),
   dummy((X,Y), pr(X, Y), S)).
bench_mark(double_setof, 30, setof((X,S), setof(Y, pr(X, Y), S), T),
                       dummy(S, setof(Y, pr(X, Y), S), T)).
bench_mark(bagof, 30, bagof(X, Y^pr(X, Y), S), dummy(X, Y^pr(X, Y), S)).

% 23a
bench_mark(functor_in(N), 2000, functor_in(T), dummy(T)) :-
   functors(N),
   functor(T, f, N).

% 23b
bench_mark(functor_out(N), 2000, functor_out(f, N), dummy(f, N)) :-
   functors(N).

% 23c
bench_mark(functor_unify(N), 2000, functor_unify(T, f, N), dummy(T, f, N)) :-
   functors(N),
   functor(T, f, N).

% 24
bench_mark(call_test(G), 2000, call_test(G), call_dummy(G)) :-
   call_goal(G).

%% 25
%bench_mark(write(1), 200, write_1(File, T), dummy(File, T)) :- 
%	File = '/tmp/bench.pl', s1(T).
%
%bench_mark(write(2), 50, write_2(File, T), dummy(File, T)) :- 
%	File = '/tmp/bench.pl', s1(T).
%
%% 26
%bench_mark(read(1), 200, read_1(File, _), dummy(File, _)) :- 
%	File = '/tmp/bench.pl', s1(T), write_1(File, T).
%
%bench_mark(read(2), 50, read_2(File, _), dummy(File, _)) :- 
%	File = '/tmp/bench.pl', s1(T), write_2(File, T).


get_cpu_time(T) :- statistics(runtime, [T|_]).

time_unit(ms).

%   repeat(N)
%   succeeds precisely N times.

repeat(N) :-
        N > 0,
	repeat_(N).

repeat_(N) :- N < 2, !.
repeat_(_).
repeat_(N) :- M is N - 1, repeat_(M).

%   bench_mark(Name)
%   is the top level.  It calls bench_mark_/4 to find out 
%   how many Iterations of the Action and its Control to perform.

get_iterations_(ProbeIterations, Action, Iterations) :-
	get_cpu_time(T0),
	(
	    repeat(ProbeIterations), call(Action), fail 
	;
	    get_cpu_time(T1)
	),
	T is T1 - T0,
	(T < 1000 -> 	% too close to UNIX clock resolution
		ProbeIterations1 is 10 * ProbeIterations,
		get_iterations_(ProbeIterations1, Action, Iterations)
	;
		ProbeT is T / ProbeIterations,
		(ProbeT > 5000 ->
			Iterations = 1
		;
			Iterations is 5000 / ProbeT
		)
	).

get_iterations(Action, Iterations) :-
	get_iterations_(1, Action, Iterations).

%   report(N, UT0, UT1, UT2)
%   takes the three times yielded by times and the number
%   of iterations and prints the total, overhead, and average of the
%   user cpu time.

report(N, UT0, UT1, UT2) :-
        UTestTime is UT1-UT0,
        UOverHead is UT2-UT1,
        UAverage  is (UTestTime-UOverHead)/N,
	write(' user cpu '), 
        write((UTestTime-UOverHead)/N), write(' = '), write(UAverage),
	write(' '), time_unit(TimeUnit), write(TimeUnit), write('/iteration'),
	nl.


bench_mark_(Name, Iterations, Action, Control) :-
	bench_mark(Name, _, Action, Control),
	get_iterations(Action, Iterations).

bench_mark(Name) :-
        bench_mark_(Name, Iterations, Action, Control),
        get_cpu_time(UT0),
        (   repeat(Iterations), call(Action), fail
        ;   get_cpu_time(UT1)
        ),
        (   repeat(Iterations), call(Control), fail
        ;   get_cpu_time(UT2)
        ),
        write(Name), write(' took '), nl,
        report(Iterations, UT0, UT1, UT2).

%   bench_mark_(Name, Iterations, Action, Control)
%   The number of iterations is computed so that the overall cputime of
%   calling Iterations times Action lasts at least 20s. This was previously
%   hardwired in bench_mark/4.

go :- note,
	bench_mark(_),
	fail.
go.

g:-bench_mark(deep_unify).

bug:-bench_mark(assert_unit).
