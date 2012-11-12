/* Originally written by Neng-Fa Zhou */
/* Optimized (made about 120 times faster :-)) for BinProlog by Paul Tarau */


go :-
    statistics(runtime,_),
    regions(Rs,Dict),
    solve(Rs,Dict),
    statistics(runtime,[_,T]),
    write('execution time is '),write(T), write(' milliseconds'),nl,
    write(Dict),nl.

regions(Rs,Dict):-N=110,
  findall(R,range(R,1,110),Rs),
  functor(Dict,array,N).
 
range(Min,Min,Max):-Min=<Max.
range(I,Min,Max):-
        Min<Max,
        Min1 is Min+1,
        range(I,Min1,Max).

solve([],_).
solve([R|Rs],Dict):-
   arg(R,Dict,C),
   color(C),
   consistent(R,Dict,C,Rs).

consistent(R,Dict,C,_):-
   neighbors(R,Dict,C),
   !,
   fail.
consistent(_,Dict,_,Rs):-solve(Rs,Dict).

color(red).
color(blue).
color(black).
color(yellow).


lmember([X|_],Dict,C):-arg(X,Dict,C).
lmember([_|Xs],Dict,C):-lmember(Xs,Dict,C).


neighbors(1,Dict,C):-lmember([],Dict,C).
neighbors(2,Dict,C):-lmember([1],Dict,C).
neighbors(3,Dict,C):-lmember([1,2],Dict,C).
neighbors(4,Dict,C):-lmember([1,3],Dict,C).
neighbors(5,Dict,C):-lmember([1,4],Dict,C).
neighbors(6,Dict,C):-lmember([1,5],Dict,C).
neighbors(7,Dict,C):-lmember([1,6],Dict,C).
neighbors(8,Dict,C):-lmember([1,7],Dict,C).
neighbors(9,Dict,C):-lmember([1,8],Dict,C).
neighbors(10,Dict,C):-lmember([1,9],Dict,C).
neighbors(11,Dict,C):-lmember([1,10],Dict,C).
neighbors(12,Dict,C):-lmember([2,3],Dict,C).
neighbors(13,Dict,C):-lmember([3,4,12],Dict,C).
neighbors(14,Dict,C):-lmember([4,5,13],Dict,C).
neighbors(15,Dict,C):-lmember([5,6,14],Dict,C).
neighbors(16,Dict,C):-lmember([6,7,15],Dict,C).
neighbors(17,Dict,C):-lmember([7,8,16],Dict,C).
neighbors(18,Dict,C):-lmember([8,9,17],Dict,C).
neighbors(19,Dict,C):-lmember([9,10,18],Dict,C).
neighbors(20,Dict,C):-lmember([1,10,11,19],Dict,C).
neighbors(21,Dict,C):-lmember([12,13],Dict,C).
neighbors(22,Dict,C):-lmember([13,14,21],Dict,C).
neighbors(23,Dict,C):-lmember([14,15,22],Dict,C).
neighbors(24,Dict,C):-lmember([15,16,23],Dict,C).
neighbors(25,Dict,C):-lmember([16,17,24],Dict,C).
neighbors(26,Dict,C):-lmember([17,18,25],Dict,C).
neighbors(27,Dict,C):-lmember([18,19,26],Dict,C).
neighbors(28,Dict,C):-lmember([19,20,27],Dict,C).
neighbors(29,Dict,C):-lmember([21,22],Dict,C).
neighbors(30,Dict,C):-lmember([22,23,29],Dict,C).
neighbors(31,Dict,C):-lmember([23,24,30],Dict,C).
neighbors(32,Dict,C):-lmember([24,25,31],Dict,C).
neighbors(33,Dict,C):-lmember([25,26,32],Dict,C).
neighbors(34,Dict,C):-lmember([26,27,33],Dict,C).
neighbors(35,Dict,C):-lmember([27,28,34],Dict,C).
neighbors(36,Dict,C):-lmember([29,30],Dict,C).
neighbors(37,Dict,C):-lmember([30,31,36],Dict,C).
neighbors(38,Dict,C):-lmember([31,32,37],Dict,C).
neighbors(39,Dict,C):-lmember([32,33,38],Dict,C).
neighbors(40,Dict,C):-lmember([33,34,39],Dict,C).
neighbors(41,Dict,C):-lmember([34,35,40],Dict,C).
neighbors(42,Dict,C):-lmember([36,37],Dict,C).
neighbors(43,Dict,C):-lmember([37,38,42],Dict,C).
neighbors(44,Dict,C):-lmember([38,39,43],Dict,C).
neighbors(45,Dict,C):-lmember([39,40,44],Dict,C).
neighbors(46,Dict,C):-lmember([40,41,45],Dict,C).
neighbors(47,Dict,C):-lmember([42,43],Dict,C).
neighbors(48,Dict,C):-lmember([43,44,47],Dict,C).
neighbors(49,Dict,C):-lmember([44,45,48],Dict,C).
neighbors(50,Dict,C):-lmember([45,46,49],Dict,C).
neighbors(51,Dict,C):-lmember([47,48],Dict,C).
neighbors(52,Dict,C):-lmember([48,49,51],Dict,C).
neighbors(53,Dict,C):-lmember([49,50,52],Dict,C).
neighbors(54,Dict,C):-lmember([51,52],Dict,C).
neighbors(55,Dict,C):-lmember([52,53,54],Dict,C).
neighbors(56,Dict,C):-lmember([54,55],Dict,C).
neighbors(57,Dict,C):-lmember([2,12],Dict,C).
neighbors(58,Dict,C):-lmember([12,21,57],Dict,C).
neighbors(59,Dict,C):-lmember([21,29,58],Dict,C).
neighbors(60,Dict,C):-lmember([29,36,59],Dict,C).
neighbors(61,Dict,C):-lmember([36,42,60],Dict,C).
neighbors(62,Dict,C):-lmember([42,47,61],Dict,C).
neighbors(63,Dict,C):-lmember([47,51,62],Dict,C).
neighbors(64,Dict,C):-lmember([51,54,63],Dict,C).
neighbors(65,Dict,C):-lmember([54,56,64],Dict,C).
neighbors(66,Dict,C):-lmember([55,56,65],Dict,C).
neighbors(67,Dict,C):-lmember([53,55,66],Dict,C).
neighbors(68,Dict,C):-lmember([50,53,67],Dict,C).
neighbors(69,Dict,C):-lmember([46,50,68],Dict,C).
neighbors(70,Dict,C):-lmember([41,46,69],Dict,C).
neighbors(71,Dict,C):-lmember([35,41,70],Dict,C).
neighbors(72,Dict,C):-lmember([28,35,71],Dict,C).
neighbors(73,Dict,C):-lmember([1,20,28,72],Dict,C).
neighbors(74,Dict,C):-lmember([64,65,66,67],Dict,C).
neighbors(75,Dict,C):-lmember([63,64,74],Dict,C).
neighbors(76,Dict,C):-lmember([67,68,74,75],Dict,C).
neighbors(77,Dict,C):-lmember([62,63,75],Dict,C).
neighbors(78,Dict,C):-lmember([75,76,77],Dict,C).
neighbors(79,Dict,C):-lmember([68,69,76,78],Dict,C).
neighbors(80,Dict,C):-lmember([61,62,77],Dict,C).
neighbors(81,Dict,C):-lmember([77,78,80],Dict,C).
neighbors(82,Dict,C):-lmember([78,79,81],Dict,C).
neighbors(83,Dict,C):-lmember([69,70,79,82],Dict,C).
neighbors(84,Dict,C):-lmember([60,61,80],Dict,C).
neighbors(85,Dict,C):-lmember([80,81,84],Dict,C).
neighbors(86,Dict,C):-lmember([81,82,85],Dict,C).
neighbors(87,Dict,C):-lmember([82,83,86],Dict,C).
neighbors(88,Dict,C):-lmember([70,71,83,87],Dict,C).
neighbors(89,Dict,C):-lmember([59,60,84],Dict,C).
neighbors(90,Dict,C):-lmember([84,85,89],Dict,C).
neighbors(91,Dict,C):-lmember([85,86,90],Dict,C).
neighbors(92,Dict,C):-lmember([86,87,91],Dict,C).
neighbors(93,Dict,C):-lmember([87,88,92],Dict,C).
neighbors(94,Dict,C):-lmember([71,72,88,93],Dict,C).
neighbors(95,Dict,C):-lmember([58,59,89],Dict,C).
neighbors(96,Dict,C):-lmember([89,90,95],Dict,C).
neighbors(97,Dict,C):-lmember([90,91,96],Dict,C).
neighbors(98,Dict,C):-lmember([91,92,97],Dict,C).
neighbors(99,Dict,C):-lmember([92,93,98],Dict,C).
neighbors(100,Dict,C):-lmember([93,94,99],Dict,C).
neighbors(101,Dict,C):-lmember([72,73,94,100],Dict,C).
neighbors(102,Dict,C):-lmember([2,57,58,95],Dict,C).
neighbors(103,Dict,C):-lmember([2,95,96,102],Dict,C).
neighbors(104,Dict,C):-lmember([2,96,97,103],Dict,C).
neighbors(105,Dict,C):-lmember([2,97,98,104],Dict,C).
neighbors(106,Dict,C):-lmember([2,98,99,105],Dict,C).
neighbors(107,Dict,C):-lmember([99,100,106],Dict,C).
neighbors(108,Dict,C):-lmember([100,101,107],Dict,C).
neighbors(109,Dict,C):-lmember([1,73,101,108],Dict,C).
neighbors(110,Dict,C):-lmember([1,2,106,107,108,109],Dict,C).
