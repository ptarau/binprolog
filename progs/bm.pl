bm:-bm1,bm2,bm(pereira,[pbench]),halt.

bm1:-
 bm(gc_and_speed,[nrev,boyer,tsp,tak,allperms,bfmeta,gc,choice]).

bm2:-
 All0=[differen,q8,cnrev,fibo,
         lknight,qrev,cal,color,fknight,lat_plan,maxlist,primes,qsort,war,
         chat,cube,fq8,lat_wam,money,puzzle,subset,assertbm],
 sort(All0,All),
 println(All),
 bm(all,All).

bm(Topic,Names):-
  println(testing(Topic)+Names),
  member(Name,Names),
  println(running(Name)),
  enter(Name,go),
  fail.
bm(Topic,_):-
  println(end(Topic)).

bug:-
  enter(nrev,go).
  
bug1:-
  enter(nrev,bug).

