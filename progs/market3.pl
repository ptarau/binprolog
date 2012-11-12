% stock market with random ticker

:-interactive(no).


% interface

% stock(Name,Price),
% cap(Stock,Price),
% volume(Stock,Price)
% has(Agent,Stock,Qty),
% capital(Agent,Value).

go:-
  Variation=0.09, % max up or down movement at each tick
  Direction=0.001, % growing slowly ...
  SleepTime=0, % put 1..5 seconds - to slow it down
  bg(new_ticker(Variation, Direction, SleepTime)), % starts bg ticker
  make_market, % puts a number of stocks on the market
  start_agents, % runs the trading agents
  sleep(2), % lets the market run for 3 seconds
  synchronize((nl,println('Market closed'),nl,show)),
  halt.     % exits the program

make_market:-
  put_stock(lcos,32),
  put_stock(xcit,40.5),
  put_stock(dell,69),
  put_stock(yhoo,120),
  put_stock(inkt,82).


start_agents:-
  new_agent(jBond,25000),
  new_agent(mMonroe,25000),
  run_agent(jBond,[
   buy_limit(lcos,31,300),
   buy_limit(xcit,40,100),
   buy_limit(dell,69,60),
   buy_limit(yhoo,121,50),
   buy_limit(inkt,81.75,80),
   sell_limit(lcos,33,300),
   sell_limit(xcit,41,100),
   sell_limit(dell,70,60),
   sell_limit(yhoo,130,50),
   sell_limit(inkt,83,80)
  ]),
  run_agent(mMonroe,[
   buy_market(lcos,300),
   buy_market(xcit,100),
   buy_market(dell,60),
   buy_stop(yhoo,125,50),
   buy_stop(inkt,85,30),
   sell_stop(lcos,25,300),
   sell_stop(xcit,30,100),
   sell_limit(dell,70,60),
   sell_limit(yhoo,130,50),
   sell_limit(inkt,83,80)
  ]). 
  

new_agent(Name,Capital):-nonvar(Name),number(Capital),
  local_out(capital(Name,Capital)).

run_agent(Name,Todo):-forall(member(Op,Todo),bg(call(Op,Name))).

% Delta = scale of +/- change and 
% N = sleeping interval in sec
new_ticker(Variation,Direction,N):-
  repeat,
    get_stock(Name,true,Base),
    Scale is Base*Variation,
    scale_random_val(Scale,Base,Val1),
	Half is Scale/2,
    Val2 is Val1-Half,
	Val is Val2 * (1+Direction),
    max(0.1,Val,NewVal),
    put_stock(Name,NewVal),
    synchronize((println(ticker(stock(Name),price(NewVal))),nl)),
    sleep(N),
  fail.

scale_random_val(Scale,Base,Random):-
  random(R),X is (R mod 1000)/1000,
  Random is Scale*X+Base.

max(A,B,M):-A<B,!,M=B.
max(A,_,A).

put_stock(Name,Val):-
  notify_about(stock(Name,Val)).

get_stock(Name,Cond,Val):-
  wait_for(stock(Name,Val),Cond).

watch_stock(Name,Price):-
  get_stock(Name,true,Price),
  put_stock(Name,Price).

buy_market(Name,Qty,Agent):-
  watch_stock(Name,Price),
  buy(Agent,Name,Price,Qty).

buy_limit(Name,Limit,Qty,Agent):-
  get_stock(Name,(Price=<Limit),Price),
  put_stock(Name,Price),
  buy(Agent,Name,Price,Qty).

buy_stop(Name,Limit,Qty,Agent):-
  get_stock(Name,(Price>=Limit),Price),
  put_stock(Name,Price),
  buy(Agent,Name,Price,Qty).

sell_market(Name,Qty,Agent):-
  watch_stock(Name,Price),
  sell(Agent,Name,Price,Qty).

sell_limit(Name,Limit,Qty,Agent):-
  get_stock(Name,(Price>=Limit),Price),
  put_stock(Name,Price),
  sell(Agent,Name,Price,Qty).
   
sell_stop(Name,Limit,Qty,Agent):-
  get_stock(Name,(Price=<Limit),Price),
  put_stock(Name,Price),
  sell(Agent,Name,Price,Qty).

buy(Me,Stock,Price,Qty):-
    local_in(capital(Me,MyCapital)),    % operations
    Cost is Price*Qty,
    MyNewCapital is MyCapital-Cost,
    local_out(capital(Me,MyNewCapital)),
    if(local_cin(has(Me,Stock,OldQty)),
      NewQty is OldQty+Qty,
      eq(NewQty,Qty)
    ),
    local_out(has(Me,Stock,NewQty)),
	show(Me,buy(Stock,Price,Qty)).

sell(Me,Stock,Price,Qty):-
    local_in(capital(Me,MyCapital)),    % operations
    Cost is Price*Qty,
    MyNewCapital is MyCapital+Cost,
    local_out(capital(Me,MyNewCapital)),
    if(local_cin(has(Me,Stock,OldQty)),
      NewQty is OldQty-Qty,
      NewQty is -Qty % sell short
    ),
    local_out(has(Me,Stock,NewQty)),
	show(Me,sell(Stock,Price,Qty)).

show:-synchronize(show0).
show(Name,Action):-synchronize(show0(Name,Action)).

show0(Name,Action):-
 println(Name=>Action),
 Capital=capital(Name,Cash),
 local_all(Capital,Cs),
 all_for(stock(_,_),StockPrices),
 member(Capital,Cs),
   local_all(has(Name,_,_),Holdings), 
   forall(member(X,Holdings),println(X)),
   compute_assets(Holdings,StockPrices,Val),
   Total is Cash+Val,
   println(capital=Cash),
   println(stocks=Val),
   println(total=Total),
 nl.

compute_assets(List,StockPrices,Val):-
   compute_assets(List,StockPrices,0,Val).

compute_assets([],_,SoFar,SoFar).
compute_assets([has(_,Stock,Qty)|Hs],StockPrices,SoFar,Val):-
  member(stock(Stock,Price),StockPrices),!,
  Temp is SoFar+Price*Qty,
  compute_assets(Hs,StockPrices,Temp,Val).

show0:-all_for(stock(_,_),Xs),member(X,Xs),println(current(X)),fail.
show0:-nl,local_all(Name,capital(Name,_),Ns),member(N,Ns),show0(N,showing),fail.
show0.

clean(X):-local_all(X,Xs),forall(member(X,Xs),cin(X)).

clean:-
  clean(has(_,_,_)),
  clean(capital(_,_)).

