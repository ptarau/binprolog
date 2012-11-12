:-[lattice].
% planets & attributes: must be sorted

planet(earth,[moon,near,small]).
planet(jupiter,[far,large,moon]).
planet(mars,[moon,near,small]).
planet(mercury,[near,single,small]).
planet(neptune,[far,medium,moon]).
planet(pluto,[far,moon,small]).
planet(saturn,[far,large,moon]).
planet(uranus,[far,medium,moon]).
planet(venus,[near,single,small]).

lcontext(L,Rs):-planet(L,Rs).
