# integrates changes in lib.pl and builtins
make
../bin/bp "reboot,halt"
cp wam.bp wam.ok
make
make realclean


