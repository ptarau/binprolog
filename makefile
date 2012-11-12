all:
	cd src ; make nobp
	cd src ; make realclean
	@echo executable: bin/bp
	./bin/bp
