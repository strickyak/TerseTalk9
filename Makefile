all:
	python genesis.py >&2
	cc -g -D'DEBUG' vm.c _generated.c -o tt
	:
	:
	./tt
	:
	:
	ci-l genesis.py Makefile vm.c vm.h
	sync; sync; sync

clean:
	rm -f tt _generated.*
