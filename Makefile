all:
	python genesis.py >&2
	cc -Wall -Werror -D'DEBUG' -g vm.c _generated.c -o tt
	:
	:
	./tt
	:
	:
	ci-l genesis.py Makefile vm.c vm.h
	sync; sync; sync

clean:
	rm -f tt _generated.*
