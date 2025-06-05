<<<<<<< HEAD
.PHONY: clean build

build:
	chmod u+x build.sh
	cotocoa_mode=COTOCOA_DISABLED ./build.sh

requester:
	chmod u+x build.sh
	cotocoa_mode=COTOCOA_REQUESTER ./build.sh

worker:
	chmod u+x build.sh
	cotocoa_mode=COTOCOA_WORKER ./build.sh

clean:
	-make -C lib/mtarm clean
	-make -C lib/ohhelp/ohhelp-1.1.1 clean
	-make -C lib/psort clean
	-make -C lib/cotocoa/src -f Makefile.env clean
	-rm -rf build/
	-rm -rf bin/
	-rm -rf *.mod
=======
all:
	make -C requester/lib/cotocoa/src
	make -C coupler
	make -C requester requester
	make -C worker

clean:
	make -C coupler clean
	make -C requester clean
	make -C worker clean
>>>>>>> eedfb028bed20d0129d3822679aa2cd1dd44b085
