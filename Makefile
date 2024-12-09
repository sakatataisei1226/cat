all:
	make -C requester/lib/cotocoa/src
	make -C coupler
	make -C requester requester
	make -C worker

clean:
	make -C coupler clean
	make -C requester clean
	make -C worker clean
