# $Header: /repo/public.cvs/app/LambdaMOO/src/Makefile,v 1.2 2021/07/15 07:10:04 bruce Exp $

ropt = -Cav --delete

pkgs :
	echo "make pkgs" >>build-notes.txt
	sudo apt-get install flex bison gperf
	sudo ln -s /usr/bin/gperf /usr/bin/pgperf
	sudo apt-get install lib32gcc-7-dev
	sudo apt-get install g++-multilib libc6-dev-i386

unpack :
	echo "make unpack" >>build-notes.txt
	tar -xzf ../archive/LambdaMOO-1.8.0p6.tar.gz
	-rm -rf MOO
	mv MOO-1.8.0p6 MOO

fix :
	echo "make fix" >>build-notes.txt
	cd MOO; \
	for i in $$(find * -prune -type f); do \
		if [ $$i = "Makefile.in" ]; then continue; fi; \
		echo $$i; \
		../fixlog.sh <$$i >t.tmp; \
		mv t.tmp $$i; \
	done
	# edit MOO/Makefile and MOO/Makefile.in
	# Add '-m32' to 'CFLAGS'

build1 :
	echo "make build1" >>build-notes.txt
	cd MOO; sh configure
	cp patch/options.h MOO
	cd MOO; make

build2 :
	echo "make build2" >>build-notes.txt
	cd MOO; patch -l < ../patch/moo-1.8.0-pAS9.patch
	cd MOO; make

build3 :
	echo "make build3" >>build-notes.txt
	cd MOO; patch -l < ../patch/moo-1.8.0-pAS5.c.patch
	cd MOO; patch -l < ../patch/moo-1.8.0-pAS5.patch
	cd MOO; make

build4 : 
	echo "make build4" >>build-notes.txt
	cd MOO; patch -l < ../patch/moo-1.8.0-FUP.patch
	cd MOO; patch -l < ../patch/moo-1.8.0-files.c.patch
	cd MOO; patch -l < ../patch/moo-1.8.0-FUP.errfix.patch
	cd MOO; make

release-x86_64 :
	echo "make release-x86_64" >>build-notes.txt
	cat /etc/os-release >>build-notes.txt
	uname -a >>build-notes.txt
	echo 'cd MOO; mv moo moo-x86_64-ubuntu-18.04' >>build-notes.txt
	cd MOO; mv moo moo-x86_64-ubuntu-18.04
	cp -a MOO/moo-x86_64-ubuntu-18.04 ../release/bin
	# Note: this server has a PANIC with my educore.

release-i386 :
	echo "make release-i386" >>build-notes.txt
	cat /etc/os-release >>build-notes.txt
	uname -a >>build-notes.txt
	echo 'cd MOO; mv moo moo-i386-ubuntu-18.04' >>build-notes.txt
	cd MOO; mv moo moo-i386-ubuntu-18.04
	cp -a MOO/moo-i386-ubuntu-18.04 ../release/bin
	# Note: this server works with my educore.
