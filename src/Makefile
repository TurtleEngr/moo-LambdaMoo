# $Header: /data/public.cvs/app/lambda/src/Makefile,v 1.3 2001/06/11 05:51:12 bruce Exp $

ropt = -Cav --delete

build :
	cd MOO; make
	-mkdir -p ../release/bin ../release/doc 2>/dev/null
	rsync $(ropt) MOO/moo script/* ../release/bin
	rsync $(ropt) doc/* ../release/doc

release :
	date -u >>release.ver
	echo 'rel-$$Revision$$' >>release.ver
	-cvs add -kv release.ver
	-cd ../release; cvs add -kb *
	-cd ../release/bin; cvs add -kb *
	-cd ../release/bin/moo; cvs add -kb *
	-cd ../release/doc; cvs add -kb *
	-cd ../release/doc/prog; cvs add -kb *
	-cd ../release/doc/prog-1; cvs add -kb *
	cvs commit -m "Updated"
	-make build
	cd ..; cvs commit -m "Updated"
	cd ..; cvs tag $$(tail -n 1 src/release.ver | tr '.' '-')

# -------------------------
# Checking in a new version:

unpack :
	tar -xzf ../archive/LambdaMOO-1.8.0p6.tar.gz
	-rm -rf MOO
	mv MOO-1.8.0p6 MOO

fix :
	cd MOO; \
	for i in $$(find * -prune -type f); do \
		if [ $$i = "Makefile.in" ]; then continue; fi; \
		echo $$i; \
		../fixlog.sh <$$i >t.tmp; \
		mv t.tmp $$i; \
	done

import :
	export CVSROOT=$$(cat CVS/Root); \
	export CVS_RSH=ssh; \
	cd MOO; \
	cvs import -m "LambdaMOO-1.8.0p6.tar.gz" app/lambda/src/MOO lambda base-1-8-0p6

# cd MOO
# sh configure
# cvs update
# cvs add *
# cvs commit -m "After configure"
# edit options.h
# make
# Add patches (or merge with the latest CVS version)
