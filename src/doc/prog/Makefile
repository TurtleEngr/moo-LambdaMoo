# $Header: /repo/public.cvs/app/lambda/src/doc/prog/Makefile,v 1.1 2001/10/11 05:10:55 bruce Exp $
# Makefile for maintaining the files in the $(mInsDir) directory

update :
	cvs -q update

commit :
	cvs -q commit
	cvs -q tag -F work-$$LOGNAME-$$(hostname -s)
	echo "Did you remember to 'cvs add' new files?"
