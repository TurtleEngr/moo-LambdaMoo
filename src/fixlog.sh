
# $Header: /repo/public.cvs/app/lambda/src/fixlog.sh,v 1.1 2001/06/02 03:42:36 bruce Exp $
# The files in MOO-1.8.0p6 contain $Log and it needs to be moved down
# so that the '/*' is no duplicated.

awk '
/\$Log/ {
	print "/*"
	sub(/\/\*/," *")
}
{
	print $0
}
'
