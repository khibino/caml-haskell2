#! /bin/sh
## $Header$
##

PATH="/usr/bin:/bin"

#make realclean scratch.cma
make scratch.cma

rlwrap=rlwrap
if [ x"$EMACS" != x ]; then
	rlwrap=''
fi

. ./sh-vars

$rlwrap ocaml -init scratchInit.ml \
	$cmdline_byte_libs \
	scratch.cma
