#! /bin/sh
## $Header$
##

PATH="/usr/bin:/bin"

make test.cma

rlwrap=rlwrap
if [ x"$EMACS" != x ]; then
	rlwrap=''
fi

. ./sh-vars

$rlwrap ocaml -init testInit.ml \
	$cmdline_byte_libs \
	test.cma
