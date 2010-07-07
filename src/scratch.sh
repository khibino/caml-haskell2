#! /bin/sh

make clean scratch.cma

rlwrap=rlwrap
if [ x"$EMACS" != x ]; then
	rlwrap=''
fi

$rlwrap ocaml -init scratchInit.ml \
	-I +lablgtk2 lablgtk.cma \
	-I +ulex ulexing.cma \
	dynlink.cma \
	-I +camlp4 camlp4o.cma \
	unix.cma \
	str.cma \
	nums.cma \
	scratch.cma
