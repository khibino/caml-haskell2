#! /bin/sh

make clean scratch.cma

rlwrap=rlwrap
if [ x"$EMACS" != x ]; then
	rlwrap=''
fi

$rlwrap ocaml -init scratchInit.ml \
	-I +ulex ulexing.cma \
	dynlink.cma \
	-I +camlp4 camlp4o.cma \
	-I +estring pa_estring.cmo \
	bigarray.cma \
	unix.cma \
	-I +camomile camomile.cma \
	str.cma \
	nums.cma \
	-I +threads threads.cma \
	-I +batteries batteries.cma \
	-I +cf cf.cma \
	scratch.cma
