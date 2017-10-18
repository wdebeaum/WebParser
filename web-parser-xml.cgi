#!/bin/sh
#
# web-parser.cgi
#
# George Ferguson, ferguson@cs.rochester.edu,  6 Jul 2006
# Time-stamp: <Thu Jul  6 16:11:10 EDT 2006 ferguson>
#
# This script simply invokes lisp with correct image file.
# The lisp code handles the CGI arguments itself.
#

# TODO make this use src/config/lisp/*/run-image.sh and TRIPS_BASE somehow

# MacOSX/openmcl
#LISP="/usr/local/bin/openmcl --batch --noinit --image-name ../etc/lisp/web-parser-xml.image"
# Linux/cmucl
#LISP="/u/www/research/trips/parser/cmucl/latest/bin/lisp -quiet -nositeinit -noinit -core ../etc/lisp/web-parser-xml.image"
# Linux/sbcl
LISP="/u/www/research/trips/parser/bin/lisp --noinform --core ../etc/lisp/web-parser-xml.image --end-runtime-options --noprint --end-toplevel-options"

perl -T -e 'print localtime() . "\t$ENV{REMOTE_ADDR}\t$ENV{QUERY_STRING}\n";' >>web-parser.log

exec $LISP
