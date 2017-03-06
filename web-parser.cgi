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

# MacOSX/openmcl
LISP="/usr/local/bin/openmcl --batch --noinit --image-name ../etc/lisp/web-parser.image"
# Linux/cmucl
LISP="/u/www/research/trips/parser/cmucl/latest/bin/lisp -quiet -nositeinit -noinit -core ../etc/lisp/web-parser.image"

exec $LISP
