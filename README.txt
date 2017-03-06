WebParser - a web interface for the TRIPS parser
2017-02-23
William de Beaumont

Quick Start
-----------
This assumes that you already have a TRIPS system runnable on your machine, and
a web server (preferably apache) running and configured to allow CGI scripts to
run.

A few variables are used below. $TRIPS_BASE is the directory that the src/
directory you got from CVS is in. $system is the name of the TRIPS system (e.g.
STEP). $subdir is the subdirectory of the web directory that you want to
install to.

To make a TRIPS system usable via this web interface, you need to get this
directory from CVS (which if you are reading this, you probably already have):

  cd $TRIPS_BASE/src/
  cvs update -d WebParser

If your TRIPS system doesn't already require Graphviz, you need to get its
config directory and re-run configure after installing the Graphviz package:

  cd $TRIPS_BASE/src/config/
  cvs update -d Graphviz
  cd ..
  ./configure [with the same options you used before]

If this system doesn't already have it, you need to add code like this to its
$TRIPS_BASE/src/Systems/$system/system.lisp, between the calls to
def-trips-system and load-trips-system (this is from the STEP system):

  ;; add WebParser to the system when we have its source directory
  (when (probe-file #!TRIPS"src;WebParser")
    (nconc (assoc :step trips::*trips-systems*)
	   (list '(:dfc-component :webparser #!TRIPS"src;WebParser;"))))

Note that you need to change ":step" above to the name of the system used in
the call to def-trips-system.

Then you must remake the lisp image to include the WebParser component:

  cd $TRIPS_BASE/src/Systems/$system/
  make clean
  make install

And install files to the web directory. The default is $TRIPS_BASE/www/, but
you can symlink it to your webserver's actual web directory, or a subdirectory.
On Mac OS, it's usually /Library/WebServer/Documents/. On Linux it might be
/var/www/htdocs/. Check your web server's documentation and configuration. But
for example on Mac OS you might want to do this to create a suitable www
symlink:

  cd /Library/WebServer/Documents/
  sudo mkdir $subdir
  sudo chown $USER $subdir
  cd $TRIPS_BASE/
  ln -s /Library/WebServer/Documents/$subdir www

To actually install the files:

  cd $TRIPS_BASE/src/WebParser/
  make -f Makefile-component install

Then you can run the trips-$system binary, and go to this URL to use the web
interface:

  http://localhost/$subdir/$system

Code Guide
----------
To understand the code in this directory, it helps to know the history:

2006-2010
---------
The web parser was hosted only on the URCS web server, where I didn't have
permission to run a persistent TRIPS instance, only a CGI script for each
request. So that CGI script only ran the Parser itself, not the Facilitator or
anything that connects through it. In addition, there was no XML output, only
HTML. For a single request, control flowed like this:

- The webserver calls parserinterface.pl, which prints the first part of the
  HTML page, and checks that the query has an input parameter (if not it just
  prints the rest of the page with no parser output).
- Then parserinterface.pl calls web-parser.cgi, which loads a Lisp image dumped
  from web-parser.lisp.
- web-parser.lisp itself behaves as a CGI script (there was a brief period at
  the beginning where we didn't have parserinterface.pl, and web-parser.html
  was used as the input form), so it gets the input parameter, and calls the
  TRIPS Parser.
- web-parser.lisp prints its output to stdout, and parserinterface.pl captures
  it. parserinterface.pl then transforms the output, colorizing IDs, linkifying
  words and LF types (now ONT types), and formatting the parse tree in a few
  different ways (which the user can later select among). For SVG format,
  parserinterface.pl converts the parse tree to Graphviz dot format, calls dot
  to convert it to SVG, and includes that output in the page.
- The webserver delivers the output of parserinterface.pl to the user's web
  browser, and JavaScript code included in the page handles selecting a version
  of the parse tree to display.

2010-
-----
Still only on the URCS web server, we switched to outputting XML directly from
the Lisp process, and letting the user's web browser transform that to an HTML
page using XSL transforms. So we stopped using parserinterface.pl,
web-parser.{cgi,lisp} became web-parser-xml.{cgi,lisp}, and several lisp files
were added to go from Lisp to XML:

- lf-to-rdf.lisp converts the Lisp LF terms to an RDF/XML graph.
- tree-to-xml.lisp converts the Lisp parse tree to XML.
- parse-to-xml.lisp wraps both in a complete XML document.
- Later on, xml.lisp was factored out to handle general XML formatting.

make-dtd.lisp was also added to automatically make a DTD schema for the new XML
output based on current sets of POS, phrase categories, and semantic role
labels.

To go from XML to HTML, several corresponding XSL transforms were added:

- lf-to-dot.xsl converts the LF RDF/XML to Graphviz dot.
- lf-to-html.xsl converts it to HTML that displays the Lisp format similar to
  how parserinterface.pl used to do it.
- tree-to-dot.xsl converts the parse tree XML to Graphviz dot.
- tree-to-LinGO.xsl converts it to a LinGO-like table.
- tree-to-lisp.xsl converts it back to (simplified) Lisp format.
- parser-interface.xsl uses the above to convert the whole document, and
  includes the input form and interface controls.
- Later on, parser-interface.css and parser-interface.js were factored out of
  parser-interface.xsl.
- The exslt/ directory contains a couple of XSL templates from the EXSLT
  project that are used in the other XSL code. See http://exslt.org/.

Since the conversion to Graphviz dot format is now happening on the client
side, another CGI script was necessary to run dot and convert it to SVG:
dot-to-svg.pl. Later on this was changed to dot-to-svg.pl.in, and the Makefile
replaces a few configuration variables to make dot-to-svg.pl.

This version is still used on URCS, and it is what you get if you use the main
Makefile.

2014-
-----
In order to support running more components than just the Parser (notably
TextTagger, and later IM and DrumGUI), we moved to hosting the web parser(s) at
IHMC, where we could run persistent TRIPS instances. The following Lisp files
were added to turn WebParser into a real TRIPS component:

- defsys.lisp defines the component.
- messages.lisp defines KQML message handlers, including one for HTTP requests.
- utterance.lisp defines some data structures for tracking inputs that are
  either currently being parsed, or waiting to be parsed, and functions for
  doing that tracking, and sending input to and collecting results from other
  TRIPS components.

cgi-kqml-bridge.pl was also added, in order to translate CGI script calls from
the web server to KQML requests to the new WebParser component.

Since we still wanted to keep the standalone/URCS version of the web parser, a
separate Makefile was created to make the component/IHMC version of the web
parser: Makefile-component.

2015-
-----
After a brief attempt to support parsing input as if from different TRIPS
systems using the "genre" parameter in a single TRIPS instance, we instead
opted to run a separate TRIPS instance for each system. That included two
systems, STEP and DRUM, where we wanted to parse whole paragraphs instead of
just sentences. In addition, we wanted DRUM's web interface to be simplified,
with fewer options visible. So parser-interface.xsl was made to change its
behavior based on which TRIPS system was being used (mostly STEP vs. everything
else), and DRUM got its own separate version of parser-interface.xsl called
drum-interface.xsl (which actually uses parser-interface.xsl as a library).

The utterance structure in utterance.lisp became three: utterance, paragraph,
and their common parent, text-unit.

Another display format for the LF graph, AMR, was also added in this year, so
lf-to-amr.xsl was added to implement that.

For DRUM, we also added a new type of output: extractions.
- exts-to-table.xsl converts ekb XML (produced by DrumGUI) to an HTML table.
- exts-to-rdf.xsl converts it to RDF/XML so that lf-to-dot.xsl and
  dot-to-svg.pl can be reused to convert it to an SVG graph.
- ekb-to-dot.xsl uses exts-to-rdf.xsl and lf-to-dot.xsl in series. It isn't
  used in the live system, but is useful if you happen to have a raw .ekb file
  you want to visualize.

api.html was added to document the query parameters and XML output of the web
parser so that it could be used as a web API for other programs to use, not
just a web browser.

Another service was added: get-word-def. It has its own transform, word-def.xsl.

In 2016, install-cgi.pl was added to manage installing renamed copies of
cgi-kqml-bridge.pl, one for each service.

Summary
-------
So now you know where each file came from, how they fit into the various
versions of the web interface, and why some of them have slightly unintuitive
names. The following table lists each file and summarizes which versions it is
used in.

			pre-2010 URCS	post-2010 URCS	IHMC	DRUM
api.html				X		X	X
cgi-kqml-bridge.pl					X	X
defsys.lisp						X	X
dot-to-svg.pl.in			X		X	X
drum-interface.xsl						X
ekb-to-dot.xsl
exslt					X		X	X
exts-to-rdf.xsl							X
exts-to-table.xsl						X
install-cgi.pl						X	X
lf-to-amr.xsl				X		X	X
lf-to-dot.xsl				X		X	X
lf-to-html.xsl				X		X	X
lf-to-rdf.lisp				X		X	X
make-dtd.lisp				X		X	X
Makefile		X		X
Makefile-component					X	X
messages.lisp						X	X
parser-interface.css			X		X	X
parser-interface.js			X		X	X
parserinterface.pl	X
parser-interface.xsl			X		X	X
parse-to-xml.lisp			X		X	X
README.txt
tags-to-table.xsl					X	X
tags-to-xml.lisp					X	X
tree-to-dot.xsl				X		X	X
tree-to-LinGO.xsl			X		X	X
tree-to-lisp.xsl			X		X	X
tree-to-xml.lisp			X		X	X
utterance.lisp						X	X
web-parser.cgi		X
web-parser.html		X
web-parser.lisp		X
web-parser-xml.cgi			X
web-parser-xml.lisp			X
word-def.xsl						X	X
xml.lisp				X		X	X

