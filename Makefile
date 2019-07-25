#
# Makefile for WebParser
#
# George Ferguson, ferguson@cs.rochester.edu, 6 Jul 2006
# Time-stamp: <Thu Jul  6 17:32:21 EDT 2006 ferguson>
#

WWWDIR ?= /u/www/research/trips/parser

CONFIGDIR=../config
include $(CONFIGDIR)/version.mk
include $(CONFIGDIR)/defs.mk
include $(CONFIGDIR)/Graphviz/defs.mk
include $(CONFIGDIR)/lisp/defs.mk
include $(CONFIGDIR)/lisp/$(LISP_FLAVOR)/defs.mk

# get rid of --no-userinit, because I use mine to make compilation less
# verbose, and I want it to still apply when cron sends me the output of
# update-web-tools.sh in an email -- wdebeaum
LISPCMD:=$(filter-out --no-userinit,$(LISPCMD))

all: web-parser-xml.image trips-parser-output.dtd

web-parser-xml.image: web-parser-xml.lisp parse-to-xml.lisp tree-to-xml.lisp lf-to-rdf.lisp
	$(call dump-image,web-parser-xml.lisp,web-parser-xml.image,webparser::run-cgi)

trips-parser-output.dtd: make-dtd.lisp
	case "$(LISP_FLAVOR)" in \
	  cmucl) \
	    $(LISPCMD) -load make-dtd.lisp -eval '(make-dtd)' -eval '(quit)' \
	    ;; \
	  sbcl|abcl) \
	    $(LISPCMD) --load make-dtd.lisp --eval '(make-dtd)' --eval '(quit)' \
	    ;; \
	  ccl|openmcl) \
	    $(LISPCMD) -l make-dtd.lisp -e '(make-dtd)' -e '(quit)' \
	    ;; \
	  allegro) \
	    $(LISPCMD) -L make-dtd.lisp -e '(make-dtd)' -e '(exit)' \
	    ;; \
	  clisp) \
	    $(LISPCMD) -i make-dtd.lisp -x '(make-dtd)' -x '(quit)' \
	    ;; \
	  *) \
	    echo "Unsupported lisp flavor '$(LISP_FLAVOR)'" \
	    exit 1 \
	    ;; \
	esac

install: install-cgi install-dtd install-style install-docs

install-cgi: dot-htaccess web-parser-xml.cgi dot-to-svg.pl install-image
	chmod a+x web-parser-xml.cgi dot-to-svg.pl
	touch $(WWWDIR)/cgi/web-parser.log
	chmod a+w $(WWWDIR)/cgi/web-parser.log
	$(INSTALL_DATA) dot-htaccess $(WWWDIR)/cgi/.htaccess
	$(INSTALL_PROGRAM) web-parser-xml.cgi $(WWWDIR)/cgi
	$(INSTALL_PROGRAM) dot-to-svg.pl $(WWWDIR)/cgi

dot-htaccess:
	( case `hostname` in \
	    *.cs.rochester.edu) ;; \
	    *) echo 'Options ExecCGI' ;; \
	  esac ; \
	  echo 'SetHandler cgi-script' \
	) >$@

dot-to-svg.pl: dot-to-svg.pl.in
	sed -e 's@DOT_LIB_DIR@$(DOT_LIB_DIR)@g' \
	    -e 's@DOT_BIN_DIR@$(DOT_BIN_DIR)@g' \
	    -e 's@DOT_TO_SVG_CMD@|dot -Tsvg@g' \
	    $< >$@

install-image: web-parser-xml.image
	$(INSTALL_DATA) web-parser-xml.image $(WWWDIR)/etc/lisp/

install-dtd: trips-parser-output.dtd
	$(INSTALL_DATA) trips-parser-output.dtd $(WWWDIR)/

install-docs: ../Parser/docs/LF\ Documentation.pdf api.html
	$(INSTALL_DATA) ../Parser/docs/LF\ Documentation.pdf $(WWWDIR)/
	$(INSTALL_DATA) api.html $(WWWDIR)/

install-style: parser-interface.xsl parser-interface.js parser-interface.css tree-to-lisp.xsl tree-to-LinGO.xsl tree-to-dot.xsl lf-to-html.xsl lf-to-amr.xsl lf-to-dot.xsl tags-to-table.xsl exts-to-table.xsl exslt/str.replace.template.xsl exslt/str.tokenize.template.xsl
	$(INSTALL_DATA) parser-interface.xsl $(WWWDIR)/style/
	$(INSTALL_DATA) parser-interface.js $(WWWDIR)/style/
	$(INSTALL_DATA) parser-interface.css $(WWWDIR)/style/
	$(INSTALL_DATA) tree-to-lisp.xsl $(WWWDIR)/style/
	$(INSTALL_DATA) tree-to-LinGO.xsl $(WWWDIR)/style/
	$(INSTALL_DATA) tree-to-dot.xsl $(WWWDIR)/style/
	$(INSTALL_DATA) lf-to-html.xsl $(WWWDIR)/style/
	$(INSTALL_DATA) lf-to-amr.xsl $(WWWDIR)/style/
	$(INSTALL_DATA) lf-to-dot.xsl $(WWWDIR)/style/
	$(INSTALL_DATA) tags-to-table.xsl $(WWWDIR)/style/
	$(INSTALL_DATA) exts-to-table.xsl $(WWWDIR)/style/
	$(INSTALL_DATA) exts-to-rdf.xsl $(WWWDIR)/style/
	$(INSTALL_DATA) exslt/str.replace.template.xsl $(WWWDIR)/style/
	$(INSTALL_DATA) exslt/str.tokenize.template.xsl $(WWWDIR)/style/

clean:
	rm -f web-parser-xml.image trips-parser-output.dtd dot-to-svg.pl dot-htaccess

