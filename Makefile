# -*- Makefile -*-

#####################################################################
# Helper Macros

# Extract trailed page number from FILE-NAME:
#  $(call pagenum,fig1.pdf) -> 1
#
pagenum = $(shell echo $(basename $1) | sed 's/^[^0-9]*//')

# Add PAGE-NUMBER to FILE-NAME:
#   $(call addpage,fig.pdf,1) -> fig1.pdf
# if SUFFIX is set as the third param, it replaces the original suffix:
#   $(call addpage,fig.pdf,1,xbb) -> fig1.xbb
#
addpage = $(basename $(1))$(2)$(if $(3),.$(3),$(suffix $(1)))

# Find suitable command from COMMAND-LIST:
#   $(call findcmd, open xdg-open) -> /usr/bin/open (Mac)
#   $(call findcmd, open xdg-open) -> /usr/bin/xdg-open (Linux)
#
findcmd = $(firstword $(foreach c, $(1), $(shell command -v $(c))))

################################################################
## Use Emacs.app if available and anothr EMACS is not specified by user

COCOA_EMACS := /Applications/Emacs.app/Contents/MacOS/Emacs

ifneq ("$(wildcard $(COCOA_EMACS))", "")
	EMACS ?= $(COCOA_EMACS)
else
	EMACS ?= emacs
endif

################################################################
## Use cask if available and another CASK is not specified by user

CASK_VERSION := $(shell EMACS="$(EMACS)" cask --version 2>/dev/null)

ifdef CASK_VERSION
	CASK ?= cask
endif

ifdef CASK
	CASK_CMD ?= exec
	CASK_INSTALL ?= install
endif

################################################################
## Select open command

OPEN = $(call findcmd, open xdg-open acroread)

################################################################
## compile flags and options

FLAGS  = -Q -batch -l scripts/org-publish-all.el
OPTS   =

publish:
	$(CASK) $(CASK_CMD) $(EMACS) $(FLAGS) -f make-all $(OPTS) 2>&1 | \
	egrep -v '^(OVERVIEW|Loading|\(No changes|Tangled|Skipping)'

open:
	$(OPEN) html/index.html

clean:
	-rm -rf html/*
	-rm org/dyn/*/*
	-rm ${HOME}/.org-timestamps/current-*
	-rm -f *.{aux,dvi,log}

check-link:
	./scripts/org-link-check.rb org

# + Standard directory layout for a project ::
#   + org/       ... project top
#     + *.org    ... org files
#     + dat/     ... static attachments linked from *.org
#     + dyn/     ... dinamically generated files from *.org
#     + pub/     ... files only needed on publishing
#       + css/   ... style sheets
#       + top/   ... top-level dot files such as.htaccess
#     + options/ ... org-mode options (not copied on publish)
#   + html/      ... destination to publish
setup:
	$(CASK) $(CASK_INSTALL)
	-mkdir -p org/dat
	-mkdir -p org/dyn
	-mkdir -p org/options
	-mkdir -p org/pub/css
	-mkdir -p org/pub/top
	-mkdir -p html
	-touch org/pub/top/.htaccess
