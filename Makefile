
EMACS ?= emacs
EMACSW ?= /usr/local/Cellar/emacs/24.5/Emacs.app/Contents/MacOS/Emacs
CASK ?= cask

all: compile

test-interactive: compile
	$(CASK) exec $(EMACSW) -Q -L . --eval "(require 'interleave)"

compile: clean-elc
	$(CASK) build

clean-elc:
	$(CASK) clean-elc
