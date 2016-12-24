
CASK ?= cask
EMACSD-LOCAL = $(PWD)/emacs.d/

all: compile test

.PHONY: test-interactive
test-interactive: clean compile
	$(CASK) emacs -Q \
	-L $(PWD) \
	--eval "(setq emacs-user-directory \"$(EMACSD-LOCAL)\")" \
	--eval "(push '(width . 231) default-frame-alist)" \
	--eval "(push '(height . 58) default-frame-alist)" \
	--eval "(require 'interleave)" \

.PHONY: test
test:
	$(CASK) exec ecukes --win

.PHONY: compile
compile: clean-elc
	$(CASK) build

.PHONY: clean-elc
clean-elc:
	$(CASK) clean-elc

.PHONY: clean
clean: clean-elc
	-rm \#*
	-rm *.*\~

