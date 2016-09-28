
CASK ?= cask
EMACSD-LOCAL = $(PWD)/emacs.d/

all: compile

.PHONY: test-interactive
test-interactive: clean compile
	$(CASK) emacs -Q \
	-L .
	--eval "(setq emacs-user-directory \"$(EMACSD-LOCAL)\")" \

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

