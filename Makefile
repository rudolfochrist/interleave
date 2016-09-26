
CASK ?= cask
EMACSD-LOCAL = $(PWD)/emacs.d/
DIST = $(PWD)/dist/

all: compile

.PHONY: test-interactive
test-interactive: package
	$(CASK) emacs -Q \
	--eval "(setq emacs-user-directory \"$(EMACSD-LOCAL)\")" \
	--eval "(package-install-file \"$(DIST)\")"

.PHONY: compile
compile: clean-elc
	$(CASK) build

.PHONY: package
package: compile
	$(CASK) package

.PHONY: clean-elc
clean-elc:
	$(CASK) clean-elc

.PHONY: clean
clean: clean-elc
	-rm -rf dist/
	-rm *.*\~

