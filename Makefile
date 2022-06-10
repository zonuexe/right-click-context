EASK ?= eask
EMACS ?= emacs

package:
	@echo "Packaging..."
	$(EASK) package

install:
	@echo "Installing..."
	$(EASK) install

compile:
	@echo "Compiling..."
	$(EASK) compile

all: autoloads

autoloads:
	$(EASK) autoloads

clean:
	$(EASK) clean-elc
	-rm -f $(AUTOLOADS)

clobber: clean
	$(EASK) clean

.PHONY: all autoloads clean clobber

ci: clean package install compile
