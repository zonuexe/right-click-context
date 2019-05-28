CASK ?= cask
EMACS ?= emacs
ELS = right-click-context.el
AUTOLOADS = right-click-context-autoload.el
ELCS = $(ELS:.el=.elc)

.cask: Cask
	CASK

%.elc: %.el .cask
	$(EMACS) -Q -batch -L . --eval \
	"(let ((default-directory (expand-file-name \".cask\" default-directory))) \
           (normal-top-level-add-subdirs-to-load-path))" \
       -f batch-byte-compile $<

all: autoloads $(ELCS)

autoloads: $(AUTOLOADS)

$(AUTOLOADS): $(ELCS)
	$(EMACS) -Q -batch -L . --eval \
	"(progn \
           (require 'package) \
	   (package-generate-autoloads \"right-click-context\" default-directory))"

clean:
	-rm -f $(ELCS) $(AUTOLOADS)

clobber: clean
	-rm -rf .cask

.PHONY: all autoloads clean clobber
