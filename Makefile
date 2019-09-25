SRCS = pass*.el
TEST_ERT_FILES=$(wildcard test/*.el)


LOAD_PATH = -L .

EMACSBIN ?= emacs
BATCH     = $(EMACSBIN) -Q --batch $(LOAD_PATH) \
		--eval "(require 'package)" \
		--eval "(add-to-list 'package-archives '(\"melpa-stable\" . \"http://stable.melpa.org/packages/\"))" \
		--funcall package-initialize

.PHONY: all clean dependencies check test lint

all: test

dependencies:
	# Install dependencies in ~/.emacs.d/elpa
	$(BATCH) \
	--eval "(progn (setq package-check-signature nil) (package-refresh-contents))" \
	--eval "(package-install 'ert)" \
	--eval "(package-install 'password-store)"

test:
	TRAVIS=true $(BATCH) \
	--eval "(progn $(patsubst %,(load-file \"%\"),$(filter-out %-autoloads.el,${TEST_ERT_FILES})) (ert-run-tests-batch-and-exit))"
