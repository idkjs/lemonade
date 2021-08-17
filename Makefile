### Makefile -- Project Lemonade

# Lemonade (https://github.com/michipili/lemonade)
# This file is part of Lemonade
#
# Copyright © 2013–2015 Michael Grünewald
#
# This file must be used under the terms of the CeCILL-B.
# This source file is licensed as described in the file COPYING, which
# you should have received as part of this distribution. The terms
# are also available at
# http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt

.DEFAULT_GOAL := help

project_name = lemonade
opam_file = $(project_name).opam
DUNE = opam exec -- dune

help:
	@echo "List of available make commands";
	@echo "";
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "  \033[36m%-15s\033[0m %s\n", $$1, $$2}';
	@echo "";

lock: ## Generate a lock file
	opam lock . -y

build: ## Build the app
	$(DUNE) build @all

watch: ## Build the app in watch mode
	$(DUNE) build @all -w

test: ## Run the tests
	$(DUNE) runtest

clean:
	@dune clean
	rm -rf doc.public

format: ## Format the code
	dune build @fmt --auto-promote

docs: clean build ## Build the documentation
	@dune build @doc
	mkdir -p doc.public
	cp -r _build/default/_doc/_html doc.public
### End of file `Makefile'
