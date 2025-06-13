.PHONY: api.watch
api.watch:
	cd arkham-api && \
	stack build --test --no-run-tests --pedantic --fast arkham-api \
	  --ghc-options "-DDEVELOPMENT $(GHC_OPTIONS)" \
	  --interleaved-output \
	  --file-watch \
		$(EXTRA_STACK_FLAGS) \
	  --exec "bash -c \"pkill arkham-api; DEVELOPMENT=true stack exec arkham-api &\"" \

.PHONY: api.watch.test
api.watch.test:
	cd arkham-api && \
	stack build --test --pedantic --fast arkham-api \
	  --ghc-options "-DDEVELOPMENT $(GHC_OPTIONS)" \
	  --interleaved-output \
	  --file-watch \
		$(EXTRA_STACK_FLAGS) \
	  --exec "bash -c \"pkill arkham-api; DEVELOPMENT=true stack exec arkham-api &\"" \

.PHONY: api.prof
api.prof:
	cd cards-discover && \
	stack build --profile --test --no-run-tests --fast cards-discover \
	  --ghc-options "-DDEVELOPMENT $(GHC_OPTIONS)" \
		$(EXTRA_STACK_FLAGS)
	cd arkham-api && \
	stack build --profile --test --no-run-tests --fast arkham-api \
	  --ghc-options "-DDEVELOPMENT $(GHC_OPTIONS)" \
	  --interleaved-output \
	  --file-watch \
		$(EXTRA_STACK_FLAGS) \
	  --exec "bash -c \"pkill arkham-api; DEVELOPMENT=true stack exec --profile -- arkham-api +RTS -p &\"" \

## Generate a tags file for source-code navigation
.PHONY: tags
tags:
	stack exec -- fast-tags --qualified -Rv --nomerge .

.PHONY: ghci
ghci:
	stack ghci arkham-api:exe:arkham-api --test --no-run-tests \
	  --ghc-options "-DDEVELOPMENT $(GHC_OPTIONS)" \
	  --interleaved-output \
		$(EXTRA_STACK_FLAGS)

.PHONY: validate
validate:
	cd validate && \
	stack build --test --no-run-tests --pedantic --fast validate \
	  --ghc-options "-DDEVELOPMENT $(GHC_OPTIONS)" \
	  --interleaved-output \
		$(EXTRA_STACK_FLAGS)

.PHONY: validate.watch
validate.watch:
	cd validate && \
	stack build --test --no-run-tests --pedantic --fast validate \
	  --ghc-options "-DDEVELOPMENT $(GHC_OPTIONS)" \
	  --interleaved-output \
		--file-watch \
		$(EXTRA_STACK_FLAGS) \
		--exec "bash -c \"cd .. && DEVELOPMENT=true stack exec validate\"" \

.PHONY: revalidate
revalidate:
	cd arkham-api && \
	stack build --test --no-run-tests --pedantic --fast arkham-api \
	  --ghc-options "-DDEVELOPMENT $(GHC_OPTIONS)" \
	  --interleaved-output \
		$(EXTRA_STACK_FLAGS)
	stack clean validate && stack run validate

.PHONY: test
test:
	cd arkham-api && \
	stack build --pedantic --test --fast arkham-api \
	  --ghc-options "-DDEVELOPMENT $(GHC_OPTIONS)" \
	  --interleaved-output \
	  --file-watch \
		$(EXTRA_STACK_FLAGS)

.PHONY: data
data:
	cd data && \
	for lang in en it ko es zh; do \
	test $${lang} = "en" && prefix="" || prefix=$${lang}"."; \
	test $${lang} = "en" && postfix="" || postfix="_"$${lang}; \
	curl -L https://$${prefix}arkhamdb.com/api/public/cards/?encounter=1 | jq . > cards$${postfix}.json && \
	curl https://arkhamdb.com/api/public/packs/ | jq ".[].code" | prefix=$${prefix} postfix=$${postfix} xargs sh -c 'for arg in "$$@"; do mkdir -p $$arg; curl -L https://$${prefix}arkhamdb.com/api/public/cards/$$arg | jq . > $$arg/cards$${postfix}.json & done; wait'; done \

.PHONY: eventlog
eventlog:
	cd arkham-api && \
		nix run -f https://github.com/mpickering/eventlog2html/archive/master.tar.gz eventlog2html arkham-api.eventlog && \
		open arkham-api.eventlog.html
