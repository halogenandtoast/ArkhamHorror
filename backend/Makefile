.PHONY: api.watch
api.watch:
	cd arkham-api && \
	stack build --test --no-run-tests --fast arkham-api \
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

.PHONY: core
core:
	cd arkham-core && \
	stack build --pedantic --fast arkham-core \
	  --ghc-options "-DDEVELOPMENT $(GHC_OPTIONS)" \
	  --interleaved-output \
	  --file-watch \
		$(EXTRA_STACK_FLAGS)

.PHONY: core.ghci
core.ghci:
	stack ghci --test --no-run-tests arkham-core:lib \
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
	stack build --test --no-run-tests --fast validate \
	  --ghc-options "-DDEVELOPMENT $(GHC_OPTIONS)" \
	  --interleaved-output \
		--file-watch \
		$(EXTRA_STACK_FLAGS) \
		--exec "bash -c \"cd .. && DEVELOPMENT=true stack exec validate\"" \

.PHONY: revalidate
revalidate:
	cd arkham-core && \
	stack build --pedantic --fast arkham-core \
	  --ghc-options "-DDEVELOPMENT $(GHC_OPTIONS)" \
	  --interleaved-output \
		$(EXTRA_STACK_FLAGS)
	stack clean validate && stack run validate

.PHONY: test
test:
	cd arkham-core && \
	stack build --pedantic --test --fast arkham-core \
	  --ghc-options "-DDEVELOPMENT $(GHC_OPTIONS)" \
	  --interleaved-output \
	  --file-watch \
		$(EXTRA_STACK_FLAGS)

.PHONY: data
data:
	cd data && \
	curl https://arkhamdb.com/api/public/cards/?encounter=1 > cards.json && \
	curl https://arkhamdb.com/api/public/packs/ | jq ".[].code" | xargs sh -c 'for arg in "$$@"; do mkdir -p $$arg; curl https://arkhamdb.com/api/public/cards/$$arg > $$arg/cards.json & done; wait' && \
	for f in $$(find ./ -name "*json"); do echo $$f ; python3 -mjson.tool $$f > /tmp/1 && cp /tmp/1 $$f & done; wait \

.PHONY: eventlog
eventlog:
	cd arkham-api && \
		nix run -f https://github.com/mpickering/eventlog2html/archive/master.tar.gz eventlog2html arkham-api.eventlog && \
		open arkham-api.eventlog.html
