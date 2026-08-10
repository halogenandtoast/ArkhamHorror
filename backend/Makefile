# Per-developer GHC options, appended after the ones in stack.yaml. Exporting
# GHC_OPTIONS in your shell overrides this default; the flags that actually
# bound memory (-c, and the absence of -fwrite-if-simplified-core) live in
# stack.yaml so they survive that override and apply to Docker and CI too.
#
# A cold build peaks at ~7.1G of RSS with these settings, down from ~11.3G, at
# the same speed (497s / 1939s CPU vs 555s / 2060s).
#
# Going below 5G is possible but NOT free -- it costs about a third of your wall
# time, because GHC then majors constantly to stay small:
#
#   GHC_OPTIONS="-j12 +RTS -A32m -n2m -qb0 -qg1 -I0 -F1.2 -RTS" make api.watch
#
# That lands at 4.81G peak but 650s wall (+31%) on an otherwise identical cold
# build of all 7196 targets. Use it when you are memory-bound, not by default.
# At that point the peak is the linker (3.7G), not GHC (0.9G), so squeezing the
# RTS further buys nothing.
#
GHC_OPTIONS ?= -j12 +RTS -A128m -n4m -qb0 -qg1 -I0 -RTS

.PHONY: core.watch
core.watch:
	cd arkham-api && \
	stack build --test --no-run-tests --pedantic --fast arkham-api \
	  --ghc-options "-DDEVELOPMENT $(GHC_OPTIONS)" \
	  --interleaved-output \
	  --file-watch \
		$(EXTRA_STACK_FLAGS)

.PHONY: api.watch
api.watch:
	cd arkham-api && \
	stack build --test --no-run-tests --pedantic --fast arkham-api \
	  --ghc-options "-DDEVELOPMENT $(GHC_OPTIONS)" \
	  --interleaved-output \
	  --file-watch \
		$(EXTRA_STACK_FLAGS) \
	  --exec "bash -c \"pkill arkham-api; DEVELOPMENT=true stack exec arkham-api &\"" \

## Like api.watch, but tees output into a size-capped rolling log
## (.claude/build.log at the repo root) so agents can read compile errors.
.PHONY: api.watch.log
api.watch.log:
	$(MAKE) api.watch 2>&1 | scripts/rolling-log.sh ../.claude/build.log

.PHONY: api.build
api.build:
	cd arkham-api && \
	stack build --test --no-run-tests --pedantic --fast arkham-api \
	  --ghc-options "-DDEVELOPMENT $(GHC_OPTIONS)" \
	  --interleaved-output \
		$(EXTRA_STACK_FLAGS)

.PHONY: api.watch.test
api.watch.test:
	cd arkham-api && \
	stack build --test --no-run-tests --pedantic --fast arkham-api \
	  --ghc-options "-DDEVELOPMENT $(GHC_OPTIONS)" \
	  --interleaved-output \
	  --file-watch \
		$(EXTRA_STACK_FLAGS) \
	  --exec "bash -c \"pkill arkham-api || true; DEVELOPMENT=true stack exec arkham-api & stack build --test --pedantic --fast arkham-api --ghc-options '-DDEVELOPMENT $(GHC_OPTIONS)' --interleaved-output $(EXTRA_STACK_FLAGS) || echo 'Tests failed; server left running.'\"" \

.PHONY: test
test:
	cd arkham-api && \
	stack build --test --fast arkham-api \
	  --ghc-options "-DDEVELOPMENT $(GHC_OPTIONS)" \
	  --interleaved-output \
		$(EXTRA_STACK_FLAGS)

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

.PHONY: test.watch
test.watch:
	cd arkham-api && \
	stack build --pedantic --test --fast arkham-api \
	  --ghc-options "-DDEVELOPMENT $(GHC_OPTIONS)" \
	  --interleaved-output \
	  --file-watch \
		$(EXTRA_STACK_FLAGS)

.PHONY: data
data:
	cd data && \
	for lang in en it ko es zh de fr uk pl ru pt; do \
	test $${lang} = "en" && prefix="" || prefix=$${lang}"."; \
	test $${lang} = "en" && postfix="" || postfix="_"$${lang}; \
	curl -L https://$${prefix}arkhamdb.com/api/public/cards/?encounter=1 | jq . > cards$${postfix}.json && \
	curl https://arkhamdb.com/api/public/packs/ | jq ".[].code" | prefix=$${prefix} postfix=$${postfix} xargs sh -c 'for arg in "$$@"; do mkdir -p $$arg; curl -L https://$${prefix}arkhamdb.com/api/public/cards/$$arg | jq . > $$arg/cards$${postfix}.json & done; wait'; done \

.PHONY: eventlog
eventlog:
	cd arkham-api && \
		nix run -f https://github.com/mpickering/eventlog2html/archive/master.tar.gz eventlog2html arkham-api.eventlog && \
		open arkham-api.eventlog.html

.PHONY: api.metrics.full
api.metrics.full:
	cd arkham-api && \
	stack clean --full && \
	stack build -j1 --force-dirty --no-run-tests arkham-api \
		--ghc-options "-Rghc-timing -ddump-timings -ddump-to-file +RTS -M128G -s -RTS" && \
	sh scripts/timings.sh


.PHONY: api.metrics.frontend
api.metrics.frontend:
	cd arkham-api && \
	stack clean --full && \
	stack build -j1 --force-dirty --no-run-tests arkham-api \
		--ghc-options "-O0 -Rghc-timing -ddump-timings -ddump-to-file +RTS -M128G -s -RTS" && \
	sh scripts/timings.sh
