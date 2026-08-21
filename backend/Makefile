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

# Extra environment prepended to the dev server's exec line. Empty by default;
# api.watch.redis uses it to point the app at a Redis instance.
API_ENV ?=

# Where api.watch.redis looks for Redis. Any hedis-parseable URL works.
REDIS_CONN ?= redis://localhost:6379

# How the watch targets detect changes.
#
# Neither stack mode really works on this tree any more:
#
#   --file-watch opens one FSEventStream, CFRunLoop OS thread and pipe per
#   *directory* holding a watched file. Nesting cards under campaign/scenario
#   dirs took the library from 271 directories to 1256, and past roughly a
#   thousand streams macOS stops delivering events to the later ones. Only the
#   first couple of watches still fire, so editing a card does nothing while
#   editing stack.yaml or package.yaml still kicks a build.
#
#   --file-watch-poll instead stats every source file on a loop. With ~8k files
#   over 1331 directories that pegs a core permanently and starves stack itself:
#   the build finishes but --exec never runs, so the server never comes up.
#
# --file-watch is the less broken of the two, so it stays the default, but the
# working setups are api.watch.entr (entr drives the loop) or api.watch plus
# watch.poke in a second shell. Both are below.
#
# Note that `make -n` RUNS those targets instead of printing them, since their
# recipes mention $(MAKE) and make treats such lines as recursive.
FILE_WATCH ?= --file-watch

.PHONY: core.watch
core.watch:
	cd arkham-api && \
	stack build --test --no-run-tests --pedantic --fast arkham-api \
	  --ghc-options "-DDEVELOPMENT $(GHC_OPTIONS)" \
	  --interleaved-output \
	  $(FILE_WATCH) \
		$(EXTRA_STACK_FLAGS)

.PHONY: api.watch
api.watch:
	cd arkham-api && \
	stack build --test --no-run-tests --pedantic --fast arkham-api \
	  --ghc-options "-DDEVELOPMENT $(GHC_OPTIONS)" \
	  --interleaved-output \
	  $(FILE_WATCH) \
		$(EXTRA_STACK_FLAGS) \
	  --exec "bash -c \"pkill arkham-api; DEVELOPMENT=true $(API_ENV) stack exec arkham-api &\"" \

## Sidecar for api.watch, run in a second shell. stack no longer sees edits
## under library/ (see FILE_WATCH above), but its watch on this directory still
## fires, so watch the tree here -- one recursive kqueue watcher instead of 1331
## FSEvent streams -- and rewrite stack.yaml to poke stack into rebuilding.
## Rewriting picks up new card files too, since entr -d returns when one appears.
.PHONY: watch.poke
watch.poke:
	@command -v entr >/dev/null || { echo "watch.poke needs entr: brew install entr"; exit 1; }
	@while :; do \
	  find arkham-api/library arkham-api/app arkham-api/tests -name '*.hs' \
	    | entr -dnpz true >/dev/null 2>&1; \
	  cp stack.yaml .stack.yaml.poke && cat .stack.yaml.poke > stack.yaml; \
	  rm -f .stack.yaml.poke; \
	  echo "poked stack at $$(date +%T)"; \
	done

## Like api.watch, but tees output into a size-capped rolling log
## (.claude/build.log at the repo root) so agents can read compile errors.
.PHONY: api.watch.log
api.watch.log:
	$(MAKE) api.watch 2>&1 | scripts/rolling-log.sh ../.claude/build.log

## Like api.watch, but with Redis as the websocket message broker.
##
## Without REDIS_CONN the app builds a WebSocketBroker, where publishing a
## GameUpdate is a direct in-process broadcast to the room. Production always
## has Redis, so there the update instead makes a round trip out to Redis and
## back through a subscriber before it reaches any socket. That round trip is
## the part that can break on its own -- a half-open subscriber socket keeps
## delivering log lines (broadcast in-process) while every GameUpdate vanishes
## -- and the default local setup cannot reproduce it at all. Use this target
## when touching anything on the publish/subscribe path.
##
## Needs a Redis on $(REDIS_CONN); docker run --rm -p 6379:6379 redis:7-alpine
.PHONY: api.watch.redis
api.watch.redis: redis.check
	$(MAKE) api.watch API_ENV="REDIS_CONN=$(REDIS_CONN)"

## api.watch.redis + the rolling build log (see api.watch.log).
.PHONY: api.watch.redis.log
api.watch.redis.log: redis.check
	$(MAKE) api.watch.log API_ENV="REDIS_CONN=$(REDIS_CONN)"

## One build plus a server restart -- the unit api.watch.entr loops over.
.PHONY: api.rebuild
api.rebuild:
	cd arkham-api && \
	stack build --test --no-run-tests --pedantic --fast arkham-api \
	  --ghc-options "-DDEVELOPMENT $(GHC_OPTIONS)" \
	  --interleaved-output \
		$(EXTRA_STACK_FLAGS) \
	  --exec "bash -c \"pkill arkham-api; DEVELOPMENT=true $(API_ENV) stack exec arkham-api &\"" \

## api.watch with no stack watcher in the loop at all: entr holds one recursive
## kqueue watch over the tree and reruns api.rebuild per change. Trades a stack
## startup per change for not depending on --file-watch; watch.poke is the other
## way round, keeping stack resident and poking it. Pick whichever you prefer.
## entr -d returns when a new file appears, so the loop re-runs find and picks
## up new card modules.
.PHONY: api.watch.entr
api.watch.entr:
	@command -v entr >/dev/null || { echo "api.watch.entr needs entr: brew install entr"; exit 1; }
	@while :; do \
	  find arkham-api/library arkham-api/app arkham-api/tests -name '*.hs' \
	    | entr -dn $(MAKE) --no-print-directory api.rebuild; \
	done

## api.watch.entr + the rolling build log (see api.watch.log).
.PHONY: api.watch.entr.log
api.watch.entr.log:
	$(MAKE) api.watch.entr 2>&1 | scripts/rolling-log.sh ../.claude/build.log

## api.watch.entr with Redis as the websocket message broker (see api.watch.redis).
.PHONY: api.watch.entr.redis
api.watch.entr.redis: redis.check
	$(MAKE) api.watch.entr API_ENV="REDIS_CONN=$(REDIS_CONN)"

## Fail early with a useful message rather than letting checkedConnect throw.
## Skipped when redis-cli isn't installed, since absence of the CLI says
## nothing about whether a server is listening.
.PHONY: redis.check
redis.check:
	@if command -v redis-cli >/dev/null 2>&1; then \
	  redis-cli -u "$(REDIS_CONN)" ping >/dev/null 2>&1 || { \
	    echo "No Redis answering on $(REDIS_CONN)"; \
	    echo "Start one with: docker run --rm -p 6379:6379 redis:7-alpine"; \
	    echo "Or point elsewhere: make api.watch.redis REDIS_CONN=redis://host:port"; \
	    exit 1; \
	  }; \
	fi

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
	  $(FILE_WATCH) \
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
	  $(FILE_WATCH) \
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
		$(FILE_WATCH) \
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
	  $(FILE_WATCH) \
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
