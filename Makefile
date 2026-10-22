# Colors for displaying text in the terminal
RED    := $(shell tput -Txterm setaf 1)
GREEN  := $(shell tput -Txterm setaf 2)
YELLOW := $(shell tput -Txterm setaf 3)
WHITE  := $(shell tput -Txterm setaf 7)
BOLD   := $(shell tput -Txterm bold)
RESET  := $(shell tput -Txterm sgr0)

TARGET_MAX_CHAR_NUM=30

## Show help
help:
	@echo ''
	@echo 'Usage:'
	@echo '  ${YELLOW}make${RESET} ${GREEN}<target>${RESET}'
	@echo ''
	@echo 'Targets:'
	@awk '/^[a-zA-Z\-_0-9]+:/ { \
		helpMessage = match(lastLine, /^## (.*)/); \
		if (helpMessage) { \
			helpCommand = substr($$1, 0, index($$1, ":")-1); \
			helpMessage = substr(lastLine, RSTART + 3, RLENGTH); \
			printf "  ${YELLOW}%-$(TARGET_MAX_CHAR_NUM)s${RESET} ${GREEN}%s${RESET}\n", helpCommand, helpMessage; \
		} \
	} \
	{ lastLine = $$0 }' $(MAKEFILE_LIST)
.PHONY: help

V2_IMAGE       ?= halogenandtoast/arkham-horror
V2_KUBECONFIG  ?= $(CURDIR)/terraform/kubeconfig
V2_NAMESPACE   ?= arkham
V2_DEPLOYMENT  ?= arkham-web
V2_PLATFORM    ?= linux/amd64
# the sign-in cookie covers every subdomain, so 3ed.arkhamhorror.app shares it
V2_BUILD_ARGS  ?= --build-arg AUTH_COOKIE_DOMAIN=.arkhamhorror.app
V2_BUILDER     ?= arkham-multiarch
V2_BUILDKITD_CONFIG ?= $(CURDIR)/infra/buildkitd.toml
V2_BUILDKITD_FLAGS  ?= --allow-insecure-entitlement=network.host
V2_DO_CLUSTER  ?= arkham-horror-doks
V2_CACHE_DIR   ?= $(CURDIR)/.buildx-cache/v2
V2_CACHE_NEXT  ?= $(V2_CACHE_DIR).new
V2_CACHE_PREV  ?= $(V2_CACHE_DIR).prev
V2_ASSET_BUCKET ?= arkham-horror-assets
# A build that dies part-way leaves the .stack-work cache mounts half-written:
# valid .hi files beside truncated .o files that GHC never notices are stale.
# scripts/docker-build-api.sh repairs that on the next run, so one retry turns a
# permanently wedged deploy back into a working one. Anything not matching these
# signatures (a type error, say) fails immediately instead of building twice.
V2_CACHE_POISON = ZCMain_main_closure|ld returned 1 exit status|failed in phase .Linker.|file format not recognized|truncated

V2_CACHE_SETUP = CACHE_ARGS="--cache-to type=local,dest=$(V2_CACHE_NEXT),mode=max"; rm -rf "$(V2_CACHE_NEXT)"; if [ -d "$(V2_CACHE_DIR)" ]; then CACHE_ARGS="--cache-from type=local,src=$(V2_CACHE_DIR) $$CACHE_ARGS"; fi
# Uploads this build's content-hashed bundles to the CDN bucket, which is what
# lets any pod answer for any build's /assets/ (prod.nginxconf, @cdn_assets).
# Deliberately BEFORE the rollout: for the length of the rollout the backend pool
# holds both builds, so the old pods need the new bundles reachable before the
# first new pod starts handing out an index.html that names them.
V2_PUBLISH_ASSETS = ASSET_BUCKET="$(V2_ASSET_BUCKET)" V2_BUILDER="$(V2_BUILDER)" V2_CACHE_DIR="$(V2_CACHE_DIR)" BUILD_ARGS="$(V2_BUILD_ARGS)" ./scripts/publish-frontend-assets.sh

V2_CACHE_PROMOTE = if [ -d "$(V2_CACHE_NEXT)" ]; then rm -rf "$(V2_CACHE_PREV)"; if [ -d "$(V2_CACHE_DIR)" ]; then mv "$(V2_CACHE_DIR)" "$(V2_CACHE_PREV)"; fi; if mv "$(V2_CACHE_NEXT)" "$(V2_CACHE_DIR)"; then rm -rf "$(V2_CACHE_PREV)"; else if [ -d "$(V2_CACHE_PREV)" ]; then mv "$(V2_CACHE_PREV)" "$(V2_CACHE_DIR)"; fi; exit 1; fi; fi

# The builder MUST carry infra/buildkitd.toml. Without it BuildKit's default GC
# caps all `type==exec.cachemount` records at 512MB and chases an unreachable
# free-space target, so it continuously prunes the .stack-work caches the
# Dockerfile depends on and every deploy recompiles ~9,400 modules at -O2 from
# scratch (53-63 min instead of ~10). A builder created before that config
# existed keeps the defaults -- the config is only read at container creation --
# so warn loudly rather than silently deploying onto a cache that cannot hold.
## Ensure a docker-container buildx builder exists, with the right GC policy
v2-buildx-setup:
	@if ! docker buildx inspect $(V2_BUILDER) >/dev/null 2>&1; then \
	  echo ">> creating builder $(V2_BUILDER) with $(V2_BUILDKITD_CONFIG)"; \
	  docker buildx create --name $(V2_BUILDER) --driver docker-container \
	    --buildkitd-config $(V2_BUILDKITD_CONFIG) \
	    --buildkitd-flags '$(V2_BUILDKITD_FLAGS)' >/dev/null; \
	elif docker buildx inspect $(V2_BUILDER) 2>/dev/null | grep -q 'Max Used Space:[[:space:]]*488.3MiB'; then \
	  echo "$(YELLOW)>> WARNING: builder $(V2_BUILDER) is running BuildKit's DEFAULT GC policy.$(RESET)"; \
	  echo "$(YELLOW)>>   It caps the Haskell build caches at 512MB, so this deploy will$(RESET)"; \
	  echo "$(YELLOW)>>   recompile every module from scratch (expect ~1 hour, not ~10 min).$(RESET)"; \
	  echo "$(YELLOW)>>   Fix once with: make v2-buildx-recreate$(RESET)"; \
	fi
.PHONY: v2-buildx-setup

# BuildKit reads its config when the container is created, so there is no way to
# apply a changed GC policy to a running builder. This destroys the current
# build cache: the next deploy is cold, and every deploy after it is warm.
## Recreate the buildx builder so infra/buildkitd.toml takes effect (one cold build)
v2-buildx-recreate:
	@if docker buildx inspect $(V2_BUILDER) 2>/dev/null | grep -q 'Status:[[:space:]]*running' && \
	    docker buildx history ls --builder $(V2_BUILDER) 2>/dev/null | grep -q 'Running'; then \
	  echo "$(RED)>> a build is still running on $(V2_BUILDER) -- wait for it to finish$(RESET)"; exit 1; \
	fi
	@test -f $(V2_BUILDKITD_CONFIG) || { echo "$(RED)>> missing $(V2_BUILDKITD_CONFIG)$(RESET)"; exit 1; }
	@echo ">> removing builder $(V2_BUILDER) (discards its build cache)"
	@docker buildx rm $(V2_BUILDER) >/dev/null 2>&1 || true
	@docker buildx create --name $(V2_BUILDER) --driver docker-container \
	  --buildkitd-config $(V2_BUILDKITD_CONFIG) \
	  --buildkitd-flags '$(V2_BUILDKITD_FLAGS)' >/dev/null
	@docker buildx inspect --bootstrap $(V2_BUILDER) >/dev/null
	@echo ">> builder recreated; GC policy now:"
	@docker buildx inspect $(V2_BUILDER) | sed -n '/GC Policy/,$$p'
.PHONY: v2-buildx-recreate

## Show what the builder is keeping (cache mounts are the ones that matter)
v2-buildx-du:
	@docker buildx du --builder $(V2_BUILDER) --verbose 2>/dev/null | awk '\
	  /^ID:/{id=$$2} /^Size:/{size=$$2} /^Usage count:/{uc=$$3} \
	  /^Description:/{sub(/^Description:[ \t]*/,"");d=$$0} \
	  /^Type:.*cachemount/{printf "  %-10s uses=%-3s %s\n", size, uc, substr(d,index(d,"with id")+8)}'
	@docker buildx du --builder $(V2_BUILDER) 2>/dev/null | tail -2
.PHONY: v2-buildx-du

## Ensure kubeconfig exists and its token is still valid (DO tokens expire ~7 days)
v2-kubeconfig-ensure:
	@command -v kubectl >/dev/null || { echo "kubectl not found"; exit 1; }
	@if [ ! -f $(V2_KUBECONFIG) ] || ! KUBECONFIG=$(V2_KUBECONFIG) kubectl get ns $(V2_NAMESPACE) --request-timeout=5s >/dev/null 2>&1; then \
	  echo ">> kubeconfig missing or stale — refreshing via doctl ($(V2_DO_CLUSTER))"; \
	  command -v doctl >/dev/null || { echo "doctl not installed — install it, or run 'terraform -chdir=terraform refresh && terraform -chdir=terraform output -raw kubeconfig_raw > $(V2_KUBECONFIG)'"; exit 1; }; \
	  mkdir -p $$(dirname $(V2_KUBECONFIG)); \
	  doctl kubernetes cluster kubeconfig show $(V2_DO_CLUSTER) > $(V2_KUBECONFIG); \
	fi
.PHONY: v2-kubeconfig-ensure

## Build, push, and roll out to the DigitalOcean k8s (terraform/) cluster
v2-deploy: v2-buildx-setup v2-kubeconfig-ensure
	@set -e; \
	  TAG=$$(git rev-parse --short HEAD); \
	  DIRTY=$$(git status --porcelain | head -1); \
	  if [ -n "$$DIRTY" ]; then TAG="$$TAG-dirty"; fi; \
	  $(V2_CACHE_SETUP); \
	  echo ">> building $(V2_IMAGE):$$TAG ($(V2_PLATFORM))"; \
	  docker buildx build --builder $(V2_BUILDER) --platform $(V2_PLATFORM) $$CACHE_ARGS $(V2_BUILD_ARGS) \
	    --tag $(V2_IMAGE):$$TAG \
	    --tag $(V2_IMAGE):latest \
	    --push . ; \
	  $(V2_CACHE_PROMOTE); \
	  $(V2_PUBLISH_ASSETS); \
	  echo ">> rolling $(V2_DEPLOYMENT) (image stays :latest, restart forces pull)"; \
	  KUBECONFIG=$(V2_KUBECONFIG) kubectl -n $(V2_NAMESPACE) \
	    rollout restart deployment/$(V2_DEPLOYMENT); \
	  KUBECONFIG=$(V2_KUBECONFIG) kubectl -n $(V2_NAMESPACE) \
	    rollout status deployment/$(V2_DEPLOYMENT) --timeout=10m
.PHONY: v2-deploy

## Same as v2-deploy, but build+push both linux/amd64 and linux/arm64
v2-deploy-multiarch: V2_PLATFORM = linux/amd64,linux/arm64
v2-deploy-multiarch: v2-deploy
.PHONY: v2-deploy-multiarch

V2_GIT_REF ?= HEAD

## Build, push, and roll out from committed git contents only (ignores dirty working tree)
v2-deploy-committed: v2-buildx-setup v2-kubeconfig-ensure
	@set -e; \
	  REF="$(V2_GIT_REF)"; \
	  TAG=$$(git rev-parse --short "$$REF"); \
	  LOG=$$(mktemp); STATUS=$$(mktemp); ATTEMPT=1; \
	  trap 'rm -f "$$LOG" "$$STATUS"' EXIT; \
	  while :; do \
	    $(V2_CACHE_SETUP); \
	    echo ">> building $(V2_IMAGE):$$TAG ($(V2_PLATFORM)) from committed ref $$REF (attempt $$ATTEMPT)"; \
	    ( set +e; git archive --format=tar "$$REF" | docker buildx build --builder $(V2_BUILDER) --platform $(V2_PLATFORM) $$CACHE_ARGS $(V2_BUILD_ARGS) \
	        --tag $(V2_IMAGE):$$TAG \
	        --tag $(V2_IMAGE):latest \
	        --file Dockerfile \
	        --push - ; echo $$? > "$$STATUS" ) 2>&1 | tee "$$LOG"; \
	    if [ "$$(cat "$$STATUS")" = "0" ]; then break; fi; \
	    if [ "$$ATTEMPT" != "1" ] || ! grep -Eq '$(V2_CACHE_POISON)' "$$LOG"; then \
	      echo "$(RED)>> build failed$(RESET)"; exit 1; \
	    fi; \
	    echo "$(YELLOW)>> build failed on a stale build cache — retrying once; the next run repairs it$(RESET)"; \
	    ATTEMPT=2; \
	  done; \
	  $(V2_CACHE_PROMOTE); \
	  $(V2_PUBLISH_ASSETS) "$$REF"; \
	  echo ">> rolling $(V2_DEPLOYMENT) (image stays :latest, restart forces pull)"; \
	  KUBECONFIG=$(V2_KUBECONFIG) kubectl -n $(V2_NAMESPACE) \
	    rollout restart deployment/$(V2_DEPLOYMENT); \
	  KUBECONFIG=$(V2_KUBECONFIG) kubectl -n $(V2_NAMESPACE) \
	    rollout status deployment/$(V2_DEPLOYMENT) --timeout=10m
.PHONY: v2-deploy-committed

## Same as v2-deploy-committed, but build+push both linux/amd64 and linux/arm64
v2-deploy-committed-multiarch: V2_PLATFORM = linux/amd64,linux/arm64
v2-deploy-committed-multiarch: v2-deploy-committed
.PHONY: v2-deploy-committed-multiarch

## Build+push linux/amd64 and linux/arm64 images to Docker Hub (no rollout)
v2-push-multiarch: v2-buildx-setup
	@set -e; \
	  TAG=$$(git rev-parse --short HEAD); \
	  DIRTY=$$(git status --porcelain | head -1); \
	  if [ -n "$$DIRTY" ]; then TAG="$$TAG-dirty"; fi; \
	  $(V2_CACHE_SETUP); \
	  echo ">> building $(V2_IMAGE):$$TAG (linux/amd64,linux/arm64)"; \
	  docker buildx build --builder $(V2_BUILDER) --platform linux/amd64,linux/arm64 $$CACHE_ARGS $(V2_BUILD_ARGS) \
	    --tag $(V2_IMAGE):$$TAG \
	    --tag $(V2_IMAGE):latest \
	    --push . ; \
	  $(V2_CACHE_PROMOTE)
.PHONY: v2-push-multiarch

## Upload the frontend's hashed bundles to the CDN bucket (no build, no rollout)
v2-publish-assets: v2-buildx-setup
	@$(V2_PUBLISH_ASSETS) $(V2_GIT_REF)
.PHONY: v2-publish-assets

## Tail logs from the DigitalOcean k8s deployment
v2-logs: v2-kubeconfig-ensure
	KUBECONFIG=$(V2_KUBECONFIG) kubectl -n $(V2_NAMESPACE) logs \
	  -l app=$(V2_DEPLOYMENT) --tail=200 -f
.PHONY: v2-logs

## Report Postgres backends stuck idle-in-transaction blocking others
db-unstick:
	@./scripts/db-unstick.sh
.PHONY: db-unstick

## Same, but actually terminate the stuck holders (frees their locks)
db-unstick-kill:
	@./scripts/db-unstick.sh --kill
.PHONY: db-unstick-kill

## Sync local images to s3 bucket (public/ plus homebrew campaign and third edition images)
##
## Card faces are split in two: the clean masters go to the PRIVATE originals
## bucket, and only the watermarked, resolution-capped renditions built by
## scripts/watermark-cards.sh reach the public bucket. Everything else (boxes,
## sets, portraits, homebrew, 3ed) syncs from public/ as before.
##
## No --acl public-read anywhere: CloudFront reads the bucket through its origin
## access identity, so objects do not need to be world-readable, and dropping it
## closes the direct s3.amazonaws.com path. scripts/fetch-assets.sh is unaffected
## -- it lists via S3 (still public) and downloads via CloudFront.
# Card faces are supplied by the watermarked tree, so they are held back from
# the public/ sync -- except back_*, the face-down card back, which must stay
# unmarked and has no rendition. Stale .jpg/.png under some card dirs are no
# longer served and simply stop being re-synced.
CARD_EXCLUDES = --exclude "img/arkham/cards/*" --include "img/arkham/cards/back_*" \
                --exclude "img/arkham/es/cards/*" --exclude "img/arkham/fr/cards/*" \
                --exclude "img/arkham/ita/cards/*" --exclude "img/arkham/ko/cards/*" \
                --exclude "img/arkham/rus/cards/*" --exclude "img/arkham/zh/cards/*"

sync-images: watermark-cards
	cd frontend/public && aws s3 sync . s3://arkham-horror-assets --exclude "*.DS_Store" --exclude "img/custom/*" $(CARD_EXCLUDES)
	aws s3 sync frontend/.watermarked/img/arkham s3://arkham-horror-assets/img/arkham --exclude "*.DS_Store" --exclude ".build-settings"
	cd frontend/public && aws s3 sync . s3://arkham-horror-originals --exclude "*" \
		--include "img/arkham/cards/*" --include "img/arkham/es/cards/*" \
		--include "img/arkham/fr/cards/*" --include "img/arkham/ita/cards/*" \
		--include "img/arkham/ko/cards/*" --include "img/arkham/rus/cards/*" \
		--include "img/arkham/zh/cards/*" --exclude "*.DS_Store"
	./scripts/sync-homebrew-images.sh
	aws s3 sync frontend-3ed/public/img/ah3e s3://arkham-horror-assets/img/ah3e --exclude "*.DS_Store"
.PHONY: sync-images

## Build the watermarked, resolution-capped card renditions (incremental)
watermark-cards:
	./scripts/watermark-cards.sh
.PHONY: watermark-cards

## Fetch all images via CloudFront (requires aws + curl)
fetch-images:
	./scripts/fetch-assets.sh all
.PHONY: fetch-images

## Fetch only English card images via CloudFront (requires aws + curl)
fetch-cards:
	./scripts/fetch-assets.sh cards
.PHONY: fetch-cards

## Fetch only third edition images via CloudFront (requires aws + curl)
fetch-images-3ed:
	./scripts/fetch-assets.sh 3ed
.PHONY: fetch-images-3ed

## Fetch English images via Docker (no local aws CLI required)
fetch-images-docker:
	docker compose --profile fetch-images run --rm fetch-images
.PHONY: fetch-images-docker

## Fetch only English card images via Docker
fetch-cards-docker:
	docker compose --profile fetch-images run --rm fetch-images cards
.PHONY: fetch-cards-docker

## Regenerate image manifest (run after adding new images, before committing)
generate-manifest:
	node scripts/generate-manifest.cjs
.PHONY: generate-manifest

## Sync images to S3 and regenerate manifest
sync-and-manifest:
	./scripts/sync-and-manifest.sh
.PHONY: sync-and-manifest

## Install git hooks
install-hooks:
	cp scripts/check-manifest.sh .git/hooks/pre-commit
	chmod +x .git/hooks/pre-commit
	@echo "Pre-commit hook installed."
.PHONY: install-hooks

## Start the local dev stack: arkham-api + vite, images served from frontend/public
dev.up:
	@./dev.up
.PHONY: dev.up

## Count lines of code
count:
	cloc . --include-lang=Haskell,TypeScript,Vue --exclude-dir=node_modules,dist,.stack-work --timeout=0
.PHONY: count

## Tag HEAD with a date-based vYYYYMMDD.N tag and push it to trigger the offline build workflow
offline-build:
	@set -e; \
	  git fetch --tags --quiet; \
	  DATE=$$(date -u +%Y%m%d); \
	  N=1; \
	  while git rev-parse --verify --quiet "refs/tags/v$$DATE.$$N" >/dev/null; do \
	    N=$$((N + 1)); \
	  done; \
	  TAG="v$$DATE.$$N"; \
	  echo ">> tagging $$(git rev-parse --short HEAD) as $$TAG"; \
	  git tag "$$TAG"; \
	  git push origin "$$TAG"
.PHONY: offline-build
