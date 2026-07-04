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

## Deploy production (will migrate database)
deploy:
	kamal deploy
.PHONY: deploy

V2_IMAGE       ?= halogenandtoast/arkham-horror
V2_KUBECONFIG  ?= $(CURDIR)/terraform/kubeconfig
V2_NAMESPACE   ?= arkham
V2_DEPLOYMENT  ?= arkham-web
V2_PLATFORM    ?= linux/amd64
V2_BUILDER     ?= arkham-multiarch
V2_DO_CLUSTER  ?= arkham-horror-doks
V2_CACHE_DIR   ?= $(CURDIR)/.buildx-cache/v2
V2_CACHE_NEXT  ?= $(V2_CACHE_DIR).new
V2_CACHE_PREV  ?= $(V2_CACHE_DIR).prev
V2_CACHE_SETUP = CACHE_ARGS="--cache-to type=local,dest=$(V2_CACHE_NEXT),mode=max"; rm -rf "$(V2_CACHE_NEXT)"; if [ -d "$(V2_CACHE_DIR)" ]; then CACHE_ARGS="--cache-from type=local,src=$(V2_CACHE_DIR) $$CACHE_ARGS"; fi
V2_CACHE_PROMOTE = if [ -d "$(V2_CACHE_NEXT)" ]; then rm -rf "$(V2_CACHE_PREV)"; if [ -d "$(V2_CACHE_DIR)" ]; then mv "$(V2_CACHE_DIR)" "$(V2_CACHE_PREV)"; fi; if mv "$(V2_CACHE_NEXT)" "$(V2_CACHE_DIR)"; then rm -rf "$(V2_CACHE_PREV)"; else if [ -d "$(V2_CACHE_PREV)" ]; then mv "$(V2_CACHE_PREV)" "$(V2_CACHE_DIR)"; fi; exit 1; fi; fi

## Ensure a docker-container buildx builder exists (required for multi-platform builds)
v2-buildx-setup:
	@docker buildx inspect $(V2_BUILDER) >/dev/null 2>&1 || \
	  docker buildx create --name $(V2_BUILDER) --driver docker-container >/dev/null
.PHONY: v2-buildx-setup

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
	  docker buildx build --builder $(V2_BUILDER) --platform $(V2_PLATFORM) $$CACHE_ARGS \
	    --tag $(V2_IMAGE):$$TAG \
	    --tag $(V2_IMAGE):latest \
	    --push . ; \
	  $(V2_CACHE_PROMOTE); \
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
	  $(V2_CACHE_SETUP); \
	  echo ">> building $(V2_IMAGE):$$TAG ($(V2_PLATFORM)) from committed ref $$REF"; \
	  git archive --format=tar "$$REF" | docker buildx build --builder $(V2_BUILDER) --platform $(V2_PLATFORM) $$CACHE_ARGS \
	    --tag $(V2_IMAGE):$$TAG \
	    --tag $(V2_IMAGE):latest \
	    --file Dockerfile \
	    --push - ; \
	  $(V2_CACHE_PROMOTE); \
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
	  docker buildx build --builder $(V2_BUILDER) --platform linux/amd64,linux/arm64 $$CACHE_ARGS \
	    --tag $(V2_IMAGE):$$TAG \
	    --tag $(V2_IMAGE):latest \
	    --push . ; \
	  $(V2_CACHE_PROMOTE)
.PHONY: v2-push-multiarch

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

## Sync local images to s3 bucket
sync-images:
	cd frontend/public && aws s3 sync . s3://arkham-horror-assets --acl public-read --exclude ".DS_Store"
.PHONY: sync-images

## Fetch all images via CloudFront (requires aws + curl)
fetch-images:
	./scripts/fetch-assets.sh all
.PHONY: fetch-images

## Fetch only English card images via CloudFront (requires aws + curl)
fetch-cards:
	./scripts/fetch-assets.sh cards
.PHONY: fetch-cards

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
