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

## Build, push, and roll out to the DigitalOcean k8s (terraform/) cluster
v2-deploy:
	@command -v kubectl >/dev/null || { echo "kubectl not found"; exit 1; }
	@test -f $(V2_KUBECONFIG) || { echo "kubeconfig missing at $(V2_KUBECONFIG) — run 'terraform output -raw kubeconfig_raw > terraform/kubeconfig'"; exit 1; }
	@TAG=$$(git rev-parse --short HEAD); \
	  DIRTY=$$(git status --porcelain | head -1); \
	  if [ -n "$$DIRTY" ]; then TAG="$$TAG-dirty"; fi; \
	  echo ">> building $(V2_IMAGE):$$TAG ($(V2_PLATFORM))"; \
	  docker buildx build --platform $(V2_PLATFORM) \
	    --tag $(V2_IMAGE):$$TAG \
	    --tag $(V2_IMAGE):latest \
	    --push . ; \
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

## Build+push linux/amd64 and linux/arm64 images to Docker Hub (no rollout)
v2-push-multiarch:
	@TAG=$$(git rev-parse --short HEAD); \
	  DIRTY=$$(git status --porcelain | head -1); \
	  if [ -n "$$DIRTY" ]; then TAG="$$TAG-dirty"; fi; \
	  echo ">> building $(V2_IMAGE):$$TAG (linux/amd64,linux/arm64)"; \
	  docker buildx build --platform linux/amd64,linux/arm64 \
	    --tag $(V2_IMAGE):$$TAG \
	    --tag $(V2_IMAGE):latest \
	    --push .
.PHONY: v2-push-multiarch

## Tail logs from the DigitalOcean k8s deployment
v2-logs:
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
