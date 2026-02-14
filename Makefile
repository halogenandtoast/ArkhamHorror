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

## Sync local images to s3 bucket
sync-images:
	cd frontend/public && aws s3 sync . s3://arkham-horror-assets --acl public-read --exclude ".DS_Store"
.PHONY: sync-images

## Fetch images from CDN for local development
fetch-images:
	./scripts/fetch-assets.sh all
.PHONY: fetch-images

## Fetch only English card images from CDN
fetch-cards:
	./scripts/fetch-assets.sh cards
.PHONY: fetch-cards

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
