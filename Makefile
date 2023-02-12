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

## Migrate production
prod-migrate:
	cd migrations && heroku config:get DATABASE_URL | sed -e 's/postgres://' | xargs -I {} sqitch deploy db:pg:{}
.PHONY: prod-migrate

## Deploy production (will migrate database)
deploy: prod-migrate
	DOCKER_DEFAULT_PLATFORM=linux/amd64 docker build -t arkham .
	docker tag arkham:latest 834069176314.dkr.ecr.us-east-1.amazonaws.com/arkham:latest
	aws ecr get-login-password --region us-east-1 | docker login --username AWS --password-stdin 834069176314.dkr.ecr.us-east-1.amazonaws.com
	docker push 834069176314.dkr.ecr.us-east-1.amazonaws.com/arkham:latest
	aws ecs update-service --cluster arkham --service arkham-service --force-new-deployment
.PHONY: deploy

## Sync local images to s3 bucket
sync-images:
	cd frontend/public && aws s3 sync . s3://arkham-horror-assets --acl public-read
.PHONY: sync-images

## Count lines of code
count:
	cloc . --include-lang=Haskell,TypeScript,Vue --exclude-dir=node_modules,dist,.stack-work --timeout=0
.PHONY: count
