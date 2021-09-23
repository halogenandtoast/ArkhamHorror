.PHONY: deploy
deploy:
	docker build -t arkham .
	docker tag arkham:latest 834069176314.dkr.ecr.us-east-1.amazonaws.com/arkham:latest
	aws ecr get-login-password --region us-east-1 | docker login --username AWS --password-stdin 834069176314.dkr.ecr.us-east-1.amazonaws.com
	docker push 834069176314.dkr.ecr.us-east-1.amazonaws.com/arkham:latest
	aws ecs update-service --cluster arkham --service arkham-service --force-new-deployment

.PHONY: sync-images
sync-images:
	cd frontend/public && aws s3 sync . s3://arkham-horror-assets --acl public-read

.PHONY: count
count:
	cloc . --include-lang=Haskell,TypeScript,Vue --exclude-dir=node_modules,dist,.stack-work --timeout=0
