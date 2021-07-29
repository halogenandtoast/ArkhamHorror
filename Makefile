.PHONY: deploy
deploy:
	docker build -t arkham .
	docker tag arkham:latest 834069176314.dkr.ecr.us-east-1.amazonaws.com/arkham:latest
	aws ecr get-login-password --region us-east-1 | docker login --username AWS --password-stdin 834069176314.dkr.ecr.us-east-1.amazonaws.com
	docker push 834069176314.dkr.ecr.us-east-1.amazonaws.com/arkham:latest
	aws ecs update-service --cluster arkham --service arkham-service --force-new-deployment
