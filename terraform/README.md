# Terraform — DigitalOcean Kubernetes deploy

Stands up Arkham Horror on DigitalOcean Kubernetes (DOKS):

- VPC + DOKS cluster pinned at **2 worker nodes** (`s-4vcpu-8gb` by default)
- Managed Redis cluster on the same VPC, wired to the app as `REDIS_CONN`
- App `Deployment`, `Service`, DigitalOcean LoadBalancer `Service`
- Restart / availability rules:
  - `restartPolicy: Always` on the pod
  - Memory `limits` → exceeding triggers OOMKill, which the Deployment auto-restarts
  - `livenessProbe` on `/health` → restarts a hung container
  - `readinessProbe` + `startupProbe` so a slow boot doesn't get killed mid-startup
  - `HorizontalPodAutoscaler` on memory (75% target) and CPU (80%) — high-memory load scales out
  - `PodDisruptionBudget` keeping ≥1 pod available during node drains/upgrades
  - DOKS auto-recovery handles node-down events; `topologySpreadConstraints` keep replicas on separate nodes

## Usage

```sh
cd terraform
cp terraform.tfvars.example terraform.tfvars   # fill in DO token, DB URL, etc.
terraform init
terraform plan
terraform apply

# Pull the kubeconfig for kubectl access
terraform output -raw kubeconfig_raw > kubeconfig
export KUBECONFIG=$PWD/kubeconfig
kubectl -n arkham get pods,svc,hpa
```

The LB IP is exposed as `load_balancer_ip` once the DigitalOcean LB finishes provisioning. Point your DNS at it.

## Notes

- The existing AWS Terraform under `infra/terraform/environments/prod/` is unrelated and untouched.
- Postgres is **not** provisioned here — pass `database_url` pointing at your existing managed DB.
- Redis URL is generated and injected automatically; nothing to set manually.
