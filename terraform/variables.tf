variable "do_token" {
  description = "DigitalOcean API token"
  type        = string
  sensitive   = true
}

variable "project_name" {
  description = "Tag/name prefix for all resources"
  type        = string
  default     = "arkham-horror"
}

variable "region" {
  description = "DigitalOcean region slug (must support DOKS + managed Redis)"
  type        = string
  default     = "nyc3"
}

variable "k8s_version" {
  description = "DigitalOcean Kubernetes version slug used at cluster *creation*. Use `doctl kubernetes options versions` to list. Later drift from DO's auto-upgrade is ignored — see the lifecycle block in cluster.tf."
  type        = string
  default     = "1.35.5-do.2"
}

variable "node_count" {
  description = "Worker node count. Pinned to 2 per deploy spec."
  type        = number
  default     = 2
}

variable "node_size" {
  description = "Droplet slug for worker nodes. The Haskell app is memory-hungry, so default to 8GB."
  type        = string
  default     = "s-4vcpu-8gb"
}

variable "redis_size" {
  description = "Managed Valkey (Redis-compatible) slug"
  type        = string
  default     = "db-s-1vcpu-1gb"
}

variable "redis_version" {
  description = "Managed Valkey version. DigitalOcean retired `redis` engine — Valkey is wire-compatible."
  type        = string
  default     = "8"
}

variable "app_image" {
  description = "Container image for the arkham-horror app, including tag"
  type        = string
  default     = "halogenandtoast/arkham-horror:latest"
}

variable "app_replicas" {
  description = "Baseline replica count for the app Deployment"
  type        = number
  default     = 2
}

variable "app_min_replicas" {
  description = <<-EOT
    HPA minimum replicas — and, more importantly, the floor that applies when the
    HPA is NOT working.

    Raised from 2 to 4 after 2026-08-15. metrics-server was missing, so the HPA
    failed every evaluation for 13+ days and the Deployment sat at this floor the
    whole time; 2 pods (x DB_POOL) was a low enough concurrency ceiling that a
    modest per-action cost increase tipped the site into queueing collapse and
    30s RunMessagesTimeouts. 4 pods carried above-average load (41 active games)
    with the busiest pod at ~40% of its CPU limit and no timeouts.

    The HPA is a silent single point of failure, so this floor is the actual
    safety margin. Keep it at a value measured to carry real traffic on its own.
  EOT
  type        = number
  default     = 4
}

variable "app_max_replicas" {
  description = "HPA maximum replicas (memory-driven scale-out)"
  type        = number
  default     = 6
}

variable "app_memory_request" {
  description = "Container memory request"
  type        = string
  default     = "1Gi"
}

variable "app_memory_limit" {
  description = "Container memory limit. Exceeding this OOMKills the pod, which the Deployment auto-restarts."
  type        = string
  default     = "2Gi"
}

variable "app_cpu_request" {
  description = "Container CPU request"
  type        = string
  default     = "500m"
}

variable "app_cpu_limit" {
  description = "Container CPU limit. Also sets the RTS capability count (-N) via local.app_ghc_rts — a container sees the node's cores, not its quota, so the RTS cannot work this out for itself."
  type        = string
  default     = "2000m"
}

variable "app_rts_extra_flags" {
  description = <<-EOT
    Extra GHC RTS flags appended after the derived -N. Leave empty for the default.

    `-qg` is the one to reach for first if CPU throttling persists: it disables the
    parallel GC, trading slightly longer single-threaded collections for the removal
    of the multi-capability rendezvous that a CFS quota turns into a stall. `-qn2`
    limits GC threads without touching the mutator's capability count.
  EOT
  type        = string
  default     = ""
}

variable "metrics_server_enabled" {
  description = "Install metrics-server. Required for the HPA to compute ANY metric — without it the HPA is stuck at min_replicas. See metrics-server.tf."
  type        = bool
  default     = true
}

variable "metrics_server_chart_version" {
  description = "metrics-server Helm chart version (kubernetes-sigs.github.io/metrics-server)."
  type        = string
  default     = "3.13.1"
}

variable "metrics_server_kubelet_insecure_tls" {
  description = "Pass --kubelet-insecure-tls to metrics-server. Not needed on DOKS, whose kubelet serving certs are signed by the cluster CA. Turn on only if metrics-server logs TLS verification errors against the kubelets."
  type        = bool
  default     = false
}

variable "app_db_pool" {
  description = "Postgres connections per pod (DB_POOL). Budget app_max_replicas * this against the database's max_connections, leaving headroom for psql sessions and migrations."
  type        = number
  default     = 20
}

variable "app_memory_target_utilization" {
  description = "HPA memory target percent. Crossing this triggers scale-out."
  type        = number
  default     = 75
}

variable "domain" {
  description = "Public hostname. Leave empty to skip cert-manager/ingress hostname routing."
  type        = string
  default     = "arkhamhorror.app"
}

variable "acme_email" {
  description = "Email for Let's Encrypt registration"
  type        = string
  default     = ""
}

variable "tls_enabled" {
  description = "Provision a DigitalOcean-managed Let's Encrypt cert and terminate TLS at the LB"
  type        = bool
  default     = true
}

variable "http3_enabled" {
  description = "Advertise HTTP/3 (QUIC) on UDP 443 at the LB. Requires tls_enabled; the HTTPS rule on 443 stays either way, so clients that can't reach UDP 443 fall back to TCP."
  type        = bool
  default     = true
}

variable "tls_domains" {
  description = "Domains for the LE cert. Defaults to [domain, www.<domain>] if left empty. Domains MUST be hosted on DigitalOcean DNS for LE issuance to succeed."
  type        = list(string)
  default     = []
}

variable "database_url" {
  description = "Full Postgres connection URL. Wire to your existing managed DB."
  type        = string
  sensitive   = true
}

variable "asset_host" {
  description = "Public asset host"
  type        = string
  default     = "https://assets.arkhamhorror.app"
}

variable "app_secrets" {
  description = "Free-form app secrets to mount as env. Keys become env var names."
  type        = map(string)
  sensitive   = true
  default     = {}
}

variable "registry_server" {
  description = "Container registry server (e.g. https://index.docker.io/v1/)"
  type        = string
  default     = "https://index.docker.io/v1/"
}

variable "registry_username" {
  description = "Container registry username"
  type        = string
  default     = ""
}

variable "registry_password" {
  description = "Container registry password / token"
  type        = string
  sensitive   = true
  default     = ""
}

variable "registry_email" {
  description = "Email for the container registry"
  type        = string
  default     = ""
}
