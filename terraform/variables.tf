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
  description = "DigitalOcean Kubernetes version slug. Use `doctl kubernetes options versions` to list."
  type        = string
  default     = "1.35.1-do.5"
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
  description = "HPA minimum replicas"
  type        = number
  default     = 2
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
  description = "Container CPU limit"
  type        = string
  default     = "2000m"
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
