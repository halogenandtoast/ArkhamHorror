output "cluster_id" {
  value       = digitalocean_kubernetes_cluster.arkham.id
  description = "DOKS cluster UUID"
}

output "cluster_endpoint" {
  value       = digitalocean_kubernetes_cluster.arkham.endpoint
  description = "Kubernetes API endpoint"
}

output "kubeconfig_raw" {
  value       = digitalocean_kubernetes_cluster.arkham.kube_config[0].raw_config
  description = "Kubeconfig YAML — write to a file with `terraform output -raw kubeconfig_raw > kubeconfig`"
  sensitive   = true
}

output "redis_uri" {
  value       = digitalocean_database_cluster.redis.private_uri
  description = "Private rediss:// URI used by the app — already wired into the app Secret"
  sensitive   = true
}

output "load_balancer_ip" {
  value       = try(kubernetes_service.app_lb.status[0].load_balancer[0].ingress[0].ip, null)
  description = "Public IP of the DigitalOcean LB. May take a minute after apply to populate."
}

output "tls_certificate_id" {
  value       = try(digitalocean_certificate.app[0].uuid, null)
  description = "ID of the LE cert attached to the LB (null when tls_enabled = false)"
}

output "tls_domains" {
  value       = var.tls_enabled ? local.tls_domains : []
  description = "Domains the LE cert was issued for"
}
