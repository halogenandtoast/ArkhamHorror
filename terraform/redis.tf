resource "digitalocean_database_cluster" "redis" {
  # DigitalOcean retired the `redis` engine for new clusters; Valkey is the
  # drop-in replacement. The wire protocol is identical, so Hedis (and
  # `rediss://` connection strings) work unchanged.
  name                 = "${local.name}-valkey"
  engine               = "valkey"
  version              = var.redis_version
  size                 = var.redis_size
  region               = var.region
  node_count           = 1
  private_network_uuid = digitalocean_vpc.arkham.id

  eviction_policy = "allkeys_lru"

  maintenance_window {
    day  = "sunday"
    hour = "05:00:00"
  }

  tags = local.common_tags
}

resource "digitalocean_database_firewall" "redis" {
  cluster_id = digitalocean_database_cluster.redis.id

  rule {
    type  = "k8s"
    value = digitalocean_kubernetes_cluster.arkham.id
  }
}

# Valkey is its own cluster with its own CA cert. The app verifies the
# `rediss://` connection against the file at
# /opt/arkham/src/backend/arkham-api/digital-ocean.crt (see
# Application.hs:325) — we fetch this cluster's CA and mount it over
# that path in the Deployment.
data "digitalocean_database_ca" "valkey" {
  cluster_id = digitalocean_database_cluster.redis.id
}
