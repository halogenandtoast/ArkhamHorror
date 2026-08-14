locals {
  name = var.project_name

  common_tags = [
    "project:${var.project_name}",
    "managed-by:terraform",
  ]
}

resource "digitalocean_vpc" "arkham" {
  name     = "${local.name}-vpc"
  region   = var.region
  ip_range = "10.20.0.0/16"
}

resource "digitalocean_kubernetes_cluster" "arkham" {
  name     = "${local.name}-doks"
  region   = var.region
  version  = var.k8s_version
  vpc_uuid = digitalocean_vpc.arkham.id

  auto_upgrade  = true
  surge_upgrade = true

  maintenance_policy {
    start_time = "04:00"
    day        = "sunday"
  }

  node_pool {
    name = "${local.name}-pool"
    size = var.node_size

    # Pinned to exactly 2 nodes per spec — no cluster autoscaler. If the user
    # wants horizontal node scaling later, set auto_scale = true and add
    # min_nodes / max_nodes.
    node_count = var.node_count
    auto_scale = false

    tags = local.common_tags
    labels = {
      role = "app"
    }
  }

  tags = local.common_tags

  lifecycle {
    # auto_upgrade is on, so DO moves the cluster onto newer patch versions on
    # its own and `version` drifts away from var.k8s_version within weeks. That
    # attribute forces replacement, so without this the config sits permanently
    # armed to DESTROY AND RECREATE the production cluster on the next apply
    # (which also makes every downstream kubernetes_* resource unplannable,
    # because the endpoint becomes "known after apply").
    #
    # var.k8s_version is therefore only the version the cluster is *created*
    # with. To move an existing cluster deliberately, bump the variable and
    # temporarily drop `version` from this list.
    ignore_changes = [version]
  }
}
