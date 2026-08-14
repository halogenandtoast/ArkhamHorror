locals {
  app_namespace = "arkham"
  app_name      = "arkham-web"
  app_port      = 3000

  redis_url = format(
    "rediss://%s:%s@%s:%d",
    digitalocean_database_cluster.redis.user,
    digitalocean_database_cluster.redis.password,
    digitalocean_database_cluster.redis.private_host,
    digitalocean_database_cluster.redis.port,
  )

  # The app is compiled with `-with-rtsopts=-N` (see arkham-api/package.yaml),
  # and bare `-N` means "one capability per CPU the kernel reports". A container
  # under a Kubernetes CPU *limit* is restricted by a CFS quota, not a cpuset, so
  # the process still SEES every core on the node while only being allowed
  # limits.cpu worth of runtime. On an s-4vcpu-8gb node with a 2000m limit that is
  # 4 capabilities inside a 2-core quota.
  #
  # The risk is GC: a stop-the-world parallel collection needs every capability to
  # rendezvous and spin-waits while it does, so once the pod is throttled
  # mid-collection the unscheduled capabilities stall the rest, process-wide.
  #
  # Measured 2026-08-15, this is LATENT, not active -- it was investigated as the
  # cause of the Aug 14 slowdown and ruled out. On a pod ~21 minutes into its life
  # /sys/fs/cgroup/cpu.stat read nr_periods 11683, nr_throttled 2,
  # throttled_usec 4430: 4.4ms of throttling total, against a usage of ~0.88 of the
  # 2-core quota. The app does not get near its ceiling, so the extra capabilities
  # cost nothing today. Matching -N to the quota is cheap correctness for the day
  # that changes -- do not cite it as a fix for a latency problem without
  # re-reading nr_throttled first.
  #
  # Keep this expression tied to var.app_cpu_limit; the two drifting apart is the bug.
  app_cpu_limit_millicores = (
    endswith(var.app_cpu_limit, "m")
    ? tonumber(trimsuffix(var.app_cpu_limit, "m"))
    : tonumber(var.app_cpu_limit) * 1000
  )

  # Floor, and never below 1: a fractional limit still needs one capability, and
  # -N0 is rejected by the RTS.
  app_rts_capabilities = max(1, floor(local.app_cpu_limit_millicores / 1000))

  app_ghc_rts = trimspace("-N${local.app_rts_capabilities} ${var.app_rts_extra_flags}")
}

resource "kubernetes_namespace" "arkham" {
  metadata {
    name = local.app_namespace
    labels = {
      "app.kubernetes.io/part-of" = var.project_name
    }
  }
}

resource "kubernetes_secret" "registry" {
  count = var.registry_username == "" ? 0 : 1

  metadata {
    name      = "registry-credentials"
    namespace = kubernetes_namespace.arkham.metadata[0].name
  }

  type = "kubernetes.io/dockerconfigjson"

  data = {
    ".dockerconfigjson" = jsonencode({
      auths = {
        (var.registry_server) = {
          username = var.registry_username
          password = var.registry_password
          email    = var.registry_email
          auth     = base64encode("${var.registry_username}:${var.registry_password}")
        }
      }
    })
  }
}

resource "kubernetes_secret" "app" {
  metadata {
    name      = "${local.app_name}-secrets"
    namespace = kubernetes_namespace.arkham.metadata[0].name
  }

  type = "Opaque"

  data = merge(
    {
      DATABASE_URL = var.database_url
      REDIS_CONN   = local.redis_url
    },
    var.app_secrets,
  )
}

# DigitalOcean's managed Valkey serves a public Let's Encrypt cert
# (CN=*.f.db.ondigitalocean.com), NOT the "Project CA" their API returns.
# Application.hs pins the rediss:// trust store to whatever is in this file
# only — system roots are ignored — so we ship ISRG Root X1 (LE's root) in
# the bundle. The DO Project CA is included alongside in case future DO
# changes start chaining off it.
locals {
  isrg_root_x1_pem = <<-EOT
    -----BEGIN CERTIFICATE-----
    MIIFazCCA1OgAwIBAgIRAIIQz7DSQONZRGPgu2OCiwAwDQYJKoZIhvcNAQELBQAw
    TzELMAkGA1UEBhMCVVMxKTAnBgNVBAoTIEludGVybmV0IFNlY3VyaXR5IFJlc2Vh
    cmNoIEdyb3VwMRUwEwYDVQQDEwxJU1JHIFJvb3QgWDEwHhcNMTUwNjA0MTEwNDM4
    WhcNMzUwNjA0MTEwNDM4WjBPMQswCQYDVQQGEwJVUzEpMCcGA1UEChMgSW50ZXJu
    ZXQgU2VjdXJpdHkgUmVzZWFyY2ggR3JvdXAxFTATBgNVBAMTDElTUkcgUm9vdCBY
    MTCCAiIwDQYJKoZIhvcNAQEBBQADggIPADCCAgoCggIBAK3oJHP0FDfzm54rVygc
    h77ct984kIxuPOZXoHj3dcKi/vVqbvYATyjb3miGbESTtrFj/RQSa78f0uoxmyF+
    0TM8ukj13Xnfs7j/EvEhmkvBioZxaUpmZmyPfjxwv60pIgbz5MDmgK7iS4+3mX6U
    A5/TR5d8mUgjU+g4rk8Kb4Mu0UlXjIB0ttov0DiNewNwIRt18jA8+o+u3dpjq+sW
    T8KOEUt+zwvo/7V3LvSye0rgTBIlDHCNAymg4VMk7BPZ7hm/ELNKjD+Jo2FR3qyH
    B5T0Y3HsLuJvW5iB4YlcNHlsdu87kGJ55tukmi8mxdAQ4Q7e2RCOFvu396j3x+UC
    B5iPNgiV5+I3lg02dZ77DnKxHZu8A/lJBdiB3QW0KtZB6awBdpUKD9jf1b0SHzUv
    KBds0pjBqAlkd25HN7rOrFleaJ1/ctaJxQZBKT5ZPt0m9STJEadao0xAH0ahmbWn
    OlFuhjuefXKnEgV4We0+UXgVCwOPjdAvBbI+e0ocS3MFEvzG6uBQE3xDk3SzynTn
    jh8BCNAw1FtxNrQHusEwMFxIt4I7mKZ9YIqioymCzLq9gwQbooMDQaHWBfEbwrbw
    qHyGO0aoSCqI3Haadr8faqU9GY/rOPNk3sgrDQoo//fb4hVC1CLQJ13hef4Y53CI
    rU7m2Ys6xt0nUW7/vGT1M0NPAgMBAAGjQjBAMA4GA1UdDwEB/wQEAwIBBjAPBgNV
    HRMBAf8EBTADAQH/MB0GA1UdDgQWBBR5tFnme7bl5AFzgAiIyBpY9umbbjANBgkq
    hkiG9w0BAQsFAAOCAgEAVR9YqbyyqFDQDLHYGmkgJykIrGF1XIpu+ILlaS/V9lZL
    ubhzEFnTIZd+50xx+7LSYK05qAvqFyFWhfFQDlnrzuBZ6brJFe+GnY+EgPbk6ZGQ
    3BebYhtF8GaV0nxvwuo77x/Py9auJ/GpsMiu/X1+mvoiBOv/2X/qkSsisRcOj/KK
    NFtY2PwByVS5uCbMiogziUwthDyC3+6WVwW6LLv3xLfHTjuCvjHIInNzktHCgKQ5
    ORAzI4JMPJ+GslWYHb4phowim57iaztXOoJwTdwJx4nLCgdNbOhdjsnvzqvHu7Ur
    TkXWStAmzOVyyghqpZXjFaH3pO3JLF+l+/+sKAIuvtd7u+Nxe5AW0wdeRlN8NwdC
    jNPElpzVmbUq4JUagEiuTDkHzsxHpFKVK7q4+63SM1N95R1NbdWhscdCb+ZAJzVc
    oyi3B43njTOQ5yOf+1CceWxG1bQVs5ZufpsMljq4Ui0/1lvh+wjChP4kqKOJ2qxq
    4RgqsahDYVvTH9w7jXbyLeiNdd8XM2w9U/t7y0Ff/9yi0GE44Za4rF2LN9d11TPA
    mRGunUHBcnWEvgJBQl9nJEiU0Zsnvgc/ubhPgXRR4Xq37Z0j4r7g1SgEEzwxA57d
    emyPxgcYxn/eR44/KJ4EBs+lVDR3veyJm+kXQ99b21/+jh5Xos1AnX5iItreGCc=
    -----END CERTIFICATE-----
  EOT

  redis_ca_bundle = join(
    "\n",
    [
      replace(local.isrg_root_x1_pem, "/(?m)^    /", ""),
      data.digitalocean_database_ca.valkey.certificate,
    ],
  )
}

resource "kubernetes_secret" "valkey_ca" {
  metadata {
    name      = "valkey-ca"
    namespace = kubernetes_namespace.arkham.metadata[0].name
  }

  type = "Opaque"

  data = {
    "digital-ocean.crt" = local.redis_ca_bundle
  }
}

resource "kubernetes_deployment" "app" {
  metadata {
    name      = local.app_name
    namespace = kubernetes_namespace.arkham.metadata[0].name
    labels = {
      app = local.app_name
    }
  }

  spec {
    replicas = var.app_replicas

    selector {
      match_labels = {
        app = local.app_name
      }
    }

    strategy {
      type = "RollingUpdate"
      rolling_update {
        max_surge       = "1"
        max_unavailable = "0"
      }
    }

    template {
      metadata {
        labels = {
          app = local.app_name
        }
      }

      spec {
        # Default for Deployments, set explicitly so the intent is visible:
        # any pod that exits — including OOMKill from memory limits below —
        # is restarted automatically.
        restart_policy = "Always"

        termination_grace_period_seconds = 30

        # Spread replicas across nodes so a single node going down
        # doesn't take the whole app with it.
        topology_spread_constraint {
          max_skew           = 1
          topology_key       = "kubernetes.io/hostname"
          when_unsatisfiable = "ScheduleAnyway"
          label_selector {
            match_labels = {
              app = local.app_name
            }
          }
        }

        dynamic "image_pull_secrets" {
          for_each = var.registry_username == "" ? [] : [1]
          content {
            name = kubernetes_secret.registry[0].metadata[0].name
          }
        }

        container {
          name              = "web"
          image             = var.app_image
          image_pull_policy = "Always"

          port {
            container_port = local.app_port
            name           = "http"
          }

          env {
            name  = "NODE_ENV"
            value = "production"
          }
          env {
            name  = "PORT"
            value = tostring(local.app_port)
          }
          env {
            name  = "ASSET_HOST"
            value = var.asset_host
          }

          # GHCRTS is honoured because the binary is built with -rtsopts, and it
          # is applied AFTER the baked-in -with-rtsopts, so it wins. See
          # local.app_ghc_rts for why -N must not be left to the RTS to guess.
          env {
            name  = "GHCRTS"
            value = local.app_ghc_rts
          }

          # Postgres connections per pod. Without this the app falls back to the
          # `database-pool-size` default of 10 in arkham-api/config/settings.yml,
          # which was never a deliberate production choice. Every action holds a
          # transaction (and a FOR UPDATE lock on the game row) for the whole of
          # runMessages, so the pool is a hard ceiling on concurrent actions per
          # pod -- 10 was being reached.
          #
          # Budget against the database's max_connections, not the pod: the
          # ceiling is app_max_replicas * this value, plus room for psql sessions
          # and migrations. At 6 replicas x 20 = 120 against max_connections 200.
          env {
            name  = "DB_POOL"
            value = tostring(var.app_db_pool)
          }

          # var.app_secrets keys become env var names here, so a GHCRTS or DB_POOL
          # placed in there would collide with the two above. Kubernetes resolves
          # an explicit `env` ahead of `env_from`, so these win -- which is the
          # right way round (the derived -N should not be silently overridable by
          # an ad-hoc secret), but it does mean setting either in app_secrets has
          # no effect. Change the variable instead.
          env_from {
            secret_ref {
              name = kubernetes_secret.app.metadata[0].name
            }
          }

          resources {
            requests = {
              cpu    = var.app_cpu_request
              memory = var.app_memory_request
            }
            # Hitting this limit triggers OOMKill, which restarts the
            # container under Deployment's Always restart policy.
            limits = {
              cpu    = var.app_cpu_limit
              memory = var.app_memory_limit
            }
          }

          # Restarts the container if /health stops responding (hung process,
          # deadlock, etc.). Tuned long enough for cold start of the Haskell binary.
          liveness_probe {
            http_get {
              path = "/health"
              port = local.app_port
            }
            initial_delay_seconds = 60
            period_seconds        = 20
            timeout_seconds       = 5
            failure_threshold     = 3
          }

          # Removes the pod from the Service while it boots or is unhealthy,
          # without restarting it.
          readiness_probe {
            http_get {
              path = "/health"
              port = local.app_port
            }
            initial_delay_seconds = 15
            period_seconds        = 10
            timeout_seconds       = 3
            failure_threshold     = 3
          }

          # Gives a slow-booting container time to come up before the liveness
          # probe is allowed to kill it.
          startup_probe {
            http_get {
              path = "/health"
              port = local.app_port
            }
            period_seconds    = 10
            failure_threshold = 30
            timeout_seconds   = 5
          }

          volume_mount {
            name       = "valkey-ca"
            mount_path = "/opt/arkham/src/backend/arkham-api/digital-ocean.crt"
            sub_path   = "digital-ocean.crt"
            read_only  = true
          }
        }

        volume {
          name = "valkey-ca"
          secret {
            secret_name = kubernetes_secret.valkey_ca.metadata[0].name
            items {
              key  = "digital-ocean.crt"
              path = "digital-ocean.crt"
            }
          }
        }
      }
    }
  }

  # Wait for the rollout so dependent resources (HPA, ingress) don't race.
  wait_for_rollout = true

  depends_on = [
    kubernetes_secret.app,
  ]

  lifecycle {
    # `make v2-deploy` ships by pushing :latest and running `kubectl rollout
    # restart`, which stamps this annotation to force the pull. It isn't in the
    # config, so Terraform plans to strip it — changing the pod template and
    # triggering a second, pointless rollout on every apply. Ignore it and the
    # two tools stop undoing each other.
    # The HPA owns spec.replicas once it exists, and this resource also declares
    # it (var.app_replicas). Without ignoring it, every apply scales the
    # Deployment back down to the baseline and the HPA has to climb out again --
    # so an apply during load actively re-created the 2-pod condition that made
    # the 2026-08-15 slowdown possible. app_replicas is the value the Deployment
    # is CREATED with; app_min_replicas is the floor thereafter.
    ignore_changes = [
      spec[0].template[0].metadata[0].annotations["kubectl.kubernetes.io/restartedAt"],
      spec[0].replicas,
    ]
  }
}

resource "kubernetes_service" "app" {
  metadata {
    name      = local.app_name
    namespace = kubernetes_namespace.arkham.metadata[0].name
    labels = {
      app = local.app_name
    }
  }

  spec {
    type = "ClusterIP"
    selector = {
      app = local.app_name
    }
    port {
      name        = "http"
      port        = 80
      target_port = local.app_port
    }
  }
}

# Memory-driven autoscaling: when average pod memory crosses
# app_memory_target_utilization, HPA adds replicas. Combined with the
# memory limit above, "high memory consumption" produces both a
# horizontal scale-out and (at the per-pod limit) a hard restart.
resource "kubernetes_horizontal_pod_autoscaler_v2" "app" {
  metadata {
    name      = local.app_name
    namespace = kubernetes_namespace.arkham.metadata[0].name
  }

  spec {
    scale_target_ref {
      api_version = "apps/v1"
      kind        = "Deployment"
      name        = kubernetes_deployment.app.metadata[0].name
    }

    min_replicas = var.app_min_replicas
    max_replicas = var.app_max_replicas

    metric {
      type = "Resource"
      resource {
        name = "memory"
        target {
          type                = "Utilization"
          average_utilization = var.app_memory_target_utilization
        }
      }
    }

    metric {
      type = "Resource"
      resource {
        name = "cpu"
        target {
          type                = "Utilization"
          average_utilization = 80
        }
      }
    }

    behavior {
      scale_up {
        stabilization_window_seconds = 30
        select_policy                = "Max"
        policy {
          type           = "Percent"
          value          = 100
          period_seconds = 30
        }
      }
      scale_down {
        stabilization_window_seconds = 300
        select_policy                = "Max"
        policy {
          type           = "Percent"
          value          = 50
          period_seconds = 60
        }
      }
    }
  }
}

# Keeps at least one pod available during voluntary disruptions (node drains,
# upgrades). With 2 nodes this means a node-down/restart event can't take
# the service fully offline.
resource "kubernetes_pod_disruption_budget_v1" "app" {
  metadata {
    name      = local.app_name
    namespace = kubernetes_namespace.arkham.metadata[0].name
  }

  spec {
    min_available = 1
    selector {
      match_labels = {
        app = local.app_name
      }
    }
  }
}
