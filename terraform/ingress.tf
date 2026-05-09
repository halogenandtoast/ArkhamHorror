locals {
  tls_domains = length(var.tls_domains) > 0 ? var.tls_domains : [
    var.domain,
    "www.${var.domain}",
  ]

  tls_annotations = var.tls_enabled ? {
    "service.beta.kubernetes.io/do-loadbalancer-certificate-id"         = digitalocean_certificate.app[0].uuid
    "service.beta.kubernetes.io/do-loadbalancer-tls-ports"              = "443"
    "service.beta.kubernetes.io/do-loadbalancer-redirect-http-to-https" = "true"
  } : {}
}

# DigitalOcean-managed Let's Encrypt certificate. The domain must be hosted
# on DigitalOcean DNS for issuance to succeed (DO uses DNS-01 against its own
# nameservers). If you're on Cloudflare/Route53/etc, set tls_enabled = false
# and either upload a custom cert with `digitalocean_certificate` (type =
# "custom") or front the LB with Cloudflare and let it terminate TLS.
resource "digitalocean_certificate" "app" {
  count = var.tls_enabled ? 1 : 0

  name    = "${local.name}-cert"
  type    = "lets_encrypt"
  domains = local.tls_domains

  lifecycle {
    create_before_destroy = true

    # DigitalOcean auto-renews managed Let's Encrypt certs ~30 days before
    # expiry and refreshes any attached load balancers in place. The cert
    # UUID stays stable across renewals, but `not_after` and the fingerprint
    # change — ignore those so a refresh after auto-renewal doesn't mark
    # this resource as needing recreation (which would briefly drop the LB's
    # cert binding).
    ignore_changes = [
      not_after,
      sha1_fingerprint,
    ]
  }
}

resource "kubernetes_service" "app_lb" {
  metadata {
    name      = "${local.app_name}-lb"
    namespace = kubernetes_namespace.arkham.metadata[0].name

    annotations = merge(
      {
        # Force the L7-capable LB type. DO's CCM defaults to REGIONAL_NETWORK
        # (L4 only), which silently ignores TLS-termination annotations and
        # leaves :443 as raw TCP passthrough.
        "service.beta.kubernetes.io/do-loadbalancer-type"                                = "REGIONAL"
        "service.beta.kubernetes.io/do-loadbalancer-name"                                = "${local.name}-lb"
        "service.beta.kubernetes.io/do-loadbalancer-protocol"                            = "http"
        "service.beta.kubernetes.io/do-loadbalancer-healthcheck-path"                    = "/health"
        "service.beta.kubernetes.io/do-loadbalancer-healthcheck-protocol"                = "http"
        "service.beta.kubernetes.io/do-loadbalancer-healthcheck-check-interval-seconds"  = "10"
        "service.beta.kubernetes.io/do-loadbalancer-enable-proxy-protocol"               = "false"
      },
      local.tls_annotations,
    )
  }

  spec {
    type = "LoadBalancer"
    selector = {
      app = local.app_name
    }

    port {
      name        = "http"
      port        = 80
      target_port = local.app_port
      protocol    = "TCP"
    }

    dynamic "port" {
      for_each = var.tls_enabled ? [1] : []
      content {
        name        = "https"
        port        = 443
        target_port = local.app_port
        protocol    = "TCP"
      }
    }
  }
}
