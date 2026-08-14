# metrics-server -- the resource-metrics API (metrics.k8s.io) the HPA reads.
#
# DOKS does not ship this, and nothing else here installed it, so the API simply
# did not exist: `kubectl get apiservice v1beta1.metrics.k8s.io` returned
# NotFound and there was no deployment in kube-system.
#
# Without it the HPA in app.tf cannot compute EITHER of its metrics, so it never
# scales at all. Measured 2026-08-15, before this was added:
#
#   ScalingActive  False  FailedGetResourceMetric
#   Warning  FailedGetResourceMetric  (x75522 over 13d)
#   REPLICAS 2   MINPODS 2   MAXPODS 6
#
# 75,522 consecutive failures over 13 days, pinned at min_replicas the whole
# time. `kubectl top` did not work either, for the same reason. So the
# "memory-driven scale-out" the HPA comment in app.tf describes had never once
# happened, and the app was permanently capped at 2 pods no matter the load --
# which, combined with DB_POOL, is a hard ceiling on concurrent game actions.
#
# The HPA is what makes this load-bearing rather than cosmetic, so it lives in
# terraform beside the HPA rather than being kubectl-applied once and forgotten.

resource "helm_release" "metrics_server" {
  count = var.metrics_server_enabled ? 1 : 0

  name       = "metrics-server"
  repository = "https://kubernetes-sigs.github.io/metrics-server/"
  chart      = "metrics-server"
  version    = var.metrics_server_chart_version
  namespace  = "kube-system"

  # Block until the deployment is actually Ready. The point of installing this is
  # that the HPA can read it; a release that reports success while the API is
  # still unavailable would leave the HPA broken and look fixed.
  wait    = true
  timeout = 300

  # metrics-server scrapes each kubelet over TLS and verifies its serving
  # certificate. That works on DOKS, whose kubelets get certs signed by the
  # cluster CA -- unlike several managed providers where this must be disabled.
  # Kept as a variable rather than set unconditionally: skipping verification is
  # a real (if minor, on a private network) downgrade and shouldn't be the
  # silent default. See var.metrics_server_kubelet_insecure_tls.
  dynamic "set" {
    for_each = var.metrics_server_kubelet_insecure_tls ? [1] : []
    content {
      name  = "args[0]"
      value = "--kubelet-insecure-tls"
    }
  }

  depends_on = [digitalocean_kubernetes_cluster.arkham]
}
