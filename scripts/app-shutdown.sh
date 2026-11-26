#!/usr/bin/env bash
# app-shutdown.sh -- deprovision the whole production stack, and bring it back,
# leaving the Postgres database alone.
#
# `down` destroys everything in terraform/'s state: both worker nodes, the DOKS
# cluster, the VPC, the DigitalOcean load balancer, the Valkey cluster, the
# Let's Encrypt certificate, and the app's namespace / Deployment / Secrets.
#
# It cannot reach Postgres. That cluster was never provisioned here -- terraform
# only gets var.database_url pointing at an existing managed cluster, so it is
# absent from state and no plan generated here can include it. `down` asserts
# that, and refuses to apply a plan containing any database cluster other than
# the Valkey broker. Also surviving: DO DNS, the CloudFront asset bucket, the S3
# archive bucket, and the Docker Hub image -- everything needed to come back.
#
# WHAT GOING DOWN COSTS
#   * The load balancer's IP is released. `up` provisions a new LB with a new IP
#     and repoints the A/AAAA records recorded on the way down -- including the
#     DUPLICATE apex A record, which is easy to miss by hand and would leave
#     half of all traffic on a dead address.
#   * Valkey comes back empty. It holds only pub/sub channels and room presence
#     tallies, so nothing durable is lost; the counts re-derive as sockets
#     reconnect.
#   * The certificate is reissued on `up` (DNS-01 against DO's nameservers).
#   * ANYTHING ELSE SHARING THE CLUSTER DIES WITH IT. There is an `elder-sign`
#     namespace in this cluster that this terraform does not manage. `down`
#     refuses while a foreign namespace has pods unless you pass
#     --force-shared-cluster. With that flag it also deletes those namespaces'
#     LoadBalancer Services first, so the CCM releases their load balancers
#     instead of leaving them billing with nothing left to reconcile them -- but
#     it CANNOT bring those apps back. Redeploy each from its own repo.
#
# Usage:
#   scripts/app-shutdown.sh down      # drain, delete the LB, destroy the stack
#   scripts/app-shutdown.sh up        # reprovision, then repoint DNS
#   scripts/app-shutdown.sh status    # provisioned or not, plus leftovers
#   scripts/app-shutdown.sh orphans   # only the leftover-resource check
#
#   --yes                    skip confirmation prompts
#   --drain N                seconds for in-flight work to land (default 20)
#   --force-shared-cluster   proceed though a foreign namespace has pods
#   --skip-dns               do not repoint DNS on `up`
#   --no-verify-db           skip the read-only psql "who is connected" check
#
# `down` writes terraform/.app-shutdown-state.json (LB id and IPs, the DNS
# records that pointed at it, cluster ids). `up` reads it to repoint DNS, so
# keep it between the two runs.
#
# DB URL for the connection check: $DATABASE_URL, else database_url in
# terraform/terraform.tfvars.

set -euo pipefail

REPO="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
TF_DIR="$REPO/terraform"
STATE_FILE="$TF_DIR/.app-shutdown-state.json"
NAMESPACE=arkham
DEPLOYMENT=arkham-web
LB_SERVICE=arkham-web-lb
DO_CLUSTER=arkham-horror-doks
DO_LB_NAME=arkham-horror-lb
DOMAIN=arkhamhorror.app
export KUBECONFIG="${KUBECONFIG:-$TF_DIR/kubeconfig}"

# Namespaces that belong to this stack or to Kubernetes itself. Anything else
# with pods in it is another application sharing the cluster.
OWN_NAMESPACES='^(arkham|default|kube-system|kube-public|kube-node-lease)$'

CLUSTER_UP=1
DRAIN=20
ASSUME_YES=0
VERIFY_DB=1
FORCE_SHARED=0
SKIP_DNS=0
CMD=""

usage() { sed -n '2,/^set -euo/p' "$0" | sed '$d' | sed -E 's/^# ?//'; exit 0; }
die()   { echo "ERROR: $*" >&2; exit 1; }
say()   { printf '>> %s\n' "$*"; }

while [[ $# -gt 0 ]]; do
  case "$1" in
    down|up|status|orphans) CMD="$1"; shift ;;
    --yes|-y)               ASSUME_YES=1; shift ;;
    --drain)                DRAIN="$2"; shift 2 ;;
    --force-shared-cluster) FORCE_SHARED=1; shift ;;
    --skip-dns)             SKIP_DNS=1; shift ;;
    --no-verify-db)         VERIFY_DB=0; shift ;;
    -h|--help)              usage ;;
    *) die "unknown arg: $1 (try --help)" ;;
  esac
done
[[ -n "$CMD" ]] || usage
[[ "$DRAIN" =~ ^[0-9]+$ ]] || die "--drain wants a whole number of seconds"

TMP="$(mktemp -d)"
SAVED_SELECTOR=""
SELECTOR_PATCHED=0

# If we abort between closing the LB and deleting it, the app is up but
# unreachable. Put the selector back. Disarmed once the Service is deleted,
# which is the point of no return.
restore_selector() {
  [[ $SELECTOR_PATCHED -eq 1 ]] || return 0
  SELECTOR_PATCHED=0
  say "putting the LB selector back ($SAVED_SELECTOR)"
  kubectl -n "$NAMESPACE" patch svc "$LB_SERVICE" --type=merge \
    -p "{\"spec\":{\"selector\":$SAVED_SELECTOR}}" >/dev/null 2>&1 \
    || echo "WARNING: could not restore it. Run: kubectl -n $NAMESPACE patch svc $LB_SERVICE --type=merge -p '{\"spec\":{\"selector\":$SAVED_SELECTOR}}'" >&2
}
cleanup() { local rc=$?; restore_selector; rm -rf "$TMP"; exit $rc; }
trap cleanup EXIT

tf() { terraform -chdir="$TF_DIR" "$@"; }

confirm() {
  [[ $ASSUME_YES -eq 1 ]] && return 0
  printf '\nType the word yes to continue: '
  local reply; read -r reply
  [[ "$reply" == "yes" ]] || die "aborted"
}

# -------------------------------------------------------------- preflight ----

require_tools() {
  local bin
  for bin in "$@"; do command -v "$bin" >/dev/null || die "$bin not found"; done
}

# Returns non-zero when the cluster cannot be reached, rather than dying: a
# destroy that failed part-way may already have taken the cluster, and `down`
# still has to be able to finish the job.
ensure_kubeconfig() {
  command -v kubectl >/dev/null || return 1
  if [[ -f "$KUBECONFIG" ]] && kubectl get ns --request-timeout=10s >/dev/null 2>&1; then
    return 0
  fi
  command -v doctl >/dev/null || return 1
  say "kubeconfig missing or stale -- refreshing via doctl ($DO_CLUSTER)"
  doctl kubernetes cluster kubeconfig show "$DO_CLUSTER" >"$TMP/kubeconfig" 2>/dev/null || return 1
  [[ -s "$TMP/kubeconfig" ]] || return 1
  mv "$TMP/kubeconfig" "$KUBECONFIG"
  kubectl get ns --request-timeout=10s >/dev/null 2>&1
}

# The property the whole script rests on: terraform does not manage Postgres,
# so nothing it plans can delete it. The Valkey broker is the one database
# cluster legitimately in state.
assert_postgres_unmanaged() {
  local strays
  strays=$(tf state list 2>/dev/null \
    | grep '^digitalocean_database_cluster\.' \
    | grep -v '^digitalocean_database_cluster\.redis$' || true)
  [[ -z "$strays" ]] || die "terraform state holds an unexpected database cluster: $strays"
}

# Host of the database being left alone, credentials stripped.
db_url() {
  if [[ -n "${DATABASE_URL:-}" ]]; then echo "$DATABASE_URL"; return; fi
  awk -F\" '/^database_url[[:space:]]*=/ {print $2; exit}' "$TF_DIR/terraform.tfvars"
}
db_host() { sed -E 's#^[a-z]+://[^@]*@##; s#\?.*$##' <<<"$(db_url)"; }

do_lb_json() {
  doctl compute load-balancer list -o json 2>/dev/null \
    | jq --arg n "$DO_LB_NAME" '[.[] | select(.name==$n)][0] // empty'
}

# Other applications sharing the cluster. Destroying the cluster destroys them,
# and their load balancers are orphaned because the CCM dies with the cluster.
foreign_namespaces() {
  kubectl get ns -o json --request-timeout=20s 2>/dev/null \
    | jq -r '.items[].metadata.name' \
    | grep -vE "$OWN_NAMESPACES" || true
}
foreign_with_pods() {
  local ns
  for ns in $(foreign_namespaces); do
    if [[ -n "$(kubectl -n "$ns" get pods --no-headers --request-timeout=20s 2>/dev/null)" ]]; then
      echo "$ns"
    fi
  done
}
# The CCM stamps the live LB's id onto the Service; that is the only reliable
# way back from a Service to the DigitalOcean resource it created.
foreign_lb_records() {
  kubectl get svc -A -o json --request-timeout=20s 2>/dev/null | jq -r --arg own "$NAMESPACE" '
    .items[]
    | select(.spec.type=="LoadBalancer")
    | select(.metadata.namespace != $own)
    | [.metadata.namespace, .metadata.name,
       (.metadata.annotations["kubernetes.digitalocean.com/load-balancer-id"] // ""),
       (.status.loadBalancer.ingress[0].ip // "pending")]
    | @tsv' || true
}

foreign_lbs_json() {
  foreign_lb_records | jq -R -s 'split("\n") | map(select(length>0) | split("\t"))
    | map({namespace:.[0], name:.[1], lb_id:.[2], ip:.[3]})'
}

# Delete the LoadBalancer Services of namespaces this stack does not own, so
# the CCM releases their DigitalOcean load balancers while it still exists.
# Without this they outlive the cluster and bill forever with nothing left to
# reconcile them.
release_foreign_lbs() {
  local recs; recs=$(foreign_lb_records)
  [[ -n "$recs" ]] || return 0
  say "releasing load balancers belonging to other namespaces (--force-shared-cluster)"
  local ns name id ip waited
  while IFS=$'\t' read -r ns name id ip; do
    [[ -z "${ns:-}" ]] && continue
    say "  deleting Service $ns/$name ($ip)"
    kubectl -n "$ns" delete svc "$name" --wait=true --timeout=120s >/dev/null 2>&1 || true
    [[ -n "${id:-}" ]] || { echo "  no LB id on that Service -- check for a leftover by hand" >&2; continue; }
    waited=0
    while doctl compute load-balancer get "$id" >/dev/null 2>&1; do
      if [[ $waited -ge 300 ]]; then
        echo "  WARNING: LB $id still up; delete it with: doctl compute load-balancer delete $id" >&2
        break
      fi
      sleep 10; waited=$((waited + 10))
    done
    [[ $waited -lt 300 ]] && say "  released $id after ${waited}s"
  done <<<"$recs"
}

guard_shared_cluster() {
  local shared; shared=$(foreign_with_pods)
  [[ -z "$shared" ]] && return 0

  echo
  echo "  OTHER APPLICATIONS ARE RUNNING IN THIS CLUSTER."
  echo
  echo "  Destroying the DOKS cluster destroys them too. They are not in this"
  echo "  terraform state, so \`up\` CANNOT bring them back -- you would redeploy"
  echo "  each from its own repo."
  echo
  local ns
  for ns in $shared; do
    printf '    namespace %s -- %s pod(s)\n' "$ns" \
      "$(kubectl -n "$ns" get pods --no-headers --request-timeout=20s 2>/dev/null | grep -c . || echo 0)"
  done
  local flbs; flbs=$(foreign_lb_records)
  if [[ -n "$flbs" ]]; then
    echo
    echo "  Their load balancers, which --force-shared-cluster WILL release so they"
    echo "  stop billing (the pods and the DNS records are still yours to deal with):"
    awk -F'\t' '{printf "    %s/%s  %s\n", $1, $2, $4}' <<<"$flbs"
  fi

  if [[ $FORCE_SHARED -eq 0 ]]; then
    echo
    die "refusing to destroy a shared cluster. Re-run with --force-shared-cluster once you have dealt with the above."
  fi
  echo
  echo "  --force-shared-cluster given: proceeding, and taking the above down too."
}

# Reject any plan that is not exactly the change advertised. $1 = plan json,
# $2 = the action every listed resource must have, $3 = the expected address
# set (newline separated), $4 = strict|loose.
verify_plan() {
  local json="$1" want="$2" expected="$3" mode="$4" changes addrs extra missing
  changes=$(jq -r '
    (.resource_changes // [])[]
    | select((.change.actions | inside(["no-op","read"])) | not)
    | "\(.address) \(.change.actions | join(","))"
  ' "$json")

  local addr actions
  while read -r addr actions; do
    [[ -z "$addr" ]] && continue
    [[ "$actions" == "$want" ]] \
      || die "plan wants to '$actions' $addr -- expected only '$want'. Refusing. Read it with: terraform -chdir=terraform show $json"
  done <<<"$changes"

  # Belt and braces on top of assert_postgres_unmanaged: whatever the state
  # says, no plan that touches a non-Valkey database cluster is applied.
  local dbs
  dbs=$(awk '{print $1}' <<<"$changes" \
    | grep '^digitalocean_database_cluster\.' \
    | grep -v '^digitalocean_database_cluster\.redis$' || true)
  [[ -z "$dbs" ]] || die "plan touches a database cluster that is not the Valkey broker: $dbs"

  addrs=$(awk '{print $1}' <<<"$changes" | grep -v '^$' | sort || true)
  expected=$(grep -v '^$' <<<"$expected" | sort)

  missing=$(comm -23 <(echo "$expected") <(echo "$addrs"))
  extra=$(comm -13 <(echo "$expected") <(echo "$addrs"))

  [[ -z "$missing" ]] || die "plan is missing resources it should have included: $(tr '\n' ' ' <<<"$missing")"
  if [[ -n "$extra" ]]; then
    [[ "$mode" == strict ]] && die "plan touches unexpected resources: $(tr '\n' ' ' <<<"$extra")"
    echo
    echo "  NOTE: the plan also covers resources beyond the recorded set:"
    sed 's/^/    - /' <<<"$extra"
    confirm
  fi
}

# ----------------------------------------------------------------- record ----

# Facts terraform will not remember for us, gathered BEFORE anything is
# destroyed so the confirmation can quote them and `up` can act on them.
LB_ID=""; LB_IP=""; LB_IPV6=""; DNS_JSON="[]"; FOREIGN_LBS="[]"

collect_facts() {
  local lb; lb=$(do_lb_json)
  if [[ -n "$lb" ]]; then
    LB_ID=$(jq -r '.id // ""' <<<"$lb")
    LB_IP=$(jq -r '.ip // ""' <<<"$lb")
    LB_IPV6=$(jq -r '.ipv6 // ""' <<<"$lb")
  fi

  # EVERY record pointing at the LB. The apex A record is duplicated here, and
  # repointing only one of the pair leaves half of all lookups on a dead IP.
  if [[ -n "$LB_IP" ]]; then
    DNS_JSON=$(doctl compute domain records list "$DOMAIN" -o json 2>/dev/null \
      | jq --arg ip "$LB_IP" --arg ip6 "$LB_IPV6" \
          '[.[] | select((.data == $ip) or ($ip6 != "" and .data == $ip6))
                | {id, type, name, data, ttl}]' 2>/dev/null || echo '[]')
    [[ -n "$DNS_JSON" ]] || DNS_JSON="[]"
  fi

  if [[ $CLUSTER_UP -eq 1 ]]; then
    FOREIGN_LBS=$(foreign_lbs_json 2>/dev/null || echo '[]')
    [[ -n "$FOREIGN_LBS" ]] || FOREIGN_LBS="[]"
  fi
}

record_state() {
  jq -n \
    --arg at "$(date -u '+%Y-%m-%dT%H:%M:%SZ')" \
    --arg domain "$DOMAIN" \
    --arg lb_id "$LB_ID" --arg lb_ip "$LB_IP" --arg lb_ipv6 "$LB_IPV6" \
    --arg cluster "$(tf state show digitalocean_kubernetes_cluster.arkham 2>/dev/null | awk '/^ *id *=/ {gsub(/"/,"",$3); print $3; exit}')" \
    --argjson dns "$DNS_JSON" \
    --argjson foreign "$FOREIGN_LBS" \
    '{taken_down_at:$at, domain:$domain, lb:{id:$lb_id, ip:$lb_ip, ipv6:$lb_ipv6}, cluster_id:$cluster, dns_records:$dns, foreign_load_balancers:$foreign}' \
    > "$STATE_FILE"
  say "recorded the way back in $STATE_FILE"
}

# ---------------------------------------------------------------- orphans ----

check_orphans() {
  require_tools doctl jq
  local found=0 lb vk ks vpc label managed
  # A resource terraform still manages is not an orphan -- it is the stack
  # running as intended. Only once state is empty does a survivor mean the
  # teardown left something behind that is still being billed for.
  managed=$(tf state list 2>/dev/null | grep -vc '^data\.' || echo 0)
  if [[ "$managed" -gt 0 ]]; then label="still provisioned"; else label="LEFTOVER"; fi

  lb=$(do_lb_json)
  if [[ -n "$lb" ]]; then
    found=1
    printf '  %s  load balancer %s (%s) holding %s\n' \
      "$label" "$DO_LB_NAME" "$(jq -r .id <<<"$lb")" "$(jq -r .ip <<<"$lb")"
    [[ "$managed" -eq 0 ]] && printf '      doctl compute load-balancer delete %s\n' "$(jq -r .id <<<"$lb")"
  fi

  vk=$(doctl databases list -o json 2>/dev/null | jq -r '.[] | select(.name=="arkham-horror-valkey") | .id')
  if [[ -n "$vk" ]]; then
    found=1
    printf '  %s  Valkey cluster %s\n' "$label" "$vk"
    [[ "$managed" -eq 0 ]] && printf '      doctl databases delete %s\n' "$vk"
  fi

  ks=$(doctl kubernetes cluster list -o json 2>/dev/null | jq -r --arg n "$DO_CLUSTER" '.[] | select(.name==$n) | .id')
  if [[ -n "$ks" ]]; then
    found=1
    printf '  %s  DOKS cluster %s\n' "$label" "$ks"
    [[ "$managed" -eq 0 ]] && printf '      doctl kubernetes cluster delete %s\n' "$ks"
  fi

  vpc=$(doctl vpcs list -o json 2>/dev/null | jq -r '.[] | select(.name=="arkham-horror-vpc") | .id')
  if [[ -n "$vpc" ]]; then
    found=1
    printf '  %s  VPC %s\n' "$label" "$vpc"
    [[ "$managed" -eq 0 ]] && printf '      doctl vpcs delete %s\n' "$vpc"
  fi

  # Once the cluster is gone these can no longer be enumerated from
  # Kubernetes, so they are checked against what `down` recorded.
  if [[ -f "$STATE_FILE" ]]; then
    local fid fns fname
    while IFS=$'\t' read -r fid fns fname; do
      [[ -z "${fid:-}" ]] && continue
      if doctl compute load-balancer get "$fid" >/dev/null 2>&1; then
        found=1
        printf '  LEFTOVER  load balancer %s for %s/%s -- still billing\n' "$fid" "$fns" "$fname"
        printf '      doctl compute load-balancer delete %s\n' "$fid"
      fi
    done < <(jq -r '(.foreign_load_balancers // [])[] | select(.lb_id != "") | [.lb_id, .namespace, .name] | @tsv' "$STATE_FILE" 2>/dev/null)
  fi

  if [[ $found -eq 0 ]]; then
    echo "  nothing left over -- cluster, LB, Valkey and VPC are all gone"
  elif [[ "$managed" -gt 0 ]]; then
    printf '  (terraform still manages %s resource(s), so the above is expected)\n' "$managed"
  fi
  return 0
}

cmd_orphans() { echo; check_orphans; echo; }

# ----------------------------------------------------------------- status ----

cmd_status() {
  require_tools terraform jq
  local managed; managed=$(tf state list 2>/dev/null | grep -vc '^data\.' || echo 0)
  echo
  printf 'terraform manages %s resource(s)\n' "$managed"
  printf 'database (never managed here): %s\n' "$(db_host)"

  if [[ -f "$STATE_FILE" ]]; then
    echo
    echo "A recorded teardown exists ($STATE_FILE):"
    jq -r '"  taken down at \(.taken_down_at)\n  old LB \(.lb.ip) (\(.lb.id))\n  DNS records to repoint: \(.dns_records | length)"' "$STATE_FILE"
  fi

  if [[ "$managed" -gt 0 ]] && command -v kubectl >/dev/null \
     && kubectl get ns "$NAMESPACE" --request-timeout=10s >/dev/null 2>&1; then
    echo
    kubectl -n "$NAMESPACE" get deploy,hpa,svc --no-headers --request-timeout=20s 2>/dev/null | sed 's/^/  /'
    local shared; shared=$(foreign_with_pods)
    [[ -n "$shared" ]] && printf '  sharing this cluster: %s\n' "$(tr '\n' ' ' <<<"$shared")"
  fi

  echo
  echo "DigitalOcean:"
  check_orphans
  echo
}

# The CCM is the only thing that deletes a DO load balancer, and it dies with
# the cluster. If the LB outlived both, nothing will ever clean it up: it keeps
# billing and keeps holding the IP.
reap_orphan_lb() {
  local lb; lb=$(do_lb_json)
  [[ -n "$lb" ]] || return 0
  local id ip
  id=$(jq -r '.id' <<<"$lb"); ip=$(jq -r '.ip' <<<"$lb")

  echo
  echo "  Load balancer $DO_LB_NAME ($id, $ip) outlived the cluster."
  echo "  Nothing else will delete it now -- the CCM went with the cluster -- so"
  echo "  it will keep billing until it is removed."

  # Deliberately not `confirm`: saying no here should not abandon the rest of
  # the teardown report, it should just hand over the command.
  if [[ $ASSUME_YES -eq 0 ]]; then
    printf '  Delete it? Type yes: '
    local reply; read -r reply
    if [[ "$reply" != "yes" ]]; then
      echo "  left in place -- remove it with: doctl compute load-balancer delete $id"
      return 0
    fi
  fi

  if doctl compute load-balancer delete "$id" --force >/dev/null 2>&1; then
    say "deleted load balancer $id"
  else
    echo "  FAILED. Remove it by hand: doctl compute load-balancer delete $id" >&2
  fi
}

# ------------------------------------------------------------------- down ----

cmd_down() {
  require_tools terraform kubectl jq doctl
  [[ -f "$TF_DIR/terraform.tfvars" ]] || die "missing $TF_DIR/terraform.tfvars"
  CLUSTER_UP=1
  ensure_kubeconfig || CLUSTER_UP=0
  [[ $CLUSTER_UP -eq 1 ]] \
    || say "cannot reach the cluster -- treating it as already gone; skipping the drain and the Service delete"
  assert_postgres_unmanaged

  local expected; expected=$(tf state list | grep -v '^data\.')
  [[ -n "$expected" ]] || die "terraform state is empty -- nothing to destroy"

  say "planning the destroy"
  tf plan -destroy -out="$TMP/down.tfplan" -input=false -no-color \
    >"$TMP/down.plan.txt" 2>&1 || { tail -30 "$TMP/down.plan.txt"; die "plan failed"; }
  tf show -json "$TMP/down.tfplan" >"$TMP/down.json"
  verify_plan "$TMP/down.json" delete "$expected" strict

  [[ $CLUSTER_UP -eq 1 ]] && guard_shared_cluster

  collect_facts
  local pods dns_n
  pods=0
  [[ $CLUSTER_UP -eq 1 ]] && pods=$(kubectl -n "$NAMESPACE" get pods -l "app=$DEPLOYMENT" --no-headers 2>/dev/null | grep -c . || echo 0)
  dns_n=$(jq 'length' <<<"$DNS_JSON")

  cat <<INFO

  DEPROVISIONING THE WHOLE STACK.

  destroying      $(wc -l <<<"$expected" | tr -d ' ') resources, every one in terraform state:
$(sed 's/^/                    /' <<<"$expected")

  pods now        $pods (all gone)
  LB ${LB_IP:-none}  DELETED -- the IP is released and not recoverable;
                  \`up\` repoints these $dns_n DNS record(s) at the new one:
$(jq -r '.[] | "                    \(.type) \(.name) -> \(.data)"' <<<"$DNS_JSON")
  drain           ${DRAIN}s for in-flight actions before anything is killed

  LEFT INTACT     $(db_host)
                  not in terraform state -- the verified plan above deletes no
                  database cluster other than the Valkey broker

  Bring it back with: scripts/app-shutdown.sh up
INFO
  confirm

  record_state

  # 1. Close the LB to new connections but leave it standing, so established
  #    connections -- game websockets included -- can finish their action.
  if [[ "$DRAIN" -gt 0 ]] && [[ $CLUSTER_UP -eq 1 ]]; then
    SAVED_SELECTOR=$(kubectl -n "$NAMESPACE" get svc "$LB_SERVICE" -o jsonpath='{.spec.selector}' 2>/dev/null || echo "")
    if [[ "$SAVED_SELECTOR" == *"$DEPLOYMENT"* ]]; then
      SELECTOR_PATCHED=1
      say "closing the LB to new connections"
      kubectl -n "$NAMESPACE" patch svc "$LB_SERVICE" --type=merge \
        -p "{\"spec\":{\"selector\":{\"app\":\"$DEPLOYMENT-draining\"}}}" >/dev/null
      say "draining ${DRAIN}s -- in-flight actions land, new ones are refused"
      sleep "$DRAIN"
    else
      say "LB selector is '$SAVED_SELECTOR', not what this expects -- skipping the drain"
    fi
  fi

  # 2. Delete the Service so the in-cluster CCM deletes the DO load balancer
  #    WHILE IT STILL CAN. If the cluster goes first the CCM goes with it and
  #    the LB is left running and billing.
  if [[ $CLUSTER_UP -eq 1 ]]; then
    say "deleting the LB Service so the CCM releases the load balancer"
    kubectl -n "$NAMESPACE" delete svc "$LB_SERVICE" --wait=true --timeout=120s >/dev/null 2>&1 || true
  fi
  SELECTOR_PATCHED=0   # point of no return; nothing to restore

  if [[ -n "$LB_ID" ]] && [[ $CLUSTER_UP -eq 1 ]]; then
    say "waiting for DigitalOcean to delete load balancer $LB_ID"
    local waited=0
    while doctl compute load-balancer get "$LB_ID" >/dev/null 2>&1; do
      [[ $waited -ge 300 ]] && { echo "WARNING: the LB is still there after ${waited}s; the orphan check at the end will catch it" >&2; break; }
      sleep 10; waited=$((waited + 10))
    done
    [[ $waited -lt 300 ]] && say "load balancer gone after ${waited}s"
  fi

  # 2b. Same for namespaces this stack does not own, now that the user has
  #     accepted taking them down. Their LBs would otherwise outlive the CCM.
  if [[ $FORCE_SHARED -eq 1 ]] && [[ $CLUSTER_UP -eq 1 ]]; then
    release_foreign_lbs
  fi

  # 3. Everything else. The saved plan still lists kubernetes_service.app_lb,
  #    which is already gone; terraform treats an absent resource as destroyed.
  say "destroying the rest of the stack (the cluster delete takes a few minutes)"
  tf apply -input=false -auto-approve "$TMP/down.tfplan" \
    || die "destroy failed part-way. Re-run 'scripts/app-shutdown.sh down' -- it replans from what is left."

  say "terraform state is now: $(tf state list | grep -vc '^data\.' || echo 0) resource(s)"

  reap_orphan_lb
  echo
  echo "Checking DigitalOcean for anything left behind:"
  check_orphans

  [[ $VERIFY_DB -eq 1 ]] && verify_db_quiet

  cat <<DONE

  THE STACK IS DOWN. Postgres is untouched and has no app writers left.

  DNS still points at the released IP, so the domain is dead until \`up\`:
$(jq -r '.dns_records[] | "    \(.type) \(.name) -> \(.data)"' "$STATE_FILE" 2>/dev/null || echo "    (none recorded)")

  Take the snapshot. DigitalOcean managed Postgres has no "snapshot now" call,
  so it is one of:

    doctl databases fork arkham-snapshot-$(date +%Y%m%d) \\
      --restore-from-cluster-name $(db_host | sed -E 's/-do-user.*//') \\
      --restore-from-timestamp '$(date -u '+%Y-%m-%d %H:%M:%S +0000 UTC')'

    pg_dump "\$DATABASE_URL" -Fc -f arkham-$(date +%Y%m%d).dump
      (~112 GB, nearly all of it arkham_steps -- hours, not minutes)

  Both are consistent whether or not the app is up, since DO's backups are
  point-in-time. The downtime buys a database with no open transactions and no
  game mid-action, not a more correct copy.

  Bring it back with: scripts/app-shutdown.sh up

DONE
}

# Read-only. Not fatal: your own psql session is a fine reason for a non-zero
# count.
verify_db_quiet() {
  command -v psql >/dev/null || { say "psql not installed -- skipping the connection check"; return 0; }
  local url; url=$(db_url)
  [[ -n "$url" ]] || { say "no DATABASE_URL -- skipping the connection check"; return 0; }

  say "checking what is still connected to $(db_host)"
  local out
  out=$(psql "$url" -X -P pager=off -At -F $'\t' -c "
    SELECT state, count(*) FROM pg_stat_activity
    WHERE datname = current_database() AND pid <> pg_backend_pid()
    GROUP BY state ORDER BY 2 DESC;" 2>&1) \
    || { echo "   (check failed: $out)"; return 0; }

  if [[ -z "$out" ]]; then
    echo "   no other sessions -- the database is idle"
  else
    sed 's/^/   /' <<<"$out"
    echo "   (whatever is left is not an app pod; there are none)"
  fi
}

# --------------------------------------------------------------------- up ----

cmd_up() {
  require_tools terraform jq
  [[ -f "$TF_DIR/terraform.tfvars" ]] || die "missing $TF_DIR/terraform.tfvars"
  assert_postgres_unmanaged

  say "planning the bring-up"
  if tf plan -out="$TMP/up.tfplan" -input=false -no-color >"$TMP/up.plan.txt" 2>&1; then
    tf show -json "$TMP/up.tfplan" >"$TMP/up.json"
    local creating
    creating=$(jq -r '(.resource_changes // [])[] | select(.change.actions==["create"]) | .address' "$TMP/up.json" | sort)
    if [[ -z "$creating" ]]; then
      say "nothing to create -- everything terraform manages already exists"
      echo "   (run 'scripts/app-shutdown.sh status' to see it; 'make v2-deploy' to ship code)"
      exit 0
    fi
    verify_plan "$TMP/up.json" create "$creating" loose
    cat <<INFO

  REPROVISIONING. This creates $(grep -c . <<<"$creating") resource(s): the VPC, the DOKS
  cluster and its 2 nodes, Valkey, the certificate, the load balancer and the
  app. Expect 8-15 minutes, most of it the cluster and the LB.
INFO
    confirm
    tf apply -input=false -auto-approve "$TMP/up.tfplan" || die "apply failed"
  else
    # With an empty state the kubernetes/helm providers are configured from a
    # cluster that does not exist yet, which terraform cannot always plan in
    # one pass. Fall back to its own interactive approval rather than guessing.
    say "a saved plan was not possible (providers depend on the not-yet-created cluster)"
    tail -5 "$TMP/up.plan.txt"
    echo
    echo "  Falling back to a plain apply -- terraform will show you the plan and ask."
    tf apply -input=true || die "apply failed"
  fi

  say "writing a fresh kubeconfig"
  tf output -raw kubeconfig_raw > "$TMP/kubeconfig" && mv "$TMP/kubeconfig" "$KUBECONFIG"

  say "waiting for the load balancer to get an address"
  local ip="" waited=0
  while [[ -z "$ip" ]] && [[ $waited -lt 420 ]]; do
    ip=$(kubectl -n "$NAMESPACE" get svc "$LB_SERVICE" \
      -o jsonpath='{.status.loadBalancer.ingress[0].ip}' 2>/dev/null || true)
    [[ -n "$ip" ]] && break
    sleep 10; waited=$((waited + 10))
  done
  [[ -n "$ip" ]] || die "no LB address after ${waited}s -- check: kubectl -n $NAMESPACE describe svc $LB_SERVICE"
  local ipv6
  ipv6=$(kubectl -n "$NAMESPACE" get svc "$LB_SERVICE" \
    -o jsonpath='{.status.loadBalancer.ingress[1].ip}' 2>/dev/null || true)
  say "new load balancer: $ip ${ipv6:+/ $ipv6}"

  if [[ $SKIP_DNS -eq 1 ]]; then
    echo
    echo "  --skip-dns: repoint these yourself, or the domain stays dark:"
    jq -r '.dns_records[] | "    doctl compute domain records update '"$DOMAIN"' --record-id \(.id) --record-type \(.type) --record-name \(.name) --record-data <new>"' "$STATE_FILE" 2>/dev/null
  else
    update_dns "$ip" "$ipv6"
  fi

  say "health check"
  curl -sS -k -m 20 -o /dev/null -w '   https://%{remote_ip}/health -> HTTP %{http_code}\n' "https://${ip}/health" \
    || echo "   (the LB can take a minute to mark backends healthy; retry shortly)"

  local shared_note
  shared_note=$(jq -r '.dns_records | length' "$STATE_FILE" 2>/dev/null || echo 0)
  cat <<DONE

  BACK ONLINE at $ip.

  Still to do by hand:
    * Anything else that shared the cluster (e.g. the elder-sign namespace) is
      NOT restored by this script -- redeploy it from its own repo.
    * DNS propagation takes up to the record TTL (600s for the apex).
    * Valkey is empty; room presence counts re-derive as players reconnect.

  Recorded teardown state is still at $STATE_FILE ($shared_note DNS record(s));
  delete it once you are happy.

DONE
}

update_dns() {
  local ip="$1" ipv6="$2"
  require_tools doctl
  [[ -f "$STATE_FILE" ]] || { echo "WARNING: no $STATE_FILE -- repoint DNS by hand to $ip" >&2; return 0; }

  local n; n=$(jq -r '.dns_records | length' "$STATE_FILE")
  [[ "$n" -gt 0 ]] || { say "no DNS records were recorded; nothing to repoint"; return 0; }

  echo
  echo "  Repointing $n DNS record(s) in $DOMAIN:"
  jq -r --arg ip "$ip" --arg ipv6 "$ipv6" '.dns_records[]
    | "    \(.type)\t\(.name)\t\(.data) -> " + (if .type=="AAAA" then $ipv6 else $ip end)' "$STATE_FILE"
  confirm

  local id type name target
  while IFS=$'\t' read -r id type name; do
    [[ -z "$id" ]] && continue
    target="$ip"
    if [[ "$type" == "AAAA" ]]; then
      [[ -z "$ipv6" ]] && { echo "   skipping AAAA $name -- the new LB reported no IPv6" >&2; continue; }
      target="$ipv6"
    fi
    if doctl compute domain records update "$DOMAIN" --record-id "$id" \
         --record-type "$type" --record-name "$name" --record-data "$target" >/dev/null; then
      printf '   %s %s -> %s\n' "$type" "$name" "$target"
    else
      echo "   FAILED to update $type $name (id $id) -- do it in the DO panel" >&2
    fi
  done < <(jq -r '.dns_records[] | "\(.id)\t\(.type)\t\(.name)"' "$STATE_FILE")
}

case "$CMD" in
  down)    cmd_down ;;
  up)      cmd_up ;;
  status)  cmd_status ;;
  orphans) cmd_orphans ;;
esac
