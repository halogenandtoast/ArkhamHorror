# Production performance: topology, incident record, diagnostic playbook

Read this before diagnosing "the app is slow in production". It records the
2026-08-15 slowdown in full, the measurements that ruled things OUT (so they are
not re-chased), and the order to check things in.

The headline lesson: **a slow app with idle CPU and a healthy database is a
concurrency ceiling, not a resource problem.** Check `kubectl get hpa` for
`ScalingActive` before theorising about code.

---

## 1. Production topology (as of 2026-08-15)

nginx and Warp run in the **same pod**: nginx listens on 3000, serves
`frontend/dist`, and proxies `^~ /api` to `localhost:3002`. So "node CPU" is
both of them together. `prod.nginxconf` is the config; it sets no
`proxy_read_timeout`, so nginx's 60s default applies to REST (a saturated
`fetchGame` surfaces as a **504**), while websockets survive on the 15s
`withPingThread` keepalive.

| Thing | Value | Where |
|---|---|---|
| Nodes | 2 x `s-4vcpu-8gb` (4 CPU each) | `var.node_size` |
| Pod requests / limits | 500m / 1Gi -> 2 CPU / 2Gi | `var.app_cpu_request` etc. |
| HPA | min 4 (was 2), max 6, memory 75% + cpu 80% | `app.tf`, `var.app_min_replicas` |
| `DB_POOL` | 20 (was an accidental 10) | `var.app_db_pool` |
| Postgres | external managed; `max_connections` 200 | `var.database_url` |
| Valkey | `db-s-1vcpu-1gb` | `var.redis_size` |
| Deploy | push `:latest` + `kubectl rollout restart` | `make v2-deploy` |

**HPA utilisation is measured against `requests`, not `limits`.** `cpu: 33%` means
165m of the 500m request, not of the 2-core limit. Easy to misread as headroom.

**A pod's concurrency ceiling is `DB_POOL`,** because `updateGame` holds a
transaction *and* a `FOR UPDATE` lock on the game row for the entire duration of
`runMessages` (`Api/Handler/Arkham/Games/Shared.hs`). Site-wide ceiling =
`replicas x DB_POOL`.

---

## 2. What happened on 2026-08-15

A **retry storm standing on a capacity ceiling.** Both halves were required.

### Preconditions (silent, long-standing)

1. **`metrics-server` was never installed.** `v1beta1.metrics.k8s.io` returned
   NotFound; no deployment in `kube-system`. The HPA therefore failed *every*
   evaluation and sat pinned at `min_replicas`:
   ```
   ScalingActive  False  FailedGetResourceMetric
   Warning  FailedGetResourceMetric  (x75522 over 13d)
   REPLICAS 2   MINPODS 2   MAXPODS 6
   ```
   This also broke `kubectl top` — the obvious tool for spotting pod load was
   itself the missing piece, which is why it went unnoticed for 13+ days.
2. **`DB_POOL` was unset**, so each pod used the `config/settings.yml` default of
   **10**.
3. Terraform declared `spec.replicas` against the field the HPA owns, with no
   `ignore_changes`, so **every apply reset the deployment to 2 pods.**

**Ceiling: 2 x 10 = 20 concurrent game actions site-wide.**

### The trigger

`634f1729a1` (deployed 21:15 JST) added a client resync watchdog to `Game.vue`.
Every answer armed `RESYNC_AFTER_MS = 5000`; on expiry the client fetched the
**entire game** over REST (~134 KB plus a full DB read), up to
`MAX_RESYNC_ATTEMPTS = 4`, per client, per answer — plus a full resync on every
reconnect. The last-good build (`21e8a098c5`) has **zero** occurrences of
`resyncGame`.

### The collapse

Normal actions finish well under 5s, so the watchdog never fired and the deploy
looked clean. Then evening demand approached the 20-action ceiling, actions began
queueing, and latency crossed 5s. Now every waiting client fired a full
`fetchGame`, competing for **the same 10-connection pool the stalled actions were
occupying**. More load -> higher latency -> more watchdogs, up to 4x per answer.
Positive feedback, until `runMessages` exceeded its 30s circuit breaker
(`runMessagesTimeoutMicros`) and threw `RunMessagesTimeout` (a 500).

**It is a cliff, not a slope.** Below the threshold nothing fires and everything
is healthy. That is why it presented as "fine all day, broken at 10pm" and read
as a code regression with a clean bisect.

### Why the symptoms misled

- **CPU idle, load average barely moved** — the bottleneck was a *semaphore*
  (pool slots), not a resource. Waiting for a connection burns nothing.
- **Postgres looked healthy** — no `Lock` waits, everything `granted = t`, no
  `transactionid` rows. Backends sat `idle in transaction` / `ClientRead` with
  `xact_age` 0.1–5.2s: the DB waiting on the app, not the reverse.
- **The rollback "proved" a code regression** — it did remove the watchdog, but
  the ceiling was equally necessary.

---

## 3. Ruled OUT by measurement — do not re-chase

| Hypothesis | Disproof |
|---|---|
| Engine / message processing got slower | `arkham-replay` on the #5405 export, same answer, good vs bad commit: **identical** span counts (`fan/investigators` 2055 = 2055), 33144 span events both, wall clock 678.801 vs 676.234 ms (0.4%) |
| permessage-deflate CPU | That End Turn emitted **414 engine messages but only 1 websocket log frame** (261 bytes). Compression ≈ one 200 KB deflate ≈ 2.4 ms vs 679 ms of engine work = **0.35%**. The "hundreds of tiny frames per action" claim is wrong for normal play (scenario setup is the exception) |
| CPU throttling at the cgroup limit | `nr_periods 11683, nr_throttled 2, throttled_usec 4430` — 4.4 ms total, at ~0.88 of a 2-core quota |
| `-N` vs cgroup quota mismatch | Real (`nproc` = 4 inside a 2-core quota) but **latent**; see `local.app_ghc_rts`. Not a latency fix |
| Redis pub/sub concentration / Valkey buffer limits | Zero `pubsub subscriber died/stalled` and zero `redis publish rejected/failed` in logs |
| Game state growth | State size is **anti-correlated** with step count: 458 steps = 1095 kB vs 10226 steps = 1202 kB. Driven by table size and campaign scope, not history |
| zlib per-message `initDeflate` | Genuine bug (proven from library source, fixed in `3029d14e74`) but a memory-churn issue — and the failing build already contained the fix |

---

## 4. Diagnostic playbook

Cheapest and most discriminating first.

```bash
make v2-kubeconfig-ensure && export KUBECONFIG=$PWD/terraform/kubeconfig

# 1. Is the HPA actually working? (This was the 2026-08-15 answer.)
kubectl -n arkham get hpa
kubectl -n arkham describe hpa arkham-web | tail -20   # ScalingActive?
kubectl -n arkham top pods                             # fails if metrics-server is gone

# 2. Concurrency ceiling: replicas x DB_POOL
kubectl -n arkham get deploy arkham-web -o jsonpath='{.spec.replicas}{"\n"}'
kubectl -n arkham exec deploy/arkham-web -- sh -c 'echo $DB_POOL'

# 3. Engine actually slow, or just queueing? Grep the game ids.
kubectl -n arkham logs -l app=arkham-web --since=1h \
  | grep -o 'RunMessagesTimeout [^ ]*' | sort | uniq -c | sort -rn
# few ids repeating  -> poison game(s), replay it
# spread across many -> systemic: ceiling or a real hot-path regression

# 4. Retry storms: full-game REST fetches. ~9/min at 41 active games is healthy.
kubectl -n arkham logs -l app=arkham-web --since=3m \
  | grep -cE '"GET /api/v1/arkham/games/[0-9a-f-]{36}'

# 5. CPU throttling (usually NOT the answer — see section 3)
kubectl -n arkham exec deploy/arkham-web -- cat /sys/fs/cgroup/cpu.stat

# 6. Pub/sub health (these log lines exist precisely for this)
kubectl -n arkham logs -l app=arkham-web --since=1h \
  | grep -E 'pubsub subscriber (died|stalled)|redis publish (rejected|failed)'
```

Postgres side — `pg_locks` alone is useless here (it has no notion of *age*, and a
busy-but-healthy app shows granted locks). Use:

```sql
SELECT pid, state, now()-xact_start AS xact_age, now()-state_change AS since_change,
       wait_event_type, wait_event, left(query,120)
FROM pg_stat_activity WHERE datname = current_database() AND state <> 'idle'
ORDER BY xact_start;
```
Because `runMessages` runs *inside* the transaction, a slow action appears as
`idle in transaction` / `ClientRead` with a climbing `xact_age` — **not** as a
long-running query.

### Reproducing locally

`arkham-replay` is the tool; it needs no DB or server. Compare **span counts**,
which are deterministic and machine-independent, rather than times:

```bash
B=backend/arkham-api/.stack-work/dist/*/ghc-*/build/arkham-replay/arkham-replay
$B export.json --answers answers.json --metrics --metrics-top 20
$B export.json --answers answers.json --trace          # every message processed
```
`answers.json` is `[{"tag":"Answer","contents":{"choice":N,"playerId":"..."}}]`;
read the pending question out of `campaignData.currentData.gameQuestion`.
`--replay-all` often dies with `missing skill test` — a known undo-fidelity
limit, so drive the pending question directly instead.

To bisect a suspected regression, check the old commit out in the (clean) tree and
let the watcher rebuild — a `HasQueue`/`Message` change recompiles ~6500 modules
in ~5 min. A separate worktree needs ~42 G for `.stack-work`, which the disk has
not had spare.

---

## 5. Fixes applied

| Commit | Change |
|---|---|
| `f057ff88a2` | `metrics-server` via `helm_release` (chart pinned); `DB_POOL=20`; `GHCRTS=-N2` |
| `d1b72f8499` | `ignore_changes` on `spec[0].replicas`; `app_min_replicas` 2 -> 4 |
| `fd5be8146f` | Reconnect-only resync — the answer watchdog removed |
| `3029d14e74` | zlib takeover flags removed; `ARKHAM_WS_COMPRESSION` kill switch |

Verified after: HPA `ScalingActive` true, 4–6 replicas, ceiling 80–120 concurrent
actions (was 20), zero `RunMessagesTimeout`, busiest pod ~40% of its CPU limit
under above-average load (41 active games), compression left **on**.

**Caveat on attribution:** the watchdog removal and the capacity increase landed
together, so "it is fine now" does not prove which was decisive. The watchdog is
the better-supported culprit (the fast build has no resync machinery at all, and
the mechanism explains the cliff), but capacity alone may have sufficed.

---

## 6. Residual risks

- **The HPA is a silent single point of failure.** It failed closed for 13 days
  and nothing noticed. An alert on `ScalingActive` is worth more than any single
  fix above. `min_replicas` = 4 is the fallback floor.
- **`runMessagesTimeoutMicros` is wall-clock**, so queueing converts slowness into
  500s. Do **not** tighten it to "reduce damage" — that produces *more*
  user-visible errors, not fewer.
- **Deploy/rollback keys off `:latest`.** Roll back by the per-commit sha tag
  (`kubectl set image ... web=halogenandtoast/arkham-horror:<sha>`), not by
  `:latest`. Terraform manages `image = :latest`, so a `kubectl set image` pin
  drifts and the next apply silently reverts it.
- **Build disk** has been at 99% (`.stack-work` ~42 G), which blocks worktree-based
  bisects.
- If client-side cover for lost updates is ever needed again, make it cheap and
  self-limiting: probe a few bytes of current step, only fetch the whole game when
  it advanced, and add jitter so clients cannot synchronise.
