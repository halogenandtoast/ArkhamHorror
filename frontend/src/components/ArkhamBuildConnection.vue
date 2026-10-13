<script lang="ts" setup>
/* Linking an account to arkham.build over OAuth 2.0.
 *
 * We are a confidential client: the client secret and both tokens live on our
 * server, and this component never sees one. All it does is ask the server for
 * an authorization URL, send the browser there, and read the outcome the
 * callback leaves in the query string on the way back. */
import { computed, onMounted, ref } from 'vue'
import { useRoute, useRouter } from 'vue-router'
import api from '@/api'
import ArkhamBuildLogo from '@/components/icons/ArkhamBuildLogo.vue'

type Connection = {
  accountId: string
  username: string
  scopes: string[]
  missingScopes: string[]
  connectedAt: string
  refreshedAt: string
  expiresAt: string
}

type Status = {
  configured: boolean
  scopes: string[]
  connection: Connection | null
}

const SCOPE_LABELS: Record<string, string> = {
  'profile:read': 'Read your account name',
  'decks:read': 'Read your decks, and the ArkhamDB decks linked to them',
  'decks:write': 'Save decks, so upgrades, exiles, and earned cards are recorded',
  'decks:delete': 'Delete your decks',
}

/* What the callback can leave behind. Anything unlisted is reported verbatim so
 * a new error from arkham.build is still legible rather than swallowed. */
const OUTCOMES: Record<string, { kind: 'good' | 'bad'; text: string }> = {
  connected: { kind: 'good', text: 'Connected to arkham.build.' },
  access_denied: { kind: 'bad', text: 'You declined the request, so nothing was connected.' },
  invalid_state: {
    kind: 'bad',
    text: 'That sign-in could not be matched to a request from this browser. Try again.',
  },
  expired: { kind: 'bad', text: 'That request took too long and expired. Try again.' },
  no_code: { kind: 'bad', text: 'arkham.build did not send an authorization code back.' },
  invalid_grant: {
    kind: 'bad',
    text: 'arkham.build would not accept that authorization. Try again.',
  },
  invalid_client: {
    kind: 'bad',
    text: "arkham.build did not recognise this server's client credentials.",
  },
  not_configured: { kind: 'bad', text: 'This server has no arkham.build client credentials.' },
  unreachable: { kind: 'bad', text: 'Could not reach arkham.build. Try again in a moment.' },
}

const route = useRoute()
const router = useRouter()

const status = ref<Status | null>(null)
const loading = ref(true)
const working = ref(false)
const failure = ref<string | null>(null)
const notice = ref<{ kind: 'good' | 'bad'; text: string } | null>(null)

const configured = computed(() => status.value?.configured === true)
const connection = computed(() => status.value?.connection ?? null)
const requestedScopes = computed(() => status.value?.scopes ?? [])
const missingScopes = computed(() => connection.value?.missingScopes ?? [])

const state = computed(() => {
  if (loading.value) return 'loading'
  if (!configured.value) return 'unavailable'
  if (!connection.value) return 'disconnected'
  return missingScopes.value.length ? 'partial' : 'connected'
})

function describeScope(scope: string): string {
  return SCOPE_LABELS[scope] ?? scope
}

function when(value: string | null | undefined): string {
  if (!value) return '—'
  return new Date(value).toLocaleDateString(undefined, {
    year: 'numeric',
    month: 'short',
    day: 'numeric',
  })
}

function errorMessage(e: unknown): string | null {
  const data = (e as { response?: { data?: { message?: string; messages?: string[] } } })?.response
    ?.data
  return data?.message ?? data?.messages?.join('; ') ?? null
}

/* The callback redirects to /#<returnTo>?arkhamBuild=<outcome>. The router uses
 * hash history, so the query lives inside the fragment and only vue-router sees
 * it — window.location.search is empty here. Read it once, then replace the
 * route without it so a reload does not replay the banner. */
async function readOutcome() {
  const outcome = route.query.arkhamBuild
  if (typeof outcome !== 'string' || !outcome) return
  notice.value = OUTCOMES[outcome] ?? { kind: 'bad', text: `arkham.build reported: ${outcome}` }
  const { arkhamBuild: _dropped, ...rest } = route.query
  await router.replace({ path: route.path, query: rest })
}

async function load() {
  loading.value = true
  failure.value = null
  try {
    const { data } = await api.get<Status>('arkham-build/connection')
    status.value = data
  } catch (e) {
    failure.value = errorMessage(e) ?? 'Could not read your arkham.build connection.'
  } finally {
    loading.value = false
  }
}

async function connect() {
  working.value = true
  failure.value = null
  try {
    const { data } = await api.post<{ url: string }>('arkham-build/authorize', {
      // The route, not window.location.pathname — that is "/" under hash history.
      returnTo: route.path,
    })
    window.location.href = data.url
  } catch (e) {
    working.value = false
    failure.value = errorMessage(e) ?? 'Could not start the connection.'
  }
}

async function verify() {
  working.value = true
  failure.value = null
  notice.value = null
  try {
    const { data } = await api.post<{ connection: Connection | null }>('arkham-build/verify', {})
    if (status.value) status.value = { ...status.value, connection: data.connection }
    notice.value = { kind: 'good', text: 'The connection works.' }
  } catch (e) {
    notice.value = {
      kind: 'bad',
      text: errorMessage(e) ?? 'The connection did not answer. You may need to reconnect.',
    }
  } finally {
    working.value = false
  }
}

async function disconnect() {
  if (!window.confirm('Disconnect arkham.build? You can connect again at any time.')) return
  working.value = true
  failure.value = null
  notice.value = null
  try {
    await api.delete('arkham-build/connection')
    notice.value = { kind: 'good', text: 'Disconnected from arkham.build.' }
    await load()
  } catch (e) {
    failure.value = errorMessage(e) ?? 'Could not disconnect.'
  } finally {
    working.value = false
  }
}

onMounted(async () => {
  await readOutcome()
  await load()
})
</script>

<template>
  <section class="box column connections">
    <header class="group-head">
      <h3>Connections</h3>
      <p class="muted">
        Link this app to the services you keep decks on. Nothing is shared until you
        approve it, and you can take it back at any time.
      </p>
    </header>

    <article class="entry">
      <div class="entry-head">
        <ArkhamBuildLogo class="mark" />
        <div class="entry-title">
          <h4>arkham.build</h4>
          <p class="muted">Deck builder and collection tracker</p>
        </div>
        <span class="pill" :class="state">
          {{
            state === 'connected'
              ? 'Connected'
              : state === 'partial'
                ? 'Needs reconnecting'
                : state === 'unavailable'
                  ? 'Unavailable'
                  : state === 'loading'
                    ? '…'
                    : 'Not connected'
          }}
        </span>
      </div>

      <p v-if="notice" class="banner" :class="notice.kind">{{ notice.text }}</p>
      <p v-if="failure" class="banner bad">{{ failure }}</p>

      <p v-if="state === 'loading'" class="muted">Loading…</p>

      <p v-else-if="state === 'unavailable'" class="banner warn">
        This server has not been given arkham.build client credentials, so there is
        nothing to connect to yet.
      </p>

      <template v-else-if="connection">
        <dl class="facts">
          <dt>Account</dt>
          <dd class="strong">{{ connection.username }}</dd>
          <dt>Connected</dt>
          <dd>{{ when(connection.connectedAt) }}</dd>
          <dt>Authorization expires</dt>
          <dd>{{ when(connection.expiresAt) }}</dd>
        </dl>

        <div class="scopes">
          <p class="label">You have granted</p>
          <ul>
            <li v-for="scope in connection.scopes" :key="scope">{{ describeScope(scope) }}</li>
          </ul>
        </div>

        <div v-if="missingScopes.length" class="scopes">
          <p class="banner warn">
            This app now asks for more than this connection was given. Reconnect to
            grant it — what you have already granted keeps working until you do.
          </p>
          <p class="label">Still to grant</p>
          <ul>
            <li v-for="scope in missingScopes" :key="scope">{{ describeScope(scope) }}</li>
          </ul>
        </div>

        <div class="row actions">
          <button v-if="missingScopes.length" type="button" :disabled="working" @click="connect">
            {{ working ? 'Opening arkham.build…' : 'Reconnect' }}
          </button>
          <button type="button" class="quiet" :disabled="working" @click="verify">
            {{ working ? 'Checking…' : 'Check connection' }}
          </button>
          <button type="button" class="quiet danger" :disabled="working" @click="disconnect">
            Disconnect
          </button>
        </div>
      </template>

      <template v-else>
        <div class="scopes">
          <p class="label">Connecting will ask arkham.build to let this app</p>
          <ul>
            <li v-for="scope in requestedScopes" :key="scope">{{ describeScope(scope) }}</li>
          </ul>
        </div>
        <p class="muted fine">
          Your arkham.build password never reaches us. We are never granted the ability
          to delete a deck.
        </p>
        <div class="row actions">
          <button type="button" :disabled="working" @click="connect">
            {{ working ? 'Opening arkham.build…' : 'Connect arkham.build' }}
          </button>
        </div>
      </template>
    </article>
  </section>
</template>

<style scoped lang="scss">
/* The panel is its own component, so SettingsForm's scoped heading styles do not
   reach it; the group title and entry titles are restated to match the rest of
   the page. */
.connections {
  gap: 1rem;
}

h3 {
  color: var(--title);
  font-family: teutonic, sans-serif;
  font-size: 1.4em;
  font-weight: bold;
  margin: 0;
  text-transform: uppercase;
}

h4 {
  color: var(--title);
  font-family: teutonic, sans-serif;
  font-size: 1.2em;
  font-weight: bold;
  margin: 0;
  text-transform: uppercase;
}

.group-head {
  display: flex;
  flex-direction: column;
  gap: 0.25rem;
}

/* De-emphasised body copy. One muted tone throughout, so the only things at full
   strength are the headings and the values that answer "what is connected". */
.muted {
  color: var(--title);
  margin: 0;
  opacity: 0.65;
}

.fine {
  font-size: 0.9em;
}

.strong {
  color: var(--text);
  font-weight: 600;
}

.entry {
  border-top: 1px solid var(--box-border);
  display: flex;
  flex-direction: column;
  gap: 0.85rem;
  padding-top: 1rem;
}

.entry-head {
  align-items: center;
  display: flex;
  gap: 0.85rem;
}

.entry-title {
  display: flex;
  flex-direction: column;
  gap: 0.15rem;
  /* Takes the slack so the pill sits hard right. */
  flex: 1;
  min-width: 0;
}

.mark {
  color: var(--title);
  flex: none;
  /* The mark is a finely cut cog; below this its teeth close up into a disc. */
  height: 40px;
  width: 40px;
}


.pill {
  border: 1px solid var(--box-border);
  border-radius: 999px;
  color: var(--title);
  flex: none;
  font-size: 0.75em;
  letter-spacing: 0.04em;
  opacity: 0.8;
  padding: 0.15rem 0.6rem;
  text-transform: uppercase;
  white-space: nowrap;
}

.pill.connected {
  border-color: var(--spooky-green);
  color: var(--spooky-green);
  opacity: 1;
}

.pill.partial {
  border-color: #f5d76e;
  color: #f5d76e;
  opacity: 1;
}

.facts {
  display: grid;
  grid-template-columns: max-content 1fr;
  column-gap: 1.25rem;
  row-gap: 0.35rem;
  margin: 0;
}

.facts dt {
  color: var(--title);
  font-size: 0.9em;
  opacity: 0.6;
}

.facts dd {
  color: var(--title);
  margin: 0;
}

.scopes {
  display: flex;
  flex-direction: column;
  gap: 0.35rem;
}

.label {
  color: var(--title);
  font-size: 0.9em;
  margin: 0;
  opacity: 0.6;
}

.scopes ul {
  color: var(--title);
  display: flex;
  flex-direction: column;
  gap: 0.2rem;
  margin: 0;
  padding-left: 1.1rem;
}

.banner {
  border-left: 3px solid currentColor;
  font-size: 0.95em;
  margin: 0;
  padding: 0.6rem 0.85rem;
}

.banner.good {
  background: #1d2a1d;
  color: var(--spooky-green);
}

.banner.bad {
  background: #2d1a1a;
  color: #f08080;
}

.banner.warn {
  background: #342c14;
  color: #f5d76e;
}

.actions {
  flex-wrap: wrap;
  margin-top: 0.15rem;
}

button {
  width: fit-content;
}

.quiet {
  background: transparent;
  border: 1px solid var(--box-border);
}

.danger:hover:not(:disabled) {
  border-color: var(--delete);
  color: #f08080;
}

@media (max-width: 560px) {
  .entry-head {
    /* The pill would otherwise squeeze the title to a couple of characters. */
    flex-wrap: wrap;
  }

  .entry-title {
    flex-basis: calc(100% - 3rem);
  }

  .facts {
    grid-template-columns: 1fr;
    row-gap: 0;
  }

  .facts dd {
    margin-bottom: 0.5rem;
  }
}
</style>
