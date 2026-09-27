<script lang="ts" setup>
/* Personal API keys, for giving an agent the ability to write custom cards
 * without giving it the account.
 *
 * The account token cannot do that job: it never expires, authorises everything
 * its owner can do — decks, games, deleting the account — and can only be revoked
 * by rotating the server's signing secret, which signs out every user at once.
 * A key carries only the scopes it was granted and can be revoked by itself.
 *
 * The plaintext exists in one response and nowhere else, so it is shown once,
 * conspicuously, with a way to copy it. Coming back for it later is not possible
 * — only its digest is stored — and the panel says so rather than letting someone
 * discover it by looking.
 */
import { computed, onMounted, ref } from 'vue'
import api from '@/api'

type ApiKey = {
  id: string
  name: string
  prefix: string
  scopes: string[]
  lastUsedAt: string | null
  expiresAt: string | null
  revokedAt: string | null
  createdAt: string
}

const SCOPES = [
  { id: 'cards:read', label: 'Read my custom cards', hint: 'List sets, read card definitions.' },
  {
    id: 'cards:write',
    label: 'Create and change my custom cards',
    hint: 'Save cards, delete cards, create/rename/delete sets.',
  },
] as const

const keys = ref<ApiKey[]>([])
const loading = ref(true)
const failure = ref<string | null>(null)

const name = ref('')
const chosenScopes = ref<string[]>(['cards:read'])
const expiresInDays = ref<string>('')
const creating = ref(false)
/* The one time the key is visible. Kept out of the list so it cannot be mistaken
 * for something the list could show again. */
const minted = ref<{ key: string; name: string } | null>(null)
const copied = ref(false)

const canCreate = computed(() => name.value.trim().length > 0 && chosenScopes.value.length > 0)

async function load() {
  loading.value = true
  failure.value = null
  try {
    const { data } = await api.get<ApiKey[]>('api-keys')
    keys.value = data
  } catch {
    failure.value = 'Could not load your keys.'
  } finally {
    loading.value = false
  }
}

async function create() {
  if (!canCreate.value) return
  creating.value = true
  failure.value = null
  try {
    const days = Number.parseInt(expiresInDays.value, 10)
    const { data } = await api.post<{ key: string; name: string }>('api-keys', {
      name: name.value.trim(),
      scopes: chosenScopes.value,
      ...(Number.isFinite(days) && days > 0 ? { expiresInDays: days } : {}),
    })
    minted.value = { key: data.key, name: data.name }
    copied.value = false
    name.value = ''
    expiresInDays.value = ''
    await load()
  } catch (e) {
    failure.value = errorMessage(e) ?? 'Could not create the key.'
  } finally {
    creating.value = false
  }
}

async function revoke(key: ApiKey) {
  if (!window.confirm(`Revoke "${key.name}"? Anything using it stops working immediately.`)) return
  failure.value = null
  try {
    await api.delete(`api-keys/${key.id}`)
    await load()
  } catch (e) {
    failure.value = errorMessage(e) ?? 'Could not revoke the key.'
  }
}

function errorMessage(e: unknown): string | null {
  const response = (e as { response?: { data?: { message?: string; messages?: string[] } } })?.response
  return response?.data?.message ?? response?.data?.messages?.join('; ') ?? null
}

async function copyKey() {
  if (!minted.value) return
  try {
    await navigator.clipboard.writeText(minted.value.key)
    copied.value = true
  } catch {
    copied.value = false
  }
}

function toggleScope(scope: string) {
  chosenScopes.value = chosenScopes.value.includes(scope)
    ? chosenScopes.value.filter((s) => s !== scope)
    : [...chosenScopes.value, scope]
}

function when(value: string | null): string {
  return value ? new Date(value).toLocaleDateString() : '—'
}

function status(key: ApiKey): string {
  if (key.revokedAt) return `revoked ${when(key.revokedAt)}`
  if (key.expiresAt && new Date(key.expiresAt) <= new Date()) return `expired ${when(key.expiresAt)}`
  return key.lastUsedAt ? `last used ${when(key.lastUsedAt)}` : 'never used'
}

onMounted(load)
</script>

<template>
  <section class="box column">
    <h3>API keys</h3>
    <p>
      A key lets a tool — an MCP client, a script — write custom cards on your
      account without signing in as you. It carries only what you grant it, you can
      revoke it on its own, and you can see when it was last used.
    </p>
    <p class="caution">
      Do not paste your login token into a tool instead. It never expires, covers
      everything you can do including deleting your account, and cannot be revoked
      by itself.
    </p>

    <div v-if="minted" class="minted">
      <h4>“{{ minted.name }}” is ready</h4>
      <p>This is the only time it is shown — only a hash of it is stored.</p>
      <code>{{ minted.key }}</code>
      <div class="row">
        <button type="button" @click="copyKey">{{ copied ? 'Copied' : 'Copy' }}</button>
        <button type="button" class="quiet" @click="minted = null">Done</button>
      </div>
      <p class="hint">Send it as <code>Authorization: Bearer {{ minted.key.slice(0, 9) }}…</code></p>
    </div>

    <form class="column create" @submit.prevent="create">
      <label>
        What is it for?
        <input v-model="name" placeholder="Claude on my laptop" maxlength="80" />
      </label>

      <fieldset class="column">
        <legend>What may it do?</legend>
        <label v-for="scope in SCOPES" :key="scope.id" class="check">
          <input
            type="checkbox"
            :checked="chosenScopes.includes(scope.id)"
            @change="toggleScope(scope.id)"
          />
          <span>
            {{ scope.label }}
            <em>{{ scope.hint }}</em>
          </span>
        </label>
      </fieldset>

      <label>
        Expires after (days, optional)
        <input v-model="expiresInDays" type="number" min="1" placeholder="never" />
      </label>

      <button type="submit" :disabled="!canCreate || creating">
        {{ creating ? 'Creating…' : 'Create key' }}
      </button>
    </form>

    <p v-if="failure" class="failure">{{ failure }}</p>

    <div v-if="loading">Loading…</div>
    <table v-else-if="keys.length" class="keys">
      <thead>
        <tr><th>Name</th><th>Key</th><th>Scopes</th><th>Status</th><th></th></tr>
      </thead>
      <tbody>
        <tr v-for="key in keys" :key="key.id" :class="{ dead: !!key.revokedAt }">
          <td>{{ key.name }}</td>
          <td><code>{{ key.prefix }}…</code></td>
          <td>{{ key.scopes.join(', ') || 'none' }}</td>
          <td>{{ status(key) }}</td>
          <td>
            <button v-if="!key.revokedAt" type="button" class="quiet" @click="revoke(key)">
              Revoke
            </button>
          </td>
        </tr>
      </tbody>
    </table>
    <p v-else class="hint">No keys yet.</p>
  </section>
</template>

<style scoped lang="scss">
.caution {
  color: #f5d76e;
  background: #342c14;
  border-left: 3px solid #f5d76e;
  padding: 0.75rem 1rem;
}

.minted {
  border: 1px solid var(--spooky-green);
  border-radius: 4px;
  display: flex;
  flex-direction: column;
  gap: 0.5rem;
  padding: 1rem;

  h4 {
    margin: 0;
  }

  code {
    background: #111827;
    border-radius: 3px;
    display: block;
    overflow-wrap: anywhere;
    padding: 0.5rem;
    user-select: all;
  }
}

.create {
  border-top: 1px solid var(--box-border);
  gap: 0.75rem;
  padding-top: 1rem;
}

fieldset {
  border: 1px solid var(--box-border);
  border-radius: 4px;
  gap: 0.5rem;
}

.check {
  align-items: flex-start;
  display: flex;
  flex-direction: row;
  gap: 0.5rem;

  em {
    color: #9ca3af;
    display: block;
    font-size: 0.85em;
    font-style: normal;
  }
}

.row {
  display: flex;
  gap: 0.5rem;
}

.quiet {
  background: transparent;
  border: 1px solid var(--box-border);
}

.failure {
  color: #f08080;
}

.hint {
  color: #9ca3af;
}

.keys {
  border-collapse: collapse;
  width: 100%;

  th,
  td {
    border-bottom: 1px solid var(--box-border);
    padding: 0.4rem 0.5rem;
    text-align: left;
  }

  .dead {
    opacity: 0.5;
  }
}
</style>
