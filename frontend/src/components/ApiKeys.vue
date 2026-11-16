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
import { useI18n } from 'vue-i18n'
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

const { t } = useI18n()

const SCOPES = computed(() => [
  { id: 'cards:read', label: t('apiKeys.scopeRead'), hint: t('apiKeys.scopeReadHint') },
  { id: 'cards:write', label: t('apiKeys.scopeWrite'), hint: t('apiKeys.scopeWriteHint') },
])

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
    failure.value = t('apiKeys.loadFailed')
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
    failure.value = errorMessage(e) ?? t('apiKeys.createFailed')
  } finally {
    creating.value = false
  }
}

async function revoke(key: ApiKey) {
  if (!window.confirm(t('apiKeys.revokeConfirm', { name: key.name }))) return
  failure.value = null
  try {
    await api.delete(`api-keys/${key.id}`)
    await load()
  } catch (e) {
    failure.value = errorMessage(e) ?? t('apiKeys.revokeFailed')
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
  if (key.revokedAt) return t('apiKeys.revokedOn', { date: when(key.revokedAt) })
  if (key.expiresAt && new Date(key.expiresAt) <= new Date()) return t('apiKeys.expiredOn', { date: when(key.expiresAt) })
  return key.lastUsedAt ? t('apiKeys.lastUsed', { date: when(key.lastUsedAt) }) : t('apiKeys.neverUsed')
}

onMounted(load)
</script>

<template>
  <section class="box column">
    <h3>{{ $t('apiKeys.title') }}</h3>
    <p>{{ $t('apiKeys.intro') }}</p>
    <p class="caution">{{ $t('apiKeys.caution') }}</p>

    <div v-if="minted" class="minted">
      <h4>{{ $t('apiKeys.ready', { name: minted.name }) }}</h4>
      <p>{{ $t('apiKeys.shownOnce') }}</p>
      <code>{{ minted.key }}</code>
      <div class="row">
        <button type="button" @click="copyKey">{{ copied ? $t('apiKeys.copied') : $t('apiKeys.copy') }}</button>
        <button type="button" class="quiet" @click="minted = null">{{ $t('apiKeys.done') }}</button>
      </div>
      <p class="hint">{{ $t('apiKeys.sendItAs') }} <code>Authorization: Bearer {{ minted.key.slice(0, 9) }}…</code></p>
    </div>

    <form class="column create" @submit.prevent="create">
      <label>
        {{ $t('apiKeys.whatFor') }}
        <input v-model="name" :placeholder="$t('apiKeys.namePlaceholder')" maxlength="80" />
      </label>

      <fieldset class="column">
        <legend>{{ $t('apiKeys.whatMayItDo') }}</legend>
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
        {{ $t('apiKeys.expiresAfter') }}
        <input v-model="expiresInDays" type="number" min="1" :placeholder="$t('apiKeys.never')" />
      </label>

      <button type="submit" :disabled="!canCreate || creating">
        {{ creating ? $t('apiKeys.creating') : $t('apiKeys.createKey') }}
      </button>
    </form>

    <p v-if="failure" class="failure">{{ failure }}</p>

    <div v-if="loading">{{ $t('apiKeys.loading') }}</div>
    <table v-else-if="keys.length" class="keys">
      <thead>
        <tr><th>{{ $t('apiKeys.colName') }}</th><th>{{ $t('apiKeys.colKey') }}</th><th>{{ $t('apiKeys.colScopes') }}</th><th>{{ $t('apiKeys.colStatus') }}</th><th></th></tr>
      </thead>
      <tbody>
        <tr v-for="key in keys" :key="key.id" :class="{ dead: !!key.revokedAt }">
          <td>{{ key.name }}</td>
          <td><code>{{ key.prefix }}…</code></td>
          <td>{{ key.scopes.join(', ') || $t('apiKeys.none') }}</td>
          <td>{{ status(key) }}</td>
          <td>
            <button v-if="!key.revokedAt" type="button" class="quiet" @click="revoke(key)">
              {{ $t('apiKeys.revoke') }}
            </button>
          </td>
        </tr>
      </tbody>
    </table>
    <p v-else class="hint">{{ $t('apiKeys.noKeys') }}</p>
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
