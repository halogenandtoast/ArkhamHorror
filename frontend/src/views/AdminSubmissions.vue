<script setup lang="ts">
/* The card marketplace review queue.
 *
 * Publishing a set no longer lists it: it snapshots the set into a version and
 * queues that version here. Nothing is in the marketplace, and nothing can be
 * imported, until a row on this page is approved.
 *
 * A row shows the first few cards so the queue can be skimmed; opening one
 * fetches the rest, because that is what actually reviewing it takes. The
 * decision is final either way -- the handlers refuse a second one -- so the
 * row is replaced with what came back rather than left for another click.
 */
import { computed, onMounted, ref } from 'vue'
import { useI18n } from 'vue-i18n'
import * as Api from '@/arkham/api'
import type { CustomCard } from '@/arkham/customCards'
import CardOverlay from '@/arkham/components/CardOverlay.vue'
import CardSetStrip from '@/arkham/components/CardSetStrip.vue'
import SegmentedToggle from '@/components/SegmentedToggle.vue'

const { t } = useI18n()
const K = 'adminSubmissions.'

type Tab = 'pending' | 'approved' | 'denied' | 'all'

const tab = ref<Tab>('pending')
const rows = ref<Api.CardSetSubmission[]>([])
const loading = ref(true)
const error = ref<string | null>(null)
const status = ref<string | null>(null)

/* Which row is open, and the cards fetched for it. Held as one id rather than a
 * set: reviewing is one thing at a time, and the next set is the next decision. */
const openId = ref<string | null>(null)
const openCards = ref<CustomCard[]>([])
const openLoading = ref(false)

/* Which row a denial is being written for, and the reason. The reason is
 * required -- a denial the author cannot act on is worse than silence -- so the
 * button stays disabled until there is one. */
const denyingId = ref<string | null>(null)
const denyReason = ref('')

const busy = ref<string | null>(null)

const tabOptions = computed(() =>
  (['pending', 'approved', 'denied', 'all'] as Tab[]).map((value) => ({
    value,
    label: t(`${K}tab.${value}`),
  })),
)

const pendingCount = computed(() => rows.value.filter((r) => r.status === 'pending').length)

async function load() {
  loading.value = true
  error.value = null
  try {
    rows.value = await Api.fetchCardSetSubmissions(tab.value)
  } catch (e) {
    console.error(e)
    error.value = t(`${K}loadFailed`)
  } finally {
    loading.value = false
  }
}

onMounted(load)

async function switchTo(next: Tab) {
  tab.value = next
  openId.value = null
  denyingId.value = null
  await load()
}

/* Opening fetches the whole set. Closing keeps nothing: the queue moves on, and
 * a hundred and fifty cards per row is not worth holding. */
async function toggleOpen(row: Api.CardSetSubmission) {
  if (openId.value === row.id) {
    openId.value = null
    openCards.value = []
    return
  }
  openId.value = row.id
  openCards.value = []
  openLoading.value = true
  error.value = null
  try {
    const detail = await Api.fetchCardSetSubmission(row.id)
    openCards.value = inOrder(detail.cards)
  } catch (e) {
    console.error(e)
    error.value = t(`${K}cardsFailed`)
  } finally {
    openLoading.value = false
  }
}

/* Printed order, the order the author put them in. The server snapshots in this
 * order, so this only matters for versions published before it did. */
const inOrder = (cards: { def: any; art: string | null }[]) =>
  [...cards].sort((a, b) =>
    (a.def?.meta?.number ?? '').localeCompare(b.def?.meta?.number ?? '', undefined, {
      numeric: true,
    }),
  ) as CustomCard[]

function replace(updated: Api.CardSetSubmission) {
  // On a filtered tab the row no longer belongs where it was, so it goes rather
  // than sitting under a heading it now contradicts.
  if (tab.value !== 'all' && updated.status !== tab.value) {
    rows.value = rows.value.filter((r) => r.id !== updated.id)
    if (openId.value === updated.id) openId.value = null
    return
  }
  rows.value = rows.value.map((r) => (r.id === updated.id ? updated : r))
}

async function approve(row: Api.CardSetSubmission) {
  busy.value = row.id
  error.value = null
  status.value = null
  try {
    replace(await Api.approveCardSetSubmission(row.id))
    status.value = t(`${K}approved`, { name: row.setName, version: row.version })
  } catch (e) {
    console.error(e)
    error.value = t(`${K}approveFailed`)
  } finally {
    busy.value = null
  }
}

function startDeny(row: Api.CardSetSubmission) {
  denyingId.value = row.id
  denyReason.value = ''
  status.value = null
  error.value = null
}

async function deny(row: Api.CardSetSubmission) {
  const reason = denyReason.value.trim()
  if (!reason) return
  busy.value = row.id
  error.value = null
  status.value = null
  try {
    replace(await Api.denyCardSetSubmission(row.id, reason))
    denyingId.value = null
    status.value = t(`${K}denied`, { name: row.setName, version: row.version })
  } catch (e) {
    console.error(e)
    error.value = t(`${K}denyFailed`)
  } finally {
    busy.value = null
  }
}

const when = (iso: string) => new Date(iso).toLocaleString()

/* Whether this version replaces one already in the marketplace, which changes
 * what approving it means: people subscribed to the live one will be offered it. */
const isUpdate = (row: Api.CardSetSubmission) =>
  row.approvedVersion !== null && row.approvedVersion < row.version
</script>

<template>
  <section class="admin-block">
    <header class="section-header">
      <h2>{{ t(`${K}title`) }}</h2>
      <span v-if="tab === 'pending'" class="count-badge">{{ pendingCount }}</span>
      <SegmentedToggle
        class="tabs"
        :model-value="tab"
        :options="tabOptions"
        :label="t(`${K}filterLabel`)"
        @update:model-value="switchTo($event as Tab)"
      />
    </header>

    <p v-if="status" class="status">{{ status }}</p>
    <p v-if="error" class="error">{{ error }}</p>

    <p v-if="loading" class="empty box">{{ t(`${K}loading`) }}</p>
    <p v-else-if="!rows.length" class="empty box">
      {{ tab === 'pending' ? t(`${K}emptyPending`) : t(`${K}empty`) }}
    </p>

    <ul v-else class="queue">
      <li v-for="row in rows" :key="row.id" class="submission" :class="row.status">
        <div class="head">
          <div class="about">
            <h3>
              {{ row.setName }}
              <small class="version">{{ t(`${K}version`, { version: row.version }) }}</small>
              <small v-if="isUpdate(row)" class="tag update">
                {{ t(`${K}updatesLive`, { version: row.approvedVersion }) }}
              </small>
              <small v-else-if="row.approvedVersion === null" class="tag first">
                {{ t(`${K}firstSubmission`) }}
              </small>
            </h3>
            <p class="meta">
              <a :href="`mailto:${row.authorEmail}`">{{ row.author }}</a>
              ·
              {{ t(`${K}cardCount`, row.cardCount) }}
              ·
              {{ when(row.submittedAt) }}
              <!-- Whether they will hear about this, so the reviewer knows if
                   the reason they write is going anywhere. -->
              <span class="notify" :class="{ off: !row.notify }">
                <font-awesome-icon :icon="row.notify ? 'circle-check' : 'circle-xmark'" />
                {{ row.notify ? t(`${K}willBeEmailed`) : t(`${K}willNotBeEmailed`) }}
              </span>
            </p>
            <p v-if="row.note" class="note">“{{ row.note }}”</p>
            <!-- Where the author says the set lives. Often the only way to
                 check that a set is theirs to publish, so it is the whole
                 address rather than a tidied one. -->
            <p v-if="row.setUrl" class="source">
              <a :href="row.setUrl" target="_blank" rel="noopener noreferrer">
                <font-awesome-icon icon="external-link" />
                {{ row.setUrl }}
              </a>
            </p>
            <p v-if="row.status !== 'pending'" class="decided" :class="row.status">
              {{ row.status === 'approved'
                ? t(`${K}decidedApproved`, { by: row.reviewedBy ?? '—', at: when(row.reviewedAt ?? row.submittedAt) })
                : t(`${K}decidedDenied`, { by: row.reviewedBy ?? '—', at: when(row.reviewedAt ?? row.submittedAt) }) }}
              <em v-if="row.reason">{{ row.reason }}</em>
            </p>
          </div>

          <div class="actions">
            <button type="button" class="ghost" @click="toggleOpen(row)">
              {{ openId === row.id ? t(`${K}hideCards`) : t(`${K}reviewCards`) }}
            </button>
            <template v-if="row.status === 'pending'">
              <button
                type="button"
                class="approve"
                :disabled="busy === row.id"
                @click="approve(row)"
              >
                {{ t(`${K}approve`) }}
              </button>
              <button
                type="button"
                class="deny"
                :disabled="busy === row.id"
                @click="startDeny(row)"
              >
                {{ t(`${K}deny`) }}
              </button>
            </template>
          </div>
        </div>

        <form v-if="denyingId === row.id" class="deny-form" @submit.prevent="deny(row)">
          <label :for="`reason-${row.id}`">{{ t(`${K}reasonLabel`) }}</label>
          <textarea
            :id="`reason-${row.id}`"
            v-model="denyReason"
            rows="3"
            :placeholder="t(`${K}reasonPlaceholder`)"
            @keydown.stop
            @keydown.esc="denyingId = null"
          ></textarea>
          <p class="hint">
            {{ row.notify ? t(`${K}reasonEmailed`) : t(`${K}reasonNotEmailed`) }}
          </p>
          <div class="deny-actions">
            <button type="submit" class="deny" :disabled="!denyReason.trim() || busy === row.id">
              {{ t(`${K}confirmDeny`) }}
            </button>
            <button type="button" class="ghost" @click="denyingId = null">
              {{ t(`${K}cancel`) }}
            </button>
          </div>
        </form>

        <!-- The preview is what the queue shows; the whole set is one click. -->
        <div v-if="openId === row.id" class="cards">
          <p v-if="openLoading" class="muted">{{ t(`${K}loadingCards`) }}</p>
          <CardSetStrip v-else wrap :cards="openCards" />
        </div>
        <div v-else-if="row.preview.length" class="cards">
          <CardSetStrip :cards="inOrder(row.preview)" />
        </div>
      </li>
    </ul>

    <!-- Document-level: anything carrying `data-image` gets a hover preview. -->
    <CardOverlay />
  </section>
</template>

<style scoped lang="scss">
.admin-block {
  background: color-mix(in srgb, var(--background-dark) 42%, transparent);
  border: 1px solid color-mix(in srgb, var(--box-border) 75%, transparent);
  border-radius: 6px;
  box-shadow: 0 8px 20px rgba(0, 0, 0, 0.12);
  display: flex;
  flex-direction: column;
  gap: 10px;
  padding: 14px;
}

.section-header {
  align-items: center;
  display: flex;
  flex-wrap: wrap;
  gap: 12px;
}

.section-header h2 {
  color: var(--title);
  flex: 1;
  font-family: teutonic, sans-serif;
  font-size: 1.6rem;
  line-height: 1;
  margin: 0;
  text-transform: uppercase;
}

.count-badge {
  align-items: center;
  background: var(--background-dark);
  border: 1px solid var(--spooky-green);
  border-left-width: 4px;
  border-radius: 3px;
  color: color-mix(in srgb, var(--spooky-green) 78%, white);
  display: inline-flex;
  font-size: 0.78rem;
  font-weight: 800;
  justify-content: center;
  line-height: 1;
  min-width: 2.1em;
  padding: 5px 9px 5px 7px;
}

.tabs {
  flex: none;
}

.status,
.error {
  border-radius: 4px;
  font-size: 0.85rem;
  margin: 0;
  padding: 0.5rem 0.7rem;
}

.status {
  background: color-mix(in srgb, var(--spooky-green) 18%, transparent);
  color: color-mix(in srgb, var(--spooky-green) 80%, white);
}

.error {
  background: color-mix(in srgb, var(--survivor) 18%, transparent);
  color: color-mix(in srgb, var(--survivor) 75%, white);
}

.empty {
  color: var(--title);
  opacity: 0.75;
}

.queue {
  display: flex;
  flex-direction: column;
  gap: 12px;
  list-style: none;
  margin: 0;
  padding: 0;
}

.submission {
  background: color-mix(in srgb, var(--background-dark) 55%, transparent);
  border: 1px solid var(--box-border);
  /* A thick edge in the decision's colour, so the queue reads at a glance
     without a badge on every row. */
  border-left-width: 4px;
  border-radius: 5px;
  color: var(--title);
  display: flex;
  flex-direction: column;
  gap: 10px;
  padding: 12px;

  &.pending {
    border-left-color: var(--spooky-green);
  }

  &.approved {
    border-left-color: color-mix(in srgb, var(--rogue) 80%, transparent);
  }

  &.denied {
    border-left-color: color-mix(in srgb, var(--survivor) 70%, transparent);
  }
}

.head {
  align-items: flex-start;
  display: flex;
  flex-wrap: wrap;
  gap: 12px;
  justify-content: space-between;
}

.about {
  flex: 1 1 24rem;
  min-width: 0;
}

h3 {
  align-items: baseline;
  display: flex;
  flex-wrap: wrap;
  font-family: teutonic, sans-serif;
  font-size: 1.25rem;
  gap: 0.5rem;
  margin: 0;
}

.version {
  color: color-mix(in srgb, var(--title) 60%, transparent);
  font-family: inherit;
  font-size: 0.8rem;
}

.tag {
  border: 1px solid var(--box-border);
  border-radius: 999px;
  font-family: "Noto Sans", sans-serif;
  font-size: 0.68rem;
  padding: 0.1rem 0.5rem;
  text-transform: uppercase;
}

.tag.update {
  border-color: color-mix(in srgb, var(--spooky-green) 60%, transparent);
  color: color-mix(in srgb, var(--spooky-green) 85%, white);
}

.tag.first {
  color: color-mix(in srgb, var(--title) 65%, transparent);
}

.meta {
  align-items: center;
  color: color-mix(in srgb, var(--title) 70%, transparent);
  display: flex;
  flex-wrap: wrap;
  font-size: 0.78rem;
  gap: 0.4rem;
  margin: 0.3rem 0 0;

  a {
    color: color-mix(in srgb, var(--spooky-green) 80%, white);
  }
}

.notify {
  align-items: center;
  display: inline-flex;
  gap: 0.25rem;

  &.off {
    opacity: 0.6;
  }
}

.note {
  font-size: 0.82rem;
  margin: 0.35rem 0 0;
  max-width: 65ch;
  opacity: 0.85;
}

.source {
  font-size: 0.78rem;
  margin: 0.3rem 0 0;
  overflow-wrap: anywhere;

  a {
    align-items: center;
    color: var(--spooky-green);
    display: inline-flex;
    gap: 0.35rem;
    text-decoration: none;

    &:hover {
      text-decoration: underline;
    }
  }
}

.decided {
  font-size: 0.78rem;
  margin: 0.4rem 0 0;
  max-width: 65ch;

  em {
    display: block;
    font-style: italic;
    margin-top: 0.2rem;
    opacity: 0.9;
  }

  &.approved {
    color: color-mix(in srgb, var(--rogue) 75%, white);
  }

  &.denied {
    color: color-mix(in srgb, var(--survivor) 70%, white);
  }
}

.actions {
  display: flex;
  flex: none;
  flex-wrap: wrap;
  gap: 0.4rem;
}

button {
  background: rgba(255, 255, 255, 0.08);
  border: 1px solid var(--box-border);
  border-radius: 4px;
  color: var(--title);
  cursor: pointer;
  font-size: 0.8rem;
  padding: 0.35rem 0.75rem;

  &:hover:not(:disabled) {
    background: rgba(255, 255, 255, 0.14);
  }

  &:disabled {
    cursor: default;
    opacity: 0.45;
  }
}

button.approve {
  border-color: color-mix(in srgb, var(--spooky-green) 70%, transparent);
  color: color-mix(in srgb, var(--spooky-green) 85%, white);
}

button.deny {
  border-color: color-mix(in srgb, var(--survivor) 55%, transparent);
  color: color-mix(in srgb, var(--survivor) 75%, white);
}

button.ghost {
  background: none;
}

.deny-form {
  border-top: 1px solid var(--box-border);
  display: flex;
  flex-direction: column;
  gap: 0.4rem;
  padding-top: 0.6rem;

  label {
    font-size: 0.78rem;
    font-weight: 700;
    letter-spacing: 0.04em;
    text-transform: uppercase;
  }

  textarea {
    background: rgba(0, 0, 0, 0.25);
    border: 1px solid var(--box-border);
    border-radius: 4px;
    color: var(--title);
    font-family: inherit;
    font-size: 0.85rem;
    padding: 0.4rem 0.5rem;
    resize: vertical;
    width: 100%;
  }

  .hint {
    color: color-mix(in srgb, var(--title) 60%, transparent);
    font-size: 0.74rem;
    margin: 0;
  }
}

.deny-actions {
  display: flex;
  gap: 0.4rem;
}

.cards {
  border-top: 1px solid var(--box-border);
  padding-top: 0.6rem;
}

.muted {
  color: color-mix(in srgb, var(--title) 65%, transparent);
  font-size: 0.82rem;
  margin: 0;
}

@media (max-width: 700px) {
  .head {
    flex-direction: column;
  }

  .actions {
    width: 100%;
  }
}
</style>
