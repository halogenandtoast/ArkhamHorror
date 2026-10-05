<script lang="ts" setup>
/* Everything you have put in the marketplace, and what has become of it.
 *
 * The marketplace itself mixes your listings in with everyone else's, which is
 * right for browsing and wrong for keeping track: a listing outlives the set it
 * was published from, so one whose set you have since deleted is only reachable
 * from here, and a version that was turned down is not shown to anyone else at
 * all. This is the page that answers "what is out there under my name".
 *
 * Not for everyone yet: a dev build, or an admin. */
import { computed, ref, watch } from 'vue'
import { useI18n } from 'vue-i18n'
import * as Api from '@/arkham/api'
import { useMarketplaceVisible } from '@/composable/marketplaceAccess'
import type { CustomCard } from '@/arkham/customCards'
import CardOverlay from '@/arkham/components/CardOverlay.vue'
import CardSetStrip from '@/arkham/components/CardSetStrip.vue'

const { t } = useI18n()
const K = 'customCardSets.'

const visible = useMarketplaceVisible()
const listings = ref<Api.PublishedCardSet[]>([])
const loaded = ref(false)
const busy = ref<string | null>(null)
const status = ref<string | null>(null)
const error = ref<string | null>(null)

async function load() {
  error.value = null
  try {
    listings.value = await Api.fetchPublishedCardSets(true)
  } catch (e) {
    console.error(e)
    error.value = t(`${K}marketplaceLoadFailed`)
  } finally {
    loaded.value = true
  }
}

/* Watched rather than checked once at mount: an admin who opens this page on a
 * cold load has no `isAdmin` until `whoami` answers, and a mount-time check
 * would leave them on "Loading…" for good. */
watch(
  visible,
  (allowed) => {
    if (allowed && !loaded.value) load()
  },
  { immediate: true },
)

const ordered = computed(() =>
  [...listings.value].sort((a, b) => b.updatedAt.localeCompare(a.updatedAt)),
)

/* Printed order. The server snapshots in this order too, so this only matters
 * for versions published before it did -- which are the ones already out there. */
const inOrder = (cards: { def: any; art: string | null }[]) =>
  [...cards].sort((a, b) =>
    (a.def?.meta?.number ?? '').localeCompare(b.def?.meta?.number ?? '', undefined, {
      numeric: true,
    }),
  ) as CustomCard[]

// A listing is in the marketplace once a version of it has been approved.
const isListed = (set: Api.PublishedCardSet) => set.latestVersion > 0

/* Where the listing stands, in a line: what is live, and what is waiting or was
 * turned down. Unlike the marketplace's, this says something for every row --
 * a page about your listings that went blank for the healthy ones would be
 * hiding the answer it exists to give. */
function standing(set: Api.PublishedCardSet): string {
  if (set.pendingVersion !== null) {
    return isListed(set)
      ? t(`${K}reviewPendingUpdate`, { version: set.pendingVersion, live: set.latestVersion })
      : t(`${K}reviewPending`, { version: set.pendingVersion })
  }
  if (set.reviewStatus === 'denied') {
    return isListed(set)
      ? t(`${K}reviewDeniedOverLive`, { live: set.latestVersion })
      : t(`${K}reviewDeniedShort`)
  }
  if (isListed(set)) return t(`${K}reviewListed`, { version: set.latestVersion })
  return t(`${K}reviewNeverSubmitted`)
}

function standingIcon(set: Api.PublishedCardSet) {
  if (set.pendingVersion !== null) return 'hourglass-half'
  if (set.reviewStatus === 'denied') return 'circle-xmark'
  return isListed(set) ? 'store' : 'circle-question'
}

const versionLabel = (v: Api.PublishedCardSetVersionSummary) =>
  v.live
    ? t(`${K}versionLive`)
    : t(`${K}versionStatus.${v.status === 'none' ? 'pending' : v.status}`)

const when = (iso: string) => new Date(iso).toLocaleDateString()

function replace(updated: Api.PublishedCardSet) {
  listings.value = listings.value.map((s) => (s.id === updated.id ? updated : s))
}

/* Rewriting a blurb is not republishing: the cards are untouched, nothing goes
 * back for review, and the version people are subscribed to does not move. The
 * server writes the same text onto your own copy of the set, so the next
 * publish does not quietly undo it. */
const editingId = ref<string | null>(null)
const descriptionDraft = ref('')

function startEdit(set: Api.PublishedCardSet) {
  editingId.value = set.id
  descriptionDraft.value = set.description ?? ''
  status.value = null
  error.value = null
}

async function commitEdit(set: Api.PublishedCardSet) {
  const description = descriptionDraft.value.trim()
  editingId.value = null
  if (description === (set.description ?? '')) return
  error.value = null
  try {
    replace(await Api.updatePublishedCardSet(set.id, description || null))
    status.value = t(`${K}descriptionSaved`, { name: set.name })
  } catch (e) {
    console.error(e)
    error.value = t(`${K}descriptionSaveFailed`)
  }
}

async function unlist(set: Api.PublishedCardSet) {
  if (!confirm(t(`${K}confirmUnpublish`, { name: set.name }))) return
  busy.value = set.id
  status.value = null
  error.value = null
  try {
    await Api.unpublishCardSet(set.id)
    await load()
    status.value = t(`${K}unpublished`, { name: set.name })
  } catch (e) {
    console.error(e)
    error.value = t(`${K}unpublishFailed`)
  } finally {
    busy.value = null
  }
}
</script>

<template>
  <div class="page-container">
    <section class="listings-page">
      <header class="head">
        <div class="titles">
          <h1>{{ t(`${K}myListings`) }}</h1>
          <p class="lede">{{ t(`${K}myListingsLede`) }}</p>
        </div>
      </header>

      <p class="experimental">
        <font-awesome-icon icon="flask" />
        {{ t(`${K}experimental`) }}
      </p>

      <p v-if="status" class="status">{{ status }}</p>
      <p v-if="error" class="error">{{ error }}</p>

      <p v-if="!loaded" class="muted">{{ t(`${K}loading`) }}</p>
      <p v-else-if="!ordered.length" class="muted empty">{{ t(`${K}myListingsEmpty`) }}</p>

      <ul v-else class="listings">
        <li v-for="set in ordered" :key="set.id">
          <div class="listing-head">
            <div class="about">
              <h2>
                <router-link
                  class="set-link"
                  :to="{ name: 'CardMarketplaceSet', params: { publishedId: set.id } }"
                >
                  {{ set.name }}
                </router-link>
              </h2>
              <p class="meta">
                {{ isListed(set)
                  ? t(`${K}versionCount`, {
                      version: set.latestVersion,
                      count: t(`${K}cardCount`, set.cardCount),
                    })
                  : t(`${K}cardCount`, set.cardCount) }}
                · {{ t(`${K}likeCount`, set.likes) }}
                · {{ t(`${K}updatedAt`, { date: when(set.updatedAt) }) }}
              </p>

              <p class="standing" :class="{ denied: set.reviewStatus === 'denied' }">
                <font-awesome-icon :icon="standingIcon(set)" />
                <span>
                  {{ standing(set) }}
                  <em v-if="set.denialReason">{{ set.denialReason }}</em>
                </span>
              </p>

              <!-- The blurb, editable here because this is the page an author
                   comes to when they want to fix what their set says. -->
              <form
                v-if="editingId === set.id"
                class="describe-form"
                @submit.prevent="commitEdit(set)"
              >
                <textarea
                  v-model="descriptionDraft"
                  rows="3"
                  :aria-label="t(`${K}setDescriptionLabel`)"
                  :placeholder="t(`${K}publishDescriptionPlaceholder`)"
                  @keydown.stop
                  @keydown.esc="editingId = null"
                ></textarea>
                <div class="describe-actions">
                  <button type="submit">{{ t(`${K}saveDescription`) }}</button>
                  <button type="button" class="cancel" @click="editingId = null">
                    {{ t(`${K}publishCancel`) }}
                  </button>
                </div>
              </form>
              <template v-else>
                <p v-if="set.description" class="description">{{ set.description }}</p>
                <p v-else class="description muted">{{ t(`${K}noDescriptionYet`) }}</p>
                <button type="button" class="link edit-description" @click="startEdit(set)">
                  {{ t(set.description ? `${K}editDescription` : `${K}addDescription`) }}
                </button>
              </template>
            </div>

            <div class="actions">
              <button
                type="button"
                class="unlist"
                :disabled="busy === set.id"
                @click="unlist(set)"
              >
                {{ t(`${K}unpublish`) }}
              </button>
            </div>
          </div>

          <!-- Every version, and what came of it. Only ever shown to the author,
               which is why it is here and not on the marketplace row. -->
          <ol v-if="set.versions.length" class="versions">
            <li v-for="v in set.versions" :key="v.version" :class="v.status">
              <span class="v">v{{ v.version }}</span>
              <span class="state" :class="{ live: v.live }">{{ versionLabel(v) }}</span>
              <span class="date">{{ when(v.createdAt) }}</span>
              <span v-if="v.note" class="vnote">{{ v.note }}</span>
              <em v-if="v.reason" class="vreason">{{ v.reason }}</em>
            </li>
          </ol>

          <div v-if="set.cardCount" class="listing-cards">
            <CardSetStrip class="preview" :cards="inOrder(set.preview)" />
            <router-link
              class="view-all"
              :to="{ name: 'CardMarketplaceSet', params: { publishedId: set.id } }"
            >
              {{ t(`${K}viewSet`, { count: set.cardCount }) }}
            </router-link>
          </div>
        </li>
      </ul>
    </section>

    <!-- Document-level: anything carrying `data-image` gets a hover preview. -->
    <CardOverlay />
  </div>
</template>

<style scoped lang="scss">
.page-container {
  height: 100%;
  overflow-x: hidden;
  overflow-y: auto;
  width: 100%;
}

.listings-page {
  color: var(--title);
  margin: 0 auto;
  max-width: 1100px;
  padding: 1.5rem;
}

.head {
  margin-bottom: 0.9rem;

  h1 {
    font-family: teutonic, sans-serif;
    font-size: 1.7em;
    margin: 0 0 0.3rem;
  }
}

.lede {
  margin: 0;
  max-width: 62ch;
  opacity: 0.8;
}

.experimental {
  align-items: center;
  background: rgba(200, 60, 60, 0.1);
  border: 1px solid var(--delete);
  border-radius: 4px;
  display: flex;
  font-size: 0.8rem;
  gap: 0.5rem;
  margin: 0 0 1rem;
  padding: 0.5rem 0.75rem;
}

.status,
.error {
  border-radius: 4px;
  font-size: 0.85rem;
  margin: 0 0 0.75rem;
  padding: 0.5rem 0.75rem;
}

.status {
  background: rgba(80, 160, 110, 0.14);
  border: 1px solid var(--spooky-green);
}

.error {
  background: rgba(200, 60, 60, 0.14);
  border: 1px solid var(--delete);
}

.muted {
  opacity: 0.65;
}

.empty {
  padding: 1.5rem 0;
}

.listings {
  display: flex;
  flex-direction: column;
  gap: 1rem;
  list-style: none;
  margin: 0;
  padding: 0;

  > li {
    background: var(--background-dark);
    border: 1px solid var(--box-border);
    border-radius: 6px;
    overflow: hidden;
  }
}

.listing-head {
  display: flex;
  flex-wrap: wrap;
  gap: 0.75rem;
  justify-content: space-between;
  padding: 0.8rem 0.9rem;
}

.about {
  min-width: 0;

  h2 {
    font-family: teutonic, sans-serif;
    font-size: 1.15em;
    margin: 0;
  }
}

.set-link {
  color: var(--title);
  text-decoration: none;

  &:hover {
    text-decoration: underline;
  }
}

.meta {
  font-size: 0.78rem;
  margin: 0.2rem 0 0;
  opacity: 0.7;
}

/* Said for every listing, not only the ones with a problem: this page exists to
   answer "where does this stand", so a blank row would be hiding the answer. */
.standing {
  align-items: flex-start;
  color: color-mix(in srgb, var(--title) 80%, transparent);
  display: flex;
  font-size: 0.8rem;
  gap: 0.45rem;
  margin: 0.4rem 0 0;

  svg {
    margin-top: 0.15rem;
    opacity: 0.8;
  }

  em {
    display: block;
    font-style: italic;
    opacity: 0.9;
  }

  &.denied {
    color: color-mix(in srgb, var(--survivor) 70%, white);
  }
}

.description {
  font-size: 0.86rem;
  margin: 0.45rem 0 0;
  max-width: 62ch;
  white-space: pre-wrap;

  &.muted {
    font-style: italic;
    opacity: 0.55;
  }
}

.link {
  background: none;
  border: none;
  color: var(--title);
  cursor: pointer;
  font-size: 0.78rem;
  opacity: 0.6;
  padding: 0;
  text-decoration: underline;

  &:hover {
    opacity: 1;
  }
}

.edit-description {
  display: inline-block;
  margin-top: 0.2rem;
}

.describe-form {
  display: flex;
  flex-direction: column;
  gap: 0.35rem;
  margin-top: 0.45rem;
  max-width: 62ch;

  textarea {
    background: rgba(0, 0, 0, 0.25);
    border: 1px solid var(--box-border);
    border-radius: 4px;
    color: var(--title);
    font-family: inherit;
    font-size: 0.86rem;
    padding: 0.35rem 0.5rem;
    resize: vertical;
    width: 100%;
  }

  .describe-actions {
    display: flex;
    gap: 0.4rem;

    button {
      font-size: 0.78rem;
      padding: 0.25rem 0.7rem;
    }

    .cancel {
      background: none;
    }
  }
}

.actions {
  align-items: flex-start;
  display: flex;
  gap: 0.4rem;

  button {
    font-size: 0.8rem;
    padding: 0.3rem 0.7rem;
  }
}

.unlist {
  background: none;
  border: 1px solid var(--delete);
  border-radius: 4px;
  color: var(--delete);
  cursor: pointer;

  &:hover:not(:disabled) {
    background: rgba(200, 60, 60, 0.15);
  }
}

/* The history, as a list of lines rather than a table: most sets have two or
   three versions, and a table of three rows is heavier than what it holds. */
.versions {
  border-top: 1px solid var(--box-border);
  display: flex;
  flex-direction: column;
  gap: 0.3rem;
  list-style: none;
  margin: 0;
  padding: 0.6rem 0.9rem;

  > li {
    align-items: baseline;
    display: flex;
    flex-wrap: wrap;
    font-size: 0.78rem;
    gap: 0.5rem;
  }

  .v {
    font-variant-numeric: tabular-nums;
    min-width: 2.5rem;
    opacity: 0.85;
  }

  .state {
    border: 1px solid var(--box-border);
    border-radius: 999px;
    font-size: 0.7rem;
    opacity: 0.8;
    padding: 0.05rem 0.45rem;
    white-space: nowrap;

    &.live {
      border-color: var(--spooky-green);
      color: var(--spooky-green);
      opacity: 1;
    }
  }

  .date {
    opacity: 0.55;
  }

  .vnote {
    flex: 1 1 14rem;
    min-width: 0;
    opacity: 0.8;
  }

  .vreason {
    color: color-mix(in srgb, var(--survivor) 70%, white);
    flex: 1 1 100%;
    font-style: italic;
  }

  > li.denied .v {
    color: color-mix(in srgb, var(--survivor) 70%, white);
  }
}

.listing-cards {
  align-items: center;
  border-top: 1px solid var(--box-border);
  display: flex;
  gap: 0.75rem;
  padding: 0.6rem 0.9rem;
}

.preview {
  flex: 1 1 auto;
  min-width: 0;
}

.view-all {
  color: var(--title);
  flex: none;
  font-size: 0.78rem;
  opacity: 0.7;
  text-decoration: none;
  white-space: nowrap;

  &:hover {
    opacity: 1;
    text-decoration: underline;
  }
}
</style>
