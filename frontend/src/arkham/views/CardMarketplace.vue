<script lang="ts" setup>
/* The marketplace: sets people have published, and the ones you have taken.
 *
 * Importing one takes it into your collection and keeps it subscribed, so the
 * author's later versions can be pulled in. Editing your copy ends that -- what
 * is in it is then not what was published -- and importing again is how you get
 * back to the published version.
 *
 * Behind the dev flag, like everything else about publishing so far. */
import { computed, onMounted, ref } from 'vue'
import { useI18n } from 'vue-i18n'
import * as Api from '@/arkham/api'
import { isDevBuild } from '@/arkham/displayRules'
import { subscribeToSet } from '@/arkham/customCardLibrary'
import type { CustomCard } from '@/arkham/customCards'
import { useRoute } from 'vue-router'
import CardOverlay from '@/arkham/components/CardOverlay.vue'
import CardSetStrip from '@/arkham/components/CardSetStrip.vue'
import SegmentedToggle from '@/components/SegmentedToggle.vue'

const { t } = useI18n()
const K = 'customCardSets.'

const dev = isDevBuild()
const sets = ref<Api.PublishedCardSet[]>([])
const loaded = ref(false)
const busy = ref<string | null>(null)
const status = ref<string | null>(null)
const error = ref<string | null>(null)

async function load() {
  error.value = null
  try {
    sets.value = await Api.fetchPublishedCardSets()
  } catch (e) {
    console.error(e)
    error.value = t(`${K}marketplaceLoadFailed`)
  } finally {
    loaded.value = true
  }
}

onMounted(() => {
  if (dev) load()
})

// ------------------------------------------------------- searching & order ---

const query = ref('')
const order = ref<'newest' | 'liked' | 'name'>('newest')
const orderOptions = computed(() => [
  { value: 'newest' as const, label: t(`${K}orderNewest`) },
  { value: 'liked' as const, label: t(`${K}orderLiked`) },
  { value: 'name' as const, label: t(`${K}orderAlphabetical`) },
])

/* Clicking an author narrows the list to them. Held as the name rather than an
 * id because a username is already unique, and it is what the chip has to say.
 * Seeded from the url so a set's page can link back here filtered to its author. */
const route = useRoute()
const author = ref<string | null>(
  typeof route.query.author === 'string' ? route.query.author : null,
)

const matches = (text: string | null | undefined, needle: string) =>
  !!text && text.toLowerCase().includes(needle)

/* Printed order. The server snapshots in this order too, so this only matters for
 * versions published before it did -- but those are the ones already out there. */
const inOrder = (cards: { def: any; art: string | null }[]) =>
  [...cards].sort((a, b) =>
    (a.def?.meta?.number ?? '').localeCompare(b.def?.meta?.number ?? '', undefined, {
      numeric: true,
    }),
  ) as CustomCard[]

/* A listing matches on its own name, its author, or any card it is showing. The
 * preview is only the first few cards, so this searches what is on screen rather
 * than claiming to search the whole set. */
function hits(set: Api.PublishedCardSet, needle: string) {
  if (matches(set.name, needle) || matches(set.author, needle)) return true
  return set.preview.some(
    (c) => matches(c.def?.name?.title, needle) || matches(c.def?.name?.subtitle, needle),
  )
}

const listed = computed(() => {
  const needle = query.value.trim().toLowerCase()
  let rows = [...sets.value]
  if (author.value) rows = rows.filter((s) => s.author === author.value)
  if (needle) rows = rows.filter((s) => hits(s, needle))
  switch (order.value) {
    case 'liked':
      return rows.sort((a, b) => b.likes - a.likes || b.updatedAt.localeCompare(a.updatedAt))
    case 'name':
      return rows.sort((a, b) => a.name.localeCompare(b.name))
    default:
      /* Yours first: after publishing, your own is what you came to check on.
       * Only without an author filter, where that grouping means something. */
      return rows.sort((a, b) => {
        if (!author.value && a.mine !== b.mine) return a.mine ? -1 : 1
        return b.updatedAt.localeCompare(a.updatedAt)
      })
  }
})

// ------------------------------------------------------------------ actions ---

const isSubscribed = (set: Api.PublishedCardSet) => set.subscribedVersion !== null

const behind = (set: Api.PublishedCardSet) =>
  set.subscribedVersion !== null && set.subscribedVersion < set.latestVersion

/* The listing comes back with its new count, so the row is replaced rather than
 * the whole list reloaded: nothing else about it has changed. */
function replace(updated: Api.PublishedCardSet) {
  sets.value = sets.value.map((s) => (s.id === updated.id ? updated : s))
}

async function toggleLike(set: Api.PublishedCardSet) {
  error.value = null
  try {
    replace(set.liked ? await Api.unlikeCardSet(set.id) : await Api.likeCardSet(set.id))
  } catch (e) {
    console.error(e)
    error.value = t(`${K}likeFailed`)
  }
}

async function take(set: Api.PublishedCardSet) {
  busy.value = set.id
  status.value = null
  error.value = null
  try {
    await subscribeToSet(set.id)
    await load()
    status.value = t(`${K}imported_`, { name: set.name })
  } catch (e) {
    console.error(e)
    error.value = t(`${K}subscribeFailed`)
  } finally {
    busy.value = null
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
    <section class="marketplace">
      <header class="head">
        <div class="titles">
          <h1>{{ t(`${K}marketplace`) }}</h1>
          <p class="lede">{{ t(`${K}marketplaceLede`) }}</p>
        </div>
      </header>

      <p class="experimental">
        <font-awesome-icon icon="flask" />
        {{ t(`${K}experimental`) }}
      </p>

      <p v-if="status" class="status">{{ status }}</p>
      <p v-if="error" class="error">{{ error }}</p>

      <p v-if="!loaded" class="muted">{{ t(`${K}loading`) }}</p>
      <p v-else-if="!sets.length" class="muted empty">{{ t(`${K}marketplaceEmpty`) }}</p>

      <template v-else>
        <div class="browse">
          <div class="set-filter">
            <font-awesome-icon icon="search" />
            <input
              v-model="query"
              type="search"
              :placeholder="t(`${K}marketplaceFilterPlaceholder`)"
              :aria-label="t(`${K}marketplaceFilterPlaceholder`)"
              @keydown.stop
            />
            <button
              v-if="query"
              type="button"
              class="clear"
              v-tooltip="t(`${K}clearFilter`)" :aria-label="t(`${K}clearFilter`)"
              @click="query = ''"
            >
              <font-awesome-icon icon="times" />
            </button>
          </div>
          <SegmentedToggle
            v-model="order"
            class="order"
            :options="orderOptions"
            :label="t(`${K}marketplaceOrderLabel`)"
          />
        </div>

        <p v-if="author" class="author-filter">
          <span class="chip">
            {{ t(`${K}byThisAuthor`, { author }) }}
            <button type="button" class="chip-clear" @click="author = null">×</button>
          </span>
          <button type="button" class="link" @click="author = null">
            {{ t(`${K}clearAuthor`) }}
          </button>
        </p>

        <p v-if="!listed.length" class="muted empty">
          {{ t(`${K}marketplaceNoMatches`, { query: query.trim() }) }}
        </p>

        <ul v-else class="listings">
          <li v-for="set in listed" :key="set.id">
            <div class="listing-head">
              <div class="about">
                <h2>
                  <router-link
                    class="set-link"
                    :to="{ name: 'CardMarketplaceSet', params: { publishedId: set.id } }"
                  >
                    {{ set.name }}
                  </router-link>
                  <small v-if="set.mine" class="mine">{{ t(`${K}yours`) }}</small>
                </h2>
                <p class="meta">
                  <button type="button" class="author" @click="author = set.author">
                    {{ t(`${K}byAuthor`, { author: set.author }) }}
                  </button>
                  ·
                  {{ t(`${K}versionCount`, {
                    version: set.latestVersion,
                    count: t(`${K}cardCount`, set.cardCount),
                  }) }}
                </p>
                <p v-if="set.note" class="note">{{ set.note }}</p>
              </div>

              <div class="actions">
                <button
                  type="button"
                  class="like"
                  :class="{ on: set.liked }"
                  v-tooltip="set.liked ? t(`${K}unlike`) : t(`${K}like`)"
                  :aria-pressed="set.liked"
                  @click="toggleLike(set)"
                >
                  <font-awesome-icon icon="thumbs-up" />
                  <span v-if="set.likes">{{ set.likes }}</span>
                </button>
                <span v-if="isSubscribed(set)" class="subscribed">
                  {{ t(`${K}subscribedBadge`, { version: set.subscribedVersion }) }}
                </span>
                <button type="button" :disabled="busy === set.id" @click="take(set)">
                  {{
                    behind(set)
                      ? t(`${K}updateTo`, { version: set.latestVersion })
                      : isSubscribed(set)
                        ? t(`${K}reimport`)
                        : t(`${K}importToCollection`)
                  }}
                </button>
                <button
                  v-if="set.mine"
                  type="button"
                  class="unlist"
                  :disabled="busy === set.id"
                  @click="unlist(set)"
                >
                  {{ t(`${K}unpublish`) }}
                </button>
              </div>
            </div>

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
      </template>
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

.marketplace {
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

.titles {
  min-width: 0;
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
  border-radius: 6px;
  color: var(--delete);
  display: flex;
  font-size: 0.8rem;
  gap: 0.5rem;
  margin: 0 0 0.9rem;
  padding: 0.45rem 0.65rem;
}

.muted {
  font-size: 0.85rem;
  margin: 0 0 0.75rem;
  opacity: 0.75;
}

.empty {
  border: 1px dashed var(--box-border);
  border-radius: 8px;
  max-width: 46rem;
  padding: 1.25rem 1.5rem;
}

p.status {
  color: var(--spooky-green);
  font-size: 0.85rem;
  margin: 0 0 0.75rem;
}

p.error {
  color: var(--delete);
  font-size: 0.85rem;
  margin: 0 0 0.75rem;
}

.browse {
  align-items: center;
  display: flex;
  flex-wrap: wrap;
  gap: 0.75rem;
  justify-content: flex-end;
  margin-bottom: 0.75rem;
}

.set-filter {
  align-items: center;
  background: var(--background-dark);
  border: 1px solid var(--box-border);
  border-radius: 5px;
  color: var(--title);
  display: flex;
  flex: 0 1 22rem;
  gap: 0.45rem;
  padding: 0.35rem 0.55rem;
  transition: border-color 0.15s ease;

  &:focus-within {
    border-color: var(--spooky-green);
  }

  > svg {
    flex: none;
    font-size: 0.8rem;
    opacity: 0.5;
  }

  input {
    background: none;
    border: none;
    color: var(--title);
    flex: 1 1 auto;
    font-size: 0.85rem;
    min-width: 0;
    outline: none;

    /* The platform's own clear button, which matches nothing else here. */
    &::-webkit-search-cancel-button {
      display: none;
    }
  }

  .clear {
    background: none;
    border: none;
    color: var(--title);
    cursor: pointer;
    flex: none;
    font-size: 0.7rem;
    opacity: 0.5;
    padding: 0;

    &:hover {
      opacity: 1;
    }
  }
}

.order {
  flex: none;
  width: 15rem;
}

.author-filter {
  align-items: center;
  display: flex;
  gap: 0.6rem;
  margin: 0 0 0.75rem;
}

.chip {
  align-items: center;
  background: rgba(196, 181, 253, 0.14);
  border: 1px solid #c4b5fd;
  border-radius: 999px;
  color: #c4b5fd;
  display: flex;
  font-size: 0.78rem;
  gap: 0.35rem;
  padding: 0.15rem 0.4rem 0.15rem 0.65rem;
}

.chip-clear {
  background: none;
  border: none;
  color: inherit;
  cursor: pointer;
  font-size: 0.9rem;
  line-height: 1;
  padding: 0 0.2rem;
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

.listings {
  display: flex;
  flex-direction: column;
  gap: 0.5rem;
  list-style: none;
  margin: 0;
  padding: 0;

  > li {
    background: var(--background-dark);
    border: 1px solid var(--box-border);
    border-radius: 8px;
  }
}

.listing-head {
  align-items: flex-start;
  display: flex;
  flex-wrap: wrap;
  gap: 0.75rem;
  justify-content: space-between;
  padding: 0.75rem;

  h2 {
    align-items: baseline;
    display: flex;
    font-family: teutonic, sans-serif;
    font-size: 1.15em;
    gap: 0.5rem;
    margin: 0;
  }
}

.about {
  min-width: 0;
}

.mine {
  font-family: sans-serif;
  font-size: 0.6em;
  opacity: 0.6;
}

.meta {
  align-items: baseline;
  display: flex;
  flex-wrap: wrap;
  font-size: 0.78rem;
  gap: 0.35rem;
  margin: 0.15rem 0 0;
  opacity: 0.65;
}

/* The author is a way into their other sets, so it looks like one. */
.author {
  background: none;
  border: none;
  color: #c4b5fd;
  cursor: pointer;
  font: inherit;
  padding: 0;

  &:hover {
    text-decoration: underline;
  }
}

.note {
  font-size: 0.82rem;
  margin: 0.35rem 0 0;
  max-width: 60ch;
  opacity: 0.85;
}

.actions {
  align-items: center;
  display: flex;
  flex: none;
  gap: 0.5rem;
}

.like {
  align-items: center;
  display: flex;
  font-variant-numeric: tabular-nums;
  gap: 0.35rem;

  &.on {
    background: rgba(190, 242, 100, 0.14);
    border-color: var(--spooky-green);
    color: var(--spooky-green);
  }
}

/* The green a resolved card code wears: this is the copy you have, confirmed. */
.subscribed {
  border: 1px solid var(--spooky-green);
  border-radius: 999px;
  color: var(--spooky-green);
  font-size: 0.72rem;
  padding: 0.15rem 0.55rem;
  white-space: nowrap;
}

.listing-cards {
  align-items: flex-start;
  border-top: 1px solid var(--box-border);
  display: flex;
  gap: 0.75rem;
  padding: 0.75rem;
}

/* The strip is a grid of `auto-fill` tracks, so as a flex item it has to be told
   to take the room: sized to its content it collapses to a single column. */
.preview {
  flex: 1 1 auto;
  min-width: 0;
}

/* A link rather than a button: it goes somewhere, and the set's own page is
   where the rest of the cards are. */
.view-all {
  background: rgba(255, 255, 255, 0.08);
  border: 1px solid var(--box-border);
  border-radius: 4px;
  color: var(--title);
  flex: none;
  font-size: 0.8rem;
  padding: 0.35rem 0.7rem;
  text-decoration: none;
  white-space: nowrap;

  &:hover {
    background: rgba(255, 255, 255, 0.14);
  }
}

.set-link {
  color: inherit;
  text-decoration: none;

  &:hover {
    color: #c4b5fd;
  }
}

button {
  background: rgba(255, 255, 255, 0.08);
  border: 1px solid var(--box-border);
  border-radius: 4px;
  color: var(--title);
  cursor: pointer;
  font-size: 0.8rem;
  padding: 0.35rem 0.7rem;
  white-space: nowrap;

  &:hover:not(:disabled) {
    background: rgba(255, 255, 255, 0.14);
  }

  &:disabled {
    cursor: default;
    opacity: 0.5;
  }
}

.unlist {
  border-color: transparent;
  color: var(--delete);
}
</style>
