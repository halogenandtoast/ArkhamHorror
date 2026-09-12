<script lang="ts" setup>
/* One published set, in full.
 *
 * The marketplace's own rows carry a row's worth of cards, which is enough to
 * tell what a set is but not enough to decide on it. This is where every card is,
 * and where the same actions live so a decision made here does not need going
 * back for.
 */
import { computed, onMounted, ref } from 'vue'
import { useI18n } from 'vue-i18n'
import * as Api from '@/arkham/api'
import { subscribeToSet } from '@/arkham/customCardLibrary'
import type { CustomCard } from '@/arkham/customCards'
import CardOverlay from '@/arkham/components/CardOverlay.vue'
import CardSetStrip from '@/arkham/components/CardSetStrip.vue'

const props = defineProps<{ publishedId: string }>()

const { t } = useI18n()
const K = 'customCardSets.'

const detail = ref<Api.PublishedCardSetVersion | null>(null)
const loaded = ref(false)
const busy = ref(false)
const status = ref<string | null>(null)
const error = ref<string | null>(null)

async function load() {
  error.value = null
  try {
    detail.value = await Api.fetchPublishedCardSet(props.publishedId)
  } catch (e) {
    console.error(e)
    error.value = t(`${K}setNotFound`)
  } finally {
    loaded.value = true
  }
}

onMounted(load)

const listing = computed(() => detail.value?.listing ?? null)

/* Printed order. A version published before the server started snapshotting them
 * in order still has to read right, so it is sorted here as well. */
const cards = computed(() =>
  [...((detail.value?.cards ?? []) as CustomCard[])].sort((a, b) =>
    (a.def.meta?.number ?? '').localeCompare(b.def.meta?.number ?? '', undefined, {
      numeric: true,
    }),
  ),
)

const query = ref('')

const matches = (text: string | null | undefined, needle: string) =>
  !!text && text.toLowerCase().includes(needle)

const shown = computed(() => {
  const needle = query.value.trim().toLowerCase()
  if (!needle) return cards.value
  return cards.value.filter(
    (c) =>
      matches(c.def.name?.title, needle) ||
      matches(c.def.name?.subtitle, needle) ||
      matches(String(c.def.cardType ?? '').replace(/Type$/, ''), needle) ||
      (c.def.cardTraits ?? []).some((trait: string) => matches(trait, needle)),
  )
})

const isSubscribed = computed(() => listing.value?.subscribedVersion != null)

const behind = computed(
  () =>
    !!listing.value &&
    listing.value.subscribedVersion !== null &&
    listing.value.subscribedVersion < listing.value.latestVersion,
)

const published = computed(() =>
  detail.value ? new Date(detail.value.createdAt).toLocaleDateString() : '',
)

/* The listing comes back from every action with its new counts, so the copy held
 * here is replaced rather than the page reloaded. */
function replace(updated: Api.PublishedCardSet) {
  if (detail.value) detail.value = { ...detail.value, listing: updated }
}

async function toggleLike() {
  if (!listing.value) return
  error.value = null
  try {
    replace(
      listing.value.liked
        ? await Api.unlikeCardSet(listing.value.id)
        : await Api.likeCardSet(listing.value.id),
    )
  } catch (e) {
    console.error(e)
    error.value = t(`${K}likeFailed`)
  }
}

async function take() {
  if (!listing.value) return
  busy.value = true
  status.value = null
  error.value = null
  const name = listing.value.name
  try {
    await subscribeToSet(listing.value.id)
    await load()
    status.value = t(`${K}imported_`, { name })
  } catch (e) {
    console.error(e)
    error.value = t(`${K}subscribeFailed`)
  } finally {
    busy.value = false
  }
}
</script>

<template>
  <div class="page-container">
    <section class="set-page">
      <router-link class="back-button" :to="{ name: 'CardMarketplace' }">
        <font-awesome-icon icon="arrow-left" class="back-icon" />
        <span>{{ t(`${K}backToMarketplace`) }}</span>
      </router-link>

      <p v-if="status" class="status">{{ status }}</p>
      <p v-if="error" class="error">{{ error }}</p>

      <p v-if="!loaded" class="muted">{{ t(`${K}loading`) }}</p>

      <template v-else-if="listing && detail">
        <header class="head">
          <div class="about">
            <h1>
              {{ detail.name }}
              <small v-if="listing.mine" class="mine">{{ t(`${K}yours`) }}</small>
            </h1>
            <p class="meta">
              <router-link
                class="author"
                :to="{ name: 'CardMarketplace', query: { author: listing.author } }"
              >
                {{ t(`${K}byAuthor`, { author: listing.author }) }}
              </router-link>
              ·
              {{ t(`${K}versionCount`, {
                version: detail.version,
                count: t(`${K}cardCount`, cards.length),
              }) }}
              · {{ t(`${K}publishedAt`, { date: published }) }}
            </p>
            <p v-if="detail.note" class="note">{{ detail.note }}</p>
          </div>

          <div class="actions">
            <button
              type="button"
              class="like"
              :class="{ on: listing.liked }"
              v-tooltip="listing.liked ? t(`${K}unlike`) : t(`${K}like`)"
              :aria-pressed="listing.liked"
              @click="toggleLike"
            >
              <font-awesome-icon icon="thumbs-up" />
              <span v-if="listing.likes">{{ listing.likes }}</span>
            </button>
            <span v-if="isSubscribed" class="subscribed">
              {{ t(`${K}subscribedBadge`, { version: listing.subscribedVersion }) }}
            </span>
            <button type="button" :disabled="busy" @click="take">
              {{
                behind
                  ? t(`${K}updateTo`, { version: listing.latestVersion })
                  : isSubscribed
                    ? t(`${K}reimport`)
                    : t(`${K}importToCollection`)
              }}
            </button>
          </div>
        </header>

        <div class="browse">
          <div class="card-filter">
            <font-awesome-icon icon="search" />
            <input
              v-model="query"
              type="search"
              :placeholder="t(`${K}setFilterPlaceholder`)"
              :aria-label="t(`${K}setFilterPlaceholder`)"
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
          <span v-if="query" class="shown-count">
            {{ t(`${K}cardCount`, shown.length) }}
          </span>
        </div>

        <p v-if="!shown.length" class="muted">{{ t(`${K}noCardMatches`) }}</p>
        <CardSetStrip v-else wrap :cards="shown" class="all-cards" />
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

.set-page {
  color: var(--title);
  margin: 0 auto;
  max-width: 1100px;
  padding: 1.5rem;
}

/* The campaign log's back button, unchanged, so leaving a page looks the same
   wherever you are. */
.back-button {
  align-items: center;
  background: rgba(255, 255, 255, 0.05);
  border: 1px solid rgba(255, 255, 255, 0.1);
  border-radius: 8px;
  color: rgba(255, 255, 255, 0.7);
  display: inline-flex;
  font-family: teutonic, sans-serif;
  font-size: 0.95em;
  gap: 8px;
  letter-spacing: 0.06em;
  margin-bottom: 0.9rem;
  padding: 8px 16px;
  text-decoration: none;
  text-transform: uppercase;
  transition: background 0.15s, border-color 0.15s, color 0.15s;

  .back-icon {
    font-size: 0.85em;
    transition: transform 0.15s;
  }

  &:hover {
    background: rgba(255, 255, 255, 0.1);
    border-color: rgba(255, 255, 255, 0.2);
    color: #f0f0f0;

    .back-icon {
      transform: translateX(-3px);
    }
  }
}

.muted {
  font-size: 0.85rem;
  margin: 0 0 0.75rem;
  opacity: 0.75;
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

.head {
  align-items: flex-start;
  border-bottom: 1px solid var(--box-border);
  display: flex;
  flex-wrap: wrap;
  gap: 1rem;
  justify-content: space-between;
  margin-bottom: 1rem;
  padding-bottom: 0.9rem;

  h1 {
    align-items: baseline;
    display: flex;
    font-family: teutonic, sans-serif;
    font-size: 1.7em;
    gap: 0.5rem;
    margin: 0;
  }
}

.about {
  min-width: 0;
}

.mine {
  font-family: sans-serif;
  font-size: 0.5em;
  opacity: 0.6;
}

.meta {
  align-items: baseline;
  display: flex;
  flex-wrap: wrap;
  font-size: 0.8rem;
  gap: 0.35rem;
  margin: 0.25rem 0 0;
  opacity: 0.65;
}

/* The author is a way into their other sets, so it looks like one. */
.author {
  color: #c4b5fd;
  text-decoration: none;

  &:hover {
    text-decoration: underline;
  }
}

.note {
  font-size: 0.85rem;
  margin: 0.5rem 0 0;
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

.browse {
  align-items: center;
  display: flex;
  flex-wrap: wrap;
  gap: 0.75rem;
  margin-bottom: 0.9rem;
}

.card-filter {
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

.shown-count {
  font-size: 0.78rem;
  opacity: 0.6;
}

.all-cards {
  margin-bottom: 1rem;
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
</style>
