<script lang="ts" setup>
/* The marketplace: sets people have published, and the ones you have taken.
 *
 * Importing one takes it into your collection and keeps it subscribed, so the
 * author's later versions can be pulled in. Editing your copy ends that -- what
 * is in it is then not what was published -- and importing again is how you get
 * back to the published version. */
import { computed, ref } from 'vue'
import { useI18n } from 'vue-i18n'
import * as Api from '@/arkham/api'
import { subscribeToSet } from '@/arkham/customCardLibrary'
import { isBadLink, setLinkLabel } from '@/arkham/setLink'
import type { CustomCard } from '@/arkham/customCards'
import { useRoute } from 'vue-router'
import CardOverlay from '@/arkham/components/CardOverlay.vue'
import CustomCardsPage from '@/arkham/components/CustomCardsPage.vue'
import FilterBar from '@/arkham/components/FilterBar.vue'
import MetaChip from '@/arkham/components/MetaChip.vue'
import SetPreview from '@/arkham/components/SetPreview.vue'
import SegmentedToggle from '@/components/SegmentedToggle.vue'
import { useRouter } from 'vue-router'

const { t } = useI18n()
const K = 'customCardSets.'
const router = useRouter()

const openSet = (set: Api.PublishedCardSet) =>
  router.push({ name: 'CardMarketplaceSet', params: { publishedId: set.id } })

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

load()

// ------------------------------------------------------- searching & order ---

const query = ref('')
const order = ref<'newest' | 'liked' | 'name'>('newest')

/* Everything, or only what you have put up. Your own listings are mixed into
 * the marketplace -- they have to be, since this is where you find out what
 * happened to them -- and this is how you look at just them without leaving. */
const scope = ref<'all' | 'mine'>('all')
const scopeOptions = computed(() => [
  { value: 'all' as const, label: t(`${K}scopeAll`) },
  { value: 'mine' as const, label: t(`${K}scopeMine`) },
])
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
  if (matches(set.description, needle)) return true
  return set.preview.some(
    (c) => matches(c.def?.name?.title, needle) || matches(c.def?.name?.subtitle, needle),
  )
}

const listed = computed(() => {
  const needle = query.value.trim().toLowerCase()
  let rows = [...sets.value]
  if (scope.value === 'mine') rows = rows.filter((s) => s.mine)
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

/* A listing is in the marketplace once a version of it has been approved. Your
 * own appear here before that so you can see where they stand, and they are the
 * only ones that can be unapproved -- so these only ever read true on your own. */
const isListed = (set: Api.PublishedCardSet) => set.latestVersion > 0

const awaitingReview = (set: Api.PublishedCardSet) => set.pendingVersion !== null

const denied = (set: Api.PublishedCardSet) => set.reviewStatus === 'denied'

/* Where your own listing stands, in a line. Null for anything already approved
 * and with nothing waiting, which is every listing anyone else sees. */
function reviewLine(set: Api.PublishedCardSet): string | null {
  if (awaitingReview(set)) {
    return isListed(set)
      ? t(`${K}reviewPendingUpdate`, { version: set.pendingVersion, live: set.latestVersion })
      : t(`${K}reviewPending`, { version: set.pendingVersion })
  }
  if (denied(set)) return t(`${K}reviewDeniedShort`)
  return null
}

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

/* Rewriting a listing's blurb. Not a republish: the cards are untouched, so
 * nothing goes back for review and the version people are subscribed to does
 * not move. The server writes the same text onto your own copy of the set, so
 * the next publish does not quietly undo what was typed here. */
const editingId = ref<string | null>(null)
const descriptionDraft = ref('')
const urlDraft = ref('')

function startEdit(set: Api.PublishedCardSet) {
  editingId.value = set.id
  descriptionDraft.value = set.description ?? ''
  urlDraft.value = set.url ?? ''
  status.value = null
  error.value = null
}

/* Only what changed is sent: a key left out keeps what is stored, so saving a
 * blurb cannot blank a link and the other way round. */
async function commitEdit(set: Api.PublishedCardSet) {
  const description = descriptionDraft.value.trim()
  const url = urlDraft.value.trim()
  editingId.value = null
  const redescribed = description !== (set.description ?? '')
  const relinked = url !== (set.url ?? '')
  if (!redescribed && !relinked) return
  error.value = null
  try {
    replace(
      await Api.updatePublishedCardSet(set.id, {
        ...(redescribed ? { description: description || null } : {}),
        ...(relinked ? { url: url || null } : {}),
      }),
    )
    status.value = t(`${K}descriptionSaved`, { name: set.name })
  } catch (e) {
    console.error(e)
    error.value = t(`${K}${isBadLink(e) ? 'setLinkInvalid' : 'descriptionSaveFailed'}`)
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
  <CustomCardsPage
    :title="t(`${K}marketplace`)"
    :lede="t(`${K}marketplaceLede`)"
    :status="status"
    :error="error"
  >
    <p v-if="!loaded" class="muted">{{ t(`${K}loading`) }}</p>
    <p v-else-if="!sets.length" class="empty-state">
      <font-awesome-icon icon="store" />
      <span>{{ t(`${K}marketplaceEmpty`) }}</span>
    </p>

    <template v-else>
      <FilterBar
        v-model="query"
        :placeholder="t(`${K}marketplaceFilterPlaceholder`)"
        :clear-label="t(`${K}clearFilter`)"
      >
        <SegmentedToggle
          v-model="scope"
          :options="scopeOptions"
          :label="t(`${K}scopeLabel`)"
        />
        <SegmentedToggle
          v-model="order"
          :options="orderOptions"
          :label="t(`${K}marketplaceOrderLabel`)"
        />
      </FilterBar>

      <p v-if="author" class="author-filter">
        <span class="chip">
          {{ t(`${K}byThisAuthor`, { author }) }}
          <button
            type="button"
            class="chip-clear"
            :aria-label="t(`${K}clearAuthor`)"
            @click="author = null"
          >
            <font-awesome-icon icon="times" />
          </button>
        </span>
      </p>

      <p v-if="!listed.length" class="empty-state">
        <font-awesome-icon icon="search" />
        <span>{{ t(`${K}marketplaceNoMatches`, { query: query.trim() }) }}</span>
      </p>

      <ul v-else class="listings">
        <li v-for="set in listed" :key="set.id" class="panel">
          <div class="panel-head">
            <div class="about">
              <!-- What the set IS rides with its name: whether the project
                   stands behind it, and which version this is. The rest are
                   facts about it and stay on the line below. -->
              <div class="title-row">
                <h2>
                  <router-link
                    class="set-link"
                    :to="{ name: 'CardMarketplaceSet', params: { publishedId: set.id } }"
                  >
                    {{ set.name }}
                  </router-link>
                </h2>
                <MetaChip
                  v-if="set.official"
                  tone="gold"
                  icon="circle-check"
                  v-tooltip="t(`${K}officialHelp`)"
                >
                  {{ t(`${K}official`) }}
                </MetaChip>
                <MetaChip v-if="isListed(set)">v{{ set.latestVersion }}</MetaChip>
              </div>

              <!-- One line of facts, each its own shape. The author is a button
                   because clicking it filters the page to them. -->
              <div class="facts">
                <button type="button" class="author" @click="author = set.author">
                  {{ t(`${K}byAuthor`, { author: set.author }) }}
                </button>
                <MetaChip>{{ t(`${K}cardCount`, set.cardCount) }}</MetaChip>
                <MetaChip v-if="set.likes" icon="thumbs-up">{{ set.likes }}</MetaChip>
                <MetaChip v-if="isSubscribed(set)" tone="good" icon="circle-check">
                  {{ t(`${K}subscribedBadge`, { version: set.subscribedVersion }) }}
                </MetaChip>
              </div>

              <!-- What the set is. The author's blurb, editable in place by
                   them: it is not part of what was reviewed, so fixing a typo
                   in it should not cost a round through the queue. -->
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
                <input
                  v-model="urlDraft"
                  type="url"
                  inputmode="url"
                  :aria-label="t(`${K}setUrlLabel`)"
                  :placeholder="t(`${K}setUrlPlaceholder`)"
                  @keydown.stop
                  @keydown.esc="editingId = null"
                />
                <div class="describe-actions">
                  <button type="submit" class="go">{{ t(`${K}saveDescription`) }}</button>
                  <button type="button" class="quiet" @click="editingId = null">
                    {{ t(`${K}publishCancel`) }}
                  </button>
                </div>
              </form>
              <template v-else>
                <p v-if="set.description" class="description">{{ set.description }}</p>
                <p v-else-if="set.mine" class="description none">
                  {{ t(`${K}noDescriptionYet`) }}
                </p>
                <!-- Where to read more. The host rather than the whole
                     address: the slugs and dates are nobody's business, and the
                     anchor carries them for anyone who hovers. -->
                <a
                  v-if="set.url"
                  class="site-link"
                  :href="set.url"
                  target="_blank"
                  rel="noopener noreferrer"
                >
                  <font-awesome-icon icon="external-link" />
                  {{ setLinkLabel(set.url) }}
                </a>
                <button
                  v-if="set.mine"
                  type="button"
                  class="link"
                  @click="startEdit(set)"
                >
                  <font-awesome-icon icon="pen" />
                  {{ t(set.description ? `${K}editDescription` : `${K}addDescription`) }}
                </button>
              </template>

              <!-- Only ever on your own listing: nobody else's is shown here
                   until it has passed review. -->
              <p v-if="reviewLine(set)" class="review" :class="{ denied: denied(set) }">
                <font-awesome-icon :icon="denied(set) ? 'circle-xmark' : 'hourglass-half'" />
                <span>
                  {{ reviewLine(set) }}
                  <em v-if="denied(set) && set.denialReason">{{ set.denialReason }}</em>
                </span>
              </p>
            </div>

            <div class="actions">
              <button
                type="button"
                class="like"
                :class="{ on: set.liked }"
                v-tooltip="set.liked ? t(`${K}unlike`) : t(`${K}like`)"
                :aria-label="set.liked ? t(`${K}unlike`) : t(`${K}like`)"
                :aria-pressed="set.liked"
                @click="toggleLike(set)"
              >
                <font-awesome-icon icon="thumbs-up" />
                <span v-if="set.likes">{{ set.likes }}</span>
              </button>
              <!-- There is nothing to import until a version has been approved,
                   which only your own listing can fail to have. -->
              <button
                v-if="isListed(set)"
                type="button"
                class="go"
                :disabled="busy === set.id"
                @click="take(set)"
              >
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
                class="danger"
                :disabled="busy === set.id"
                @click="unlist(set)"
              >
                {{ t(`${K}unpublish`) }}
              </button>
            </div>
          </div>

          <SetPreview
            :cards="inOrder(set.preview)"
            :total="set.cardCount"
            @view-all="openSet(set)"
          />
        </li>
      </ul>
    </template>

    <!-- Document-level: anything carrying `data-image` gets a hover preview. -->
    <template #outside><CardOverlay /></template>
  </CustomCardsPage>
</template>

<style scoped lang="scss">
.muted {
  opacity: 0.65;
}

/* An empty page should say what it is empty of, not just be blank. */
.empty-state {
  align-items: center;
  color: color-mix(in srgb, var(--title) 60%, transparent);
  display: flex;
  flex-direction: column;
  gap: 0.6rem;
  padding: 3rem 1rem;
  text-align: center;

  svg {
    font-size: 1.6rem;
    opacity: 0.4;
  }

  span {
    max-width: 44ch;
  }
}

.author-filter {
  margin: -0.3rem 0 0.8rem;

  .chip {
    align-items: center;
    background: color-mix(in srgb, var(--guardian) 14%, transparent);
    border: 1px solid color-mix(in srgb, var(--guardian) 45%, transparent);
    border-radius: 999px;
    display: inline-flex;
    font-size: 0.78rem;
    gap: 0.2rem;
    padding: 0.1rem 0.25rem 0.1rem 0.7rem;
  }

  /* `padding: 0` is load-bearing: the global button rule's 11px of side padding
     leaves this 22px border-box square no content box at all, so the icon has
     nowhere to draw. */
  .chip-clear {
    background: none;
    border: none;
    color: inherit;
    cursor: pointer;
    display: grid;
    font-size: 0.8rem;
    height: 22px;
    opacity: 0.7;
    padding: 0;
    place-items: center;
    width: 22px;

    &:hover {
      opacity: 1;
    }
  }
}

.listings {
  display: flex;
  flex-direction: column;
  gap: 1rem;
  list-style: none;
  margin: 0;
  padding: 0;
}

/* One panel per listing: a head that says what it is and what you can do with
   it, and the cards underneath. The border lifts on hover so a long page of
   them still reads as a list of separate things. */
.panel {
  background: var(--background-dark);
  border: 1px solid var(--box-border);
  border-radius: 8px;
  overflow: hidden;
  transition: border-color 150ms ease;

  &:hover {
    border-color: color-mix(in srgb, var(--box-border) 40%, var(--background-mid));
  }
}

.panel-head {
  display: flex;
  flex-wrap: wrap;
  gap: 0.75rem 1rem;
  justify-content: space-between;
  padding: 0.85rem 0.9rem 0.6rem;
}

.about {
  flex: 1 1 22rem;
  min-width: 0;

  h2 {
    font-family: teutonic, sans-serif;
    font-size: 1.3em;
    line-height: 1.15;
    margin: 0;
  }
}

/* The chips sit on the title's baseline rather than centred on it: the display
   face is tall, and centring left them floating above its x-height. */
.title-row {
  align-items: baseline;
  column-gap: 0.4rem;
  display: flex;
  flex-wrap: wrap;
  row-gap: 0.25rem;
}

.set-link {
  color: var(--title);
  text-decoration: none;

  &:hover {
    color: white;
    text-decoration: underline;
  }
}

.facts {
  align-items: center;
  display: flex;
  flex-wrap: wrap;
  gap: 0.35rem;
  margin-top: 0.45rem;
}

.author {
  background: none;
  border: none;
  color: color-mix(in srgb, var(--title) 75%, transparent);
  cursor: pointer;
  font-size: 0.78rem;
  padding: 0;
  text-decoration: underline;
  text-decoration-style: dotted;
  text-underline-offset: 2px;

  &:hover {
    color: var(--title);
  }
}

/* What the set is, as against the version note, which is what changed in it.
   Keeps the author's line breaks: a blurb is often a sentence and a list. */
.description {
  font-size: 0.88rem;
  line-height: 1.45;
  margin: 0.55rem 0 0;
  max-width: 64ch;
  white-space: pre-wrap;

  &.none {
    font-style: italic;
    opacity: 0.5;
  }
}

.link {
  align-items: center;
  background: none;
  border: none;
  color: color-mix(in srgb, var(--title) 65%, transparent);
  cursor: pointer;
  display: inline-flex;
  font-size: 0.76rem;
  gap: 0.35rem;
  margin-top: 0.3rem;
  min-height: 28px;
  padding: 0;

  svg {
    font-size: 0.8em;
  }

  &:hover {
    color: var(--title);
    text-decoration: underline;
  }
}

/* The set's own page, which is the one thing a listing cannot say for itself. */
.site-link {
  align-items: center;
  color: var(--spooky-green);
  display: inline-flex;
  font-size: 0.8rem;
  gap: 0.35rem;
  margin: 0.4rem 0.6rem 0 0;
  text-decoration: none;
  word-break: break-all;

  &:hover {
    text-decoration: underline;
  }
}

.describe-form {
  display: flex;
  flex-direction: column;
  gap: 0.4rem;
  margin-top: 0.55rem;
  max-width: 64ch;

  textarea,
  input {
    background: var(--background);
    border: 1px solid var(--box-border);
    border-radius: 5px;
    color: var(--title);
    font-family: inherit;
    font-size: 0.88rem;
    line-height: 1.45;
    padding: 0.45rem 0.55rem;
    resize: vertical;
    width: 100%;

    &:focus {
      border-color: var(--spooky-green);
      outline: none;
    }
  }

  .describe-actions {
    display: flex;
    gap: 0.4rem;
  }
}

/* Only ever on your own listing. A denial carries the reason underneath, which
   is the only part of this a reader has to act on. */
.review {
  align-items: flex-start;
  color: color-mix(in srgb, var(--title) 72%, transparent);
  display: flex;
  font-size: 0.8rem;
  gap: 0.45rem;
  margin: 0.55rem 0 0;

  svg {
    margin-top: 0.2rem;
    opacity: 0.8;
  }

  em {
    display: block;
    font-style: italic;
    opacity: 0.9;
  }

  &.denied {
    color: color-mix(in srgb, var(--survivor) 65%, white);
  }
}

.actions {
  align-items: flex-start;
  display: flex;
  flex: 0 0 auto;
  flex-wrap: wrap;
  gap: 0.4rem;

  /* Full width below the blurb on a phone, where squeezing three buttons into
     the corner of a wrapped row put them on top of the text. */
  @media (max-width: 640px) {
    flex: 1 1 100%;
  }
}

/* One button shape for the section, in three weights: `go` is the thing the
   panel is for, `quiet` backs out of it, `danger` undoes it. */
:deep(button.go),
button.go,
button.quiet,
button.danger,
.like {
  align-items: center;
  border-radius: 5px;
  cursor: pointer;
  display: inline-flex;
  font-size: 0.82rem;
  gap: 0.35rem;
  justify-content: center;
  min-height: 34px;
  padding: 0 0.8rem;
  transition: background 120ms ease, border-color 120ms ease;

  &:disabled {
    cursor: not-allowed;
    opacity: 0.5;
  }
}

button.go {
  background: var(--button-1);
  border: 1px solid transparent;
  color: white;

  &:hover:not(:disabled) {
    background: var(--button-1-highlight);
  }
}

button.quiet {
  background: none;
  border: 1px solid var(--box-border);
  color: var(--title);

  &:hover:not(:disabled) {
    border-color: var(--background-mid);
  }
}

button.danger {
  background: none;
  border: 1px solid color-mix(in srgb, var(--delete) 55%, transparent);
  color: color-mix(in srgb, var(--delete) 40%, white);

  &:hover:not(:disabled) {
    background: color-mix(in srgb, var(--delete) 18%, transparent);
    border-color: var(--delete);
  }
}

.like {
  background: none;
  border: 1px solid var(--box-border);
  color: var(--title);
  font-variant-numeric: tabular-nums;

  &:hover {
    border-color: var(--background-mid);
  }

  &.on {
    border-color: color-mix(in srgb, var(--spooky-green) 70%, transparent);
    color: var(--spooky-green);
  }
}
</style>
