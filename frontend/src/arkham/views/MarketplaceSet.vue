<script lang="ts" setup>
/* One published set, in full.
 *
 * The marketplace's own rows carry a row's worth of cards, which is enough to
 * tell what a set is but not enough to decide on it. This is where every card is,
 * and where the same actions live so a decision made here does not need going
 * back for.
 */
import { computed, ref, watch } from 'vue'
import { useI18n } from 'vue-i18n'
import * as Api from '@/arkham/api'
import { subscribeToSet } from '@/arkham/customCardLibrary'
import { isBadLink, setLinkLabel } from '@/arkham/setLink'
import type { CustomCard } from '@/arkham/customCards'
import CardOverlay from '@/arkham/components/CardOverlay.vue'
import CardSetStrip from '@/arkham/components/CardSetStrip.vue'
import CustomCardsPage from '@/arkham/components/CustomCardsPage.vue'
import FilterBar from '@/arkham/components/FilterBar.vue'
import MetaChip from '@/arkham/components/MetaChip.vue'

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
  loaded.value = false
  try {
    detail.value = await Api.fetchPublishedCardSet(props.publishedId)
  } catch (e) {
    console.error(e)
    detail.value = null
    error.value = t(`${K}setNotFound`)
  } finally {
    loaded.value = true
  }
}

/* Watched rather than loaded once on mount. The route keeps this component
 * alive when one set links to another -- and when the id in the url is simply
 * corrected -- so a mount-time load left the previous set's cards on screen, or
 * the previous set's 404. */
watch(() => props.publishedId, load, { immediate: true })

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

/* A set is in the marketplace once a version of it has been approved. Until then
 * only its author can reach this page at all, and there is nothing to import. */
const isListed = computed(() => (listing.value?.latestVersion ?? 0) > 0)

const denied = computed(() => listing.value?.reviewStatus === 'denied')

/* Where your own set stands, in a line. Null for anything approved with nothing
 * waiting, which is every set anyone else can see. */
const reviewLine = computed(() => {
  const set = listing.value
  if (!set) return null
  if (set.pendingVersion !== null) {
    return isListed.value
      ? t(`${K}reviewPendingUpdate`, { version: set.pendingVersion, live: set.latestVersion })
      : t(`${K}reviewPending`, { version: set.pendingVersion })
  }
  if (denied.value) return t(`${K}reviewDeniedShort`)
  return null
})

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

/* The author rewriting their own blurb, in the place a reader sees it. Not a
 * republish: nothing a reviewer looked at changes, so the version people are
 * subscribed to stays where it is. */
const editing = ref(false)
const descriptionDraft = ref('')
const urlDraft = ref('')

function startEdit() {
  descriptionDraft.value = listing.value?.description ?? ''
  urlDraft.value = listing.value?.url ?? ''
  editing.value = true
  status.value = null
  error.value = null
}

/* Only what changed is sent: a key left out keeps what is stored, so saving a
 * blurb cannot blank a link and the other way round. */
async function commitEdit() {
  const set = listing.value
  if (!set) return
  const description = descriptionDraft.value.trim()
  const url = urlDraft.value.trim()
  editing.value = false
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
  <CustomCardsPage
    :title="detail?.name ?? t(`${K}marketplace`)"
    :experimental="false"
    :status="status"
    :error="error"
  >
    <template #actions>
      <router-link class="back" :to="{ name: 'CardMarketplace' }">
        <font-awesome-icon icon="arrow-left" />
        <span>{{ t(`${K}backToMarketplace`) }}</span>
      </router-link>
    </template>

    <p v-if="!loaded" class="muted">{{ t(`${K}loading`) }}</p>

    <template v-else-if="listing && detail">
      <header class="set-head">
        <div class="about">
          <div class="facts">
            <router-link
              class="author"
              :to="{ name: 'CardMarketplace', query: { author: listing.author } }"
            >
              {{ t(`${K}byAuthor`, { author: listing.author }) }}
            </router-link>
            <MetaChip v-if="listing.mine" tone="mine">{{ t(`${K}yours`) }}</MetaChip>
            <MetaChip
              v-if="listing.official"
              tone="gold"
              icon="circle-check"
              v-tooltip="t(`${K}officialHelp`)"
            >
              {{ t(`${K}official`) }}
            </MetaChip>
            <MetaChip>v{{ detail.version }}</MetaChip>
            <MetaChip>{{ t(`${K}cardCount`, cards.length) }}</MetaChip>
            <MetaChip v-if="listing.likes" icon="thumbs-up">{{ listing.likes }}</MetaChip>
            <MetaChip>{{ t(`${K}publishedAt`, { date: published }) }}</MetaChip>
            <MetaChip v-if="isSubscribed" tone="good" icon="circle-check">
              {{ t(`${K}subscribedBadge`, { version: listing.subscribedVersion }) }}
            </MetaChip>
          </div>

          <!-- The listing's blurb, not the version's: it says what the set is,
               and the note below says what changed in this version. -->
          <form v-if="editing" class="describe-form" @submit.prevent="commitEdit">
            <textarea
              v-model="descriptionDraft"
              rows="4"
              :aria-label="t(`${K}setDescriptionLabel`)"
              :placeholder="t(`${K}publishDescriptionPlaceholder`)"
              @keydown.stop
              @keydown.esc="editing = false"
            ></textarea>
            <input
              v-model="urlDraft"
              type="url"
              inputmode="url"
              :aria-label="t(`${K}setUrlLabel`)"
              :placeholder="t(`${K}setUrlPlaceholder`)"
              @keydown.stop
              @keydown.esc="editing = false"
            />
            <div class="describe-actions">
              <button type="submit" class="go">{{ t(`${K}saveDescription`) }}</button>
              <button type="button" class="quiet" @click="editing = false">
                {{ t(`${K}publishCancel`) }}
              </button>
            </div>
          </form>
          <template v-else>
            <p v-if="listing.description" class="description">{{ listing.description }}</p>
            <p v-else-if="listing.mine" class="description none">
              {{ t(`${K}noDescriptionYet`) }}
            </p>
            <!-- Where to read more: the post it was announced in, the thread
                 it is discussed in. The host rather than the whole address,
                 which the anchor carries anyway. -->
            <a
              v-if="listing.url"
              class="site-link"
              :href="listing.url"
              target="_blank"
              rel="noopener noreferrer"
            >
              <font-awesome-icon icon="external-link" />
              {{ setLinkLabel(listing.url) }}
            </a>
            <button v-if="listing.mine" type="button" class="link" @click="startEdit">
              <font-awesome-icon icon="pen" />
              {{ t(listing.description ? `${K}editDescription` : `${K}addDescription`) }}
            </button>
          </template>

          <p v-if="detail.note" class="note">
            <font-awesome-icon icon="paperclip" />
            <span>{{ detail.note }}</span>
          </p>

          <!-- Only ever on your own set: nobody else can reach one that has
               not been approved. -->
          <p v-if="reviewLine" class="review" :class="{ denied }">
            <font-awesome-icon :icon="denied ? 'circle-xmark' : 'hourglass-half'" />
            <span>
              {{ reviewLine }}
              <em v-if="denied && listing.denialReason">{{ listing.denialReason }}</em>
            </span>
          </p>
        </div>

        <div class="actions">
          <button
            type="button"
            class="like"
            :class="{ on: listing.liked }"
            v-tooltip="listing.liked ? t(`${K}unlike`) : t(`${K}like`)"
            :aria-label="listing.liked ? t(`${K}unlike`) : t(`${K}like`)"
            :aria-pressed="listing.liked"
            @click="toggleLike"
          >
            <font-awesome-icon icon="thumbs-up" />
            <span v-if="listing.likes">{{ listing.likes }}</span>
          </button>
          <!-- Nothing to import until a version has been approved. -->
          <button v-if="isListed" type="button" class="go" :disabled="busy" @click="take">
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

      <FilterBar
        v-model="query"
        :placeholder="t(`${K}setFilterPlaceholder`)"
        :clear-label="t(`${K}clearFilter`)"
      >
        <span v-if="query" class="shown-count">{{ t(`${K}cardCount`, shown.length) }}</span>
      </FilterBar>

      <p v-if="!shown.length" class="empty-state">
        <font-awesome-icon icon="search" />
        <span>{{ t(`${K}noCardMatches`) }}</span>
      </p>
      <CardSetStrip v-else wrap :cards="shown" class="all-cards" />
    </template>

    <!-- Document-level: anything carrying `data-image` gets a hover preview. -->
    <template #outside><CardOverlay /></template>
  </CustomCardsPage>
</template>

<style scoped lang="scss">
.muted {
  opacity: 0.65;
}

/* Back to the list, in the page header's action slot rather than floating above
   the title: it belongs with the page's other top-level controls, and a lone
   pill above a heading reads as something that failed to lay out. */
.back {
  align-items: center;
  border: 1px solid var(--box-border);
  border-radius: 5px;
  color: var(--title);
  display: inline-flex;
  font-size: 0.82rem;
  gap: 0.4rem;
  min-height: 34px;
  padding: 0 0.75rem;
  text-decoration: none;

  &:hover {
    border-color: var(--background-mid);
  }
}

.set-head {
  border-bottom: 1px solid var(--box-border);
  display: flex;
  flex-wrap: wrap;
  gap: 0.75rem 1rem;
  justify-content: space-between;
  margin-bottom: 1.1rem;
  padding-bottom: 1rem;
}

.about {
  flex: 1 1 22rem;
  min-width: 0;
}

.facts {
  align-items: center;
  display: flex;
  flex-wrap: wrap;
  gap: 0.35rem;
}

.author {
  color: color-mix(in srgb, var(--title) 75%, transparent);
  font-size: 0.8rem;
  text-decoration: underline;
  text-decoration-style: dotted;
  text-underline-offset: 2px;

  &:hover {
    color: var(--title);
  }
}

/* What the set is, as against the note, which is what changed in this version. */
.description {
  font-size: 0.95rem;
  line-height: 1.5;
  margin: 0.7rem 0 0;
  max-width: 64ch;
  white-space: pre-wrap;

  &.none {
    font-style: italic;
    opacity: 0.5;
  }
}

.note {
  align-items: baseline;
  color: color-mix(in srgb, var(--title) 70%, transparent);
  display: flex;
  font-size: 0.85rem;
  gap: 0.45rem;
  margin: 0.6rem 0 0;
  max-width: 64ch;

  svg {
    flex: none;
    font-size: 0.8em;
    opacity: 0.7;
  }
}

.link {
  align-items: center;
  background: none;
  border: none;
  color: color-mix(in srgb, var(--title) 65%, transparent);
  cursor: pointer;
  display: inline-flex;
  font-size: 0.78rem;
  gap: 0.35rem;
  margin-top: 0.35rem;
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
  font-size: 0.85rem;
  gap: 0.35rem;
  margin: 0.5rem 0.6rem 0 0;
  text-decoration: none;
  word-break: break-all;

  &:hover {
    text-decoration: underline;
  }
}

.describe-form {
  display: flex;
  flex-direction: column;
  gap: 0.45rem;
  margin-top: 0.7rem;
  max-width: 64ch;

  textarea,
  input {
    background: var(--background-dark);
    border: 1px solid var(--box-border);
    border-radius: 5px;
    color: var(--title);
    font-family: inherit;
    font-size: 0.95rem;
    line-height: 1.5;
    padding: 0.5rem 0.6rem;
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

.review {
  align-items: flex-start;
  color: color-mix(in srgb, var(--title) 72%, transparent);
  display: flex;
  font-size: 0.82rem;
  gap: 0.45rem;
  margin: 0.6rem 0 0;

  svg {
    margin-top: 0.2rem;
    opacity: 0.8;
  }

  em {
    display: block;
    font-style: italic;
  }

  &.denied {
    color: color-mix(in srgb, var(--survivor) 65%, white);
  }
}

.actions {
  align-items: flex-start;
  display: flex;
  flex: 0 0 auto;
  gap: 0.4rem;

  @media (max-width: 640px) {
    flex: 1 1 100%;
  }
}

button.go,
button.quiet,
.like {
  align-items: center;
  border-radius: 5px;
  cursor: pointer;
  display: inline-flex;
  font-size: 0.85rem;
  gap: 0.35rem;
  justify-content: center;
  min-height: 36px;
  padding: 0 0.9rem;
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

.shown-count {
  align-self: center;
  font-size: 0.8rem;
  opacity: 0.6;
  white-space: nowrap;
}

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
}
</style>
