<script lang="ts" setup>
/* Everything you have put in the marketplace, and what has become of it.
 *
 * The marketplace itself mixes your listings in with everyone else's, which is
 * right for browsing and wrong for keeping track: a listing outlives the set it
 * was published from, so one whose set you have since deleted is only reachable
 * from here, and a version that was turned down is not shown to anyone else at
 * all. This is the page that answers "what is out there under my name". */
import { computed, ref } from 'vue'
import { useI18n } from 'vue-i18n'
import * as Api from '@/arkham/api'
import type { CustomCard } from '@/arkham/customCards'
import CardOverlay from '@/arkham/components/CardOverlay.vue'
import CustomCardsPage from '@/arkham/components/CustomCardsPage.vue'
import MetaChip from '@/arkham/components/MetaChip.vue'
import SetPreview from '@/arkham/components/SetPreview.vue'
import { useRouter } from 'vue-router'
import { isBadLink, setLinkLabel } from '@/arkham/setLink'

const { t } = useI18n()
const K = 'customCardSets.'
const router = useRouter()

const openSet = (set: Api.PublishedCardSet) =>
  router.push({ name: 'CardMarketplaceSet', params: { publishedId: set.id } })

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

load()

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

/* Where the listing stands, as a chip: a couple of words, because the version
 * table underneath says the rest. Said for every row, not only the troubled
 * ones -- a page about your listings that went blank for the healthy ones would
 * be hiding the answer it exists to give.
 *
 * Waiting beats listed when both are true: the thing you came to check on is
 * the one still in somebody's queue. */
function standing(set: Api.PublishedCardSet): string {
  if (set.pendingVersion !== null) return t(`${K}pendingChip`, { version: set.pendingVersion })
  if (set.reviewStatus === 'denied') return t(`${K}deniedChip`)
  if (isListed(set)) return t(`${K}listedChip`, { version: set.latestVersion })
  return t(`${K}unlistedChip`)
}

function standingIcon(set: Api.PublishedCardSet) {
  if (set.pendingVersion !== null) return 'hourglass-half'
  if (set.reviewStatus === 'denied') return 'circle-xmark'
  return isListed(set) ? 'store' : 'circle-question'
}

function standingTone(set: Api.PublishedCardSet) {
  if (set.pendingVersion !== null) return 'warn' as const
  if (set.reviewStatus === 'denied') return 'bad' as const
  return isListed(set) ? ('good' as const) : ('plain' as const)
}

function versionTone(v: Api.PublishedCardSetVersionSummary) {
  if (v.live) return 'good' as const
  if (v.status === 'denied') return 'bad' as const
  return 'warn' as const
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
    :title="t(`${K}myListings`)"
    :lede="t(`${K}myListingsLede`)"
    :status="status"
    :error="error"
  >
    <p v-if="!loaded" class="muted">{{ t(`${K}loading`) }}</p>
    <p v-else-if="!ordered.length" class="empty-state">
      <font-awesome-icon icon="rectangle-list" />
      <span>{{ t(`${K}myListingsEmpty`) }}</span>
      <router-link class="go-link" :to="{ name: 'CardBuilder' }">
        {{ t(`${K}mySets`) }}
        <font-awesome-icon icon="chevron-right" />
      </router-link>
    </p>

    <ul v-else class="listings">
      <li v-for="set in ordered" :key="set.id" class="panel">
        <div class="panel-head">
          <div class="about">
            <h2>
              <router-link
                class="set-link"
                :to="{ name: 'CardMarketplaceSet', params: { publishedId: set.id } }"
              >
                {{ set.name }}
              </router-link>
            </h2>

            <div class="facts">
              <!-- Said for every listing, not only the troubled ones: this page
                   exists to answer "where does this stand", so a row that said
                   nothing would be hiding the answer. -->
              <MetaChip :tone="standingTone(set)" :icon="standingIcon(set)">
                {{ standing(set) }}
              </MetaChip>
              <MetaChip>{{ t(`${K}cardCount`, set.cardCount) }}</MetaChip>
              <MetaChip icon="thumbs-up">{{ set.likes }}</MetaChip>
              <MetaChip>{{ t(`${K}updatedAt`, { date: when(set.updatedAt) }) }}</MetaChip>
            </div>

            <p v-if="set.denialReason" class="denial">
              <font-awesome-icon icon="circle-xmark" />
              <span>{{ set.denialReason }}</span>
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
              <p v-else class="description none">{{ t(`${K}noDescriptionYet`) }}</p>
              <!-- The host rather than the whole address: the slugs and dates
                   are nobody's business, and the anchor carries them. -->
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
              <button type="button" class="link" @click="startEdit(set)">
                <font-awesome-icon icon="pen" />
                {{ t(set.description ? `${K}editDescription` : `${K}addDescription`) }}
              </button>
            </template>
          </div>

          <div class="actions">
            <button
              type="button"
              class="danger"
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
          <li v-for="v in set.versions" :key="v.version">
            <span class="v">v{{ v.version }}</span>
            <MetaChip :tone="versionTone(v)">{{ versionLabel(v) }}</MetaChip>
            <span class="date">{{ when(v.createdAt) }}</span>
            <span v-if="v.note" class="vnote">{{ v.note }}</span>
            <em v-if="v.reason" class="vreason">{{ v.reason }}</em>
          </li>
        </ol>

        <SetPreview
          :cards="inOrder(set.preview)"
          :total="set.cardCount"
          @view-all="openSet(set)"
        />
      </li>
    </ul>

    <!-- Document-level: anything carrying `data-image` gets a hover preview. -->
    <template #outside><CardOverlay /></template>
  </CustomCardsPage>
</template>

<style scoped lang="scss">
.muted {
  opacity: 0.65;
}

.empty-state {
  align-items: center;
  color: color-mix(in srgb, var(--title) 60%, transparent);
  display: flex;
  flex-direction: column;
  gap: 0.75rem;
  padding: 3rem 1rem;
  text-align: center;

  > svg {
    font-size: 1.6rem;
    opacity: 0.4;
  }

  span {
    max-width: 46ch;
  }
}

.go-link {
  align-items: center;
  border: 1px solid var(--box-border);
  border-radius: 5px;
  color: var(--title);
  display: inline-flex;
  font-size: 0.82rem;
  gap: 0.4rem;
  min-height: 34px;
  padding: 0 0.8rem;
  text-decoration: none;

  svg {
    font-size: 0.7em;
  }

  &:hover {
    border-color: var(--spooky-green);
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

.panel {
  background: var(--background-dark);
  border: 1px solid var(--box-border);
  border-radius: 8px;
  overflow: hidden;
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

.denial {
  align-items: baseline;
  color: color-mix(in srgb, var(--survivor) 65%, white);
  display: flex;
  font-size: 0.82rem;
  font-style: italic;
  gap: 0.45rem;
  margin: 0.5rem 0 0;
  max-width: 64ch;

  svg {
    flex: none;
    font-size: 0.85em;
  }
}

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
button.danger {
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

/* The history, as lines rather than a table: most sets have two or three
   versions, and a table of three rows is heavier than what it holds. */
.versions {
  background: color-mix(in srgb, black 14%, transparent);
  border-top: 1px solid var(--box-border);
  display: flex;
  flex-direction: column;
  gap: 0.35rem;
  list-style: none;
  margin: 0;
  padding: 0.65rem 0.9rem;

  > li {
    align-items: center;
    display: flex;
    flex-wrap: wrap;
    font-size: 0.78rem;
    gap: 0.5rem;
  }

  .v {
    font-variant-numeric: tabular-nums;
    min-width: 2.2rem;
    opacity: 0.85;
  }

  .date {
    opacity: 0.5;
  }

  .vnote {
    flex: 1 1 14rem;
    min-width: 0;
    opacity: 0.8;
  }

  .vreason {
    color: color-mix(in srgb, var(--survivor) 65%, white);
    flex: 1 1 100%;
    font-style: italic;
    padding-left: 2.7rem;
  }
}
</style>
