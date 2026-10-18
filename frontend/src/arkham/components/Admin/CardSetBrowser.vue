<script setup lang="ts">
/* Every custom card set on the instance.
 *
 * The review queue beside this one only knows about sets somebody chose to
 * publish. This is the other question -- what is actually being built here --
 * and most sets never reach the queue at all.
 *
 * Paginated rather than listed: this grows with every account. Scoped as well,
 * because most rows are copies -- subscribing to a listing or taking a share
 * link leaves a row per subscriber -- so the default is `originals`, the sets
 * their owner actually made.
 *
 * A row shows a few cards so the list can be skimmed; opening one fetches the
 * rest, the same split the queue makes for the same reason.
 */
import { computed, onMounted, ref, watch } from 'vue'
import { useI18n } from 'vue-i18n'
import * as Api from '@/arkham/api'
import type { CustomCard } from '@/arkham/customCards'
import CardSetStrip from '@/arkham/components/CardSetStrip.vue'
import SegmentedToggle from '@/components/SegmentedToggle.vue'

const { t } = useI18n()
const K = 'adminCardSets.'

type Scope = Api.AdminCardSetScope

const scope = ref<Scope>('originals')
const search = ref('')
const page = ref(1)

const sets = ref<Api.AdminCardSet[]>([])
const total = ref(0)
const perPage = ref(20)
const loading = ref(true)
const error = ref<string | null>(null)

/* Which row is open, and the cards fetched for it. One id rather than a set:
 * looking at a set is one thing at a time, and a set can be a hundred and fifty
 * cards that are not worth holding once you have moved on. */
const openId = ref<string | null>(null)
const openCards = ref<CustomCard[]>([])
const openLoading = ref(false)

const scopeOptions = computed(() =>
  (['originals', 'published', 'copies', 'all'] as Scope[]).map((value) => ({
    value,
    label: t(`${K}scope.${value}`),
  })),
)

const pageCount = computed(() => Math.max(1, Math.ceil(total.value / perPage.value)))

/* The range this page covers, 1-based and inclusive, so the footer can say which
 * of the matching sets are on screen rather than only which page it is. */
const firstOnPage = computed(() => (total.value === 0 ? 0 : (page.value - 1) * perPage.value + 1))
const lastOnPage = computed(() => Math.min(total.value, page.value * perPage.value))

async function load() {
  loading.value = true
  error.value = null
  try {
    const result = await Api.fetchAdminCustomCardSets({
      page: page.value,
      scope: scope.value,
      q: search.value.trim() || undefined,
    })
    sets.value = result.sets
    total.value = result.total
    perPage.value = result.perPage
    /* A filter that shrank the list can leave the page past the end, and an
     * empty page with rows behind it reads as a broken list rather than as a
     * page you walked off. Only ever backwards, so this cannot loop. */
    if (result.sets.length === 0 && page.value > 1) {
      page.value = Math.min(page.value - 1, pageCount.value)
      return load()
    }
  } catch (e) {
    console.error(e)
    error.value = t(`${K}loadFailed`)
  } finally {
    loading.value = false
  }
}

onMounted(load)

/* Every way of changing what is being asked for goes through one of these two,
 * rather than through a watcher on `page`: a watcher would fire on the page
 * reset that a scope change does, and the list would be fetched twice -- twice
 * over, if the two responses came back out of order. */
function goTo(next: number) {
  page.value = next
  openId.value = null
  load()
}

/* Any change to what is being asked for starts again at the first page: page 4
 * of the last search is not page 4 of this one. */
watch(scope, () => goTo(1))

/* Typing is debounced rather than sent per keystroke: the search is a LIKE over
 * two tables, and nobody means the first letter of what they are typing. */
let searchTimer: ReturnType<typeof setTimeout> | undefined
watch(search, () => {
  clearTimeout(searchTimer)
  searchTimer = setTimeout(() => goTo(1), 300)
})

async function toggleOpen(row: Api.AdminCardSet) {
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
    const detail = await Api.fetchAdminCustomCardSet(row.id)
    openCards.value = inOrder(detail.cards)
  } catch (e) {
    console.error(e)
    error.value = t(`${K}cardsFailed`)
  } finally {
    openLoading.value = false
  }
}

/* Printed order, the order the author put them in. The server hands cards back
 * oldest first, because the number they are ordered by lives inside the def. */
const inOrder = (cards: { def: any; art: string | null }[]) =>
  [...cards].sort((a, b) =>
    (a.def?.meta?.number ?? '').localeCompare(b.def?.meta?.number ?? '', undefined, {
      numeric: true,
    }),
  ) as CustomCard[]

const when = (iso: string) => new Date(iso).toLocaleDateString()
</script>

<template>
  <section class="admin-block">
    <header class="section-header">
      <h2>{{ t(`${K}title`) }}</h2>
      <span class="count-badge">{{ total }}</span>
      <SegmentedToggle
        class="scopes"
        :model-value="scope"
        :options="scopeOptions"
        :label="t(`${K}scopeLabel`)"
        @update:model-value="scope = $event as Scope"
      />
      <!-- One box over set name, author and email alike: whoever is being looked
           for may have been mentioned under any of the three. -->
      <label class="search">
        <font-awesome-icon icon="search" />
        <input
          v-model="search"
          type="search"
          :placeholder="t(`${K}searchPlaceholder`)"
          :aria-label="t(`${K}searchLabel`)"
          @keydown.stop
        />
      </label>
    </header>

    <p v-if="error" class="error">{{ error }}</p>

    <p v-if="loading" class="empty box">{{ t(`${K}loading`) }}</p>
    <p v-else-if="!sets.length" class="empty box">
      {{ search.trim() ? t(`${K}emptySearch`, { query: search.trim() }) : t(`${K}empty`) }}
    </p>

    <ul v-else class="sets">
      <li v-for="row in sets" :key="row.id" class="set" :class="{ copy: row.follows !== null }">
        <div class="head">
          <div class="about">
            <h3>
              {{ row.name }}
              <small v-if="row.investigatorExpansion" class="tag box-tag">
                {{ t(`${K}expansion`) }}
              </small>
              <!-- Where this set stands with the marketplace. The approved
                   version is the one people can actually import, which is not
                   always the one last submitted. -->
              <small v-if="row.approvedVersion !== null" class="tag live">
                {{ t(`${K}live`, { version: row.approvedVersion }) }}
              </small>
              <small
                v-else-if="row.submissionStatus === 'pending'"
                class="tag pending"
              >
                {{ t(`${K}awaitingReview`, { version: row.submissionVersion }) }}
              </small>
              <small v-else-if="row.submissionStatus === 'denied'" class="tag denied">
                {{ t(`${K}deniedTag`) }}
              </small>
              <!-- Not the author's own work: a copy of a listing, or of a set
                   somebody handed them a link to. -->
              <small v-if="row.follows" class="tag follows">
                {{ t(`${K}follows.${row.follows}`) }}
              </small>
              <small v-if="row.sourceCode" class="tag imported">
                {{ t(`${K}imported`) }}
              </small>
            </h3>
            <p class="meta">
              <a :href="`mailto:${row.authorEmail}`">{{ row.author }}</a>
              ·
              {{ t(`${K}cardCount`, row.cardCount) }}
              ·
              {{ t(`${K}edited`, { at: when(row.updatedAt) }) }}
            </p>
            <p v-if="row.description" class="note">{{ row.description }}</p>
            <p v-if="row.url" class="source">
              <a :href="row.url" target="_blank" rel="noopener noreferrer">
                <font-awesome-icon icon="external-link" />
                {{ row.url }}
              </a>
            </p>
          </div>

          <div class="actions">
            <button
              type="button"
              class="ghost"
              :disabled="row.cardCount === 0"
              @click="toggleOpen(row)"
            >
              {{ openId === row.id ? t(`${K}hideCards`) : t(`${K}viewCards`) }}
            </button>
          </div>
        </div>

        <div v-if="openId === row.id" class="cards">
          <p v-if="openLoading" class="muted">{{ t(`${K}loadingCards`) }}</p>
          <CardSetStrip v-else wrap :cards="openCards" />
        </div>
        <div v-else-if="row.preview.length" class="cards">
          <CardSetStrip :cards="inOrder(row.preview)" />
        </div>
      </li>
    </ul>

    <!-- Only two buttons: there is no page worth jumping to by number in a list
         ordered by when somebody last touched something.

         A div, not a `<footer>`: base.css styles that element as the page
         footer, fixed to the bottom of the window wherever it appears. -->
    <div v-if="!loading && total > 0" class="pager">
      <button type="button" class="ghost" :disabled="page <= 1" @click="goTo(page - 1)">
        <font-awesome-icon icon="chevron-left" />
        {{ t(`${K}previous`) }}
      </button>
      <span class="range">
        {{ t(`${K}showing`, { first: firstOnPage, last: lastOnPage, total }) }}
      </span>
      <button type="button" class="ghost" :disabled="page >= pageCount" @click="goTo(page + 1)">
        {{ t(`${K}next`) }}
        <font-awesome-icon icon="chevron-right" />
      </button>
    </div>
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

.scopes {
  flex: none;
  min-width: 18rem;
}

/* Takes the rest of the row, and drops to its own line before the toggle does:
   a search box squeezed to four characters is not one. */
.search {
  align-items: center;
  background: rgba(0, 0, 0, 0.25);
  border: 1px solid var(--box-border);
  border-radius: 4px;
  color: color-mix(in srgb, var(--title) 60%, transparent);
  display: flex;
  flex: 1 1 12rem;
  gap: 0.4rem;
  min-width: 0;
  padding: 0 0.5rem;

  &:focus-within {
    border-color: var(--spooky-green);
  }

  input {
    background: none;
    border: none;
    color: var(--title);
    font-family: inherit;
    font-size: 0.85rem;
    min-width: 0;
    padding: 0.35rem 0;
    width: 100%;

    &:focus {
      outline: none;
    }
  }
}

.error {
  background: color-mix(in srgb, var(--survivor) 18%, transparent);
  border-radius: 4px;
  color: color-mix(in srgb, var(--survivor) 75%, white);
  font-size: 0.85rem;
  margin: 0;
  padding: 0.5rem 0.7rem;
}

.empty {
  color: var(--title);
  opacity: 0.75;
}

.sets {
  display: flex;
  flex-direction: column;
  gap: 12px;
  list-style: none;
  margin: 0;
  padding: 0;
}

.set {
  background: color-mix(in srgb, var(--background-dark) 55%, transparent);
  border: 1px solid var(--box-border);
  /* A set somebody made, marked on the edge; a copy is left unmarked, because
     there is nothing of theirs on the row to point at. */
  border-left-width: 4px;
  border-left-color: color-mix(in srgb, var(--spooky-green) 55%, transparent);
  border-radius: 5px;
  color: var(--title);
  display: flex;
  flex-direction: column;
  gap: 10px;
  padding: 12px;

  &.copy {
    border-left-color: color-mix(in srgb, var(--title) 25%, transparent);
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

.tag {
  border: 1px solid var(--box-border);
  border-radius: 999px;
  color: color-mix(in srgb, var(--title) 65%, transparent);
  font-family: "Noto Sans", sans-serif;
  font-size: 0.68rem;
  padding: 0.1rem 0.5rem;
  text-transform: uppercase;
}

.tag.live {
  border-color: color-mix(in srgb, var(--rogue) 60%, transparent);
  color: color-mix(in srgb, var(--rogue) 85%, white);
}

.tag.pending {
  border-color: color-mix(in srgb, var(--spooky-green) 60%, transparent);
  color: color-mix(in srgb, var(--spooky-green) 85%, white);
}

.tag.denied {
  border-color: color-mix(in srgb, var(--survivor) 55%, transparent);
  color: color-mix(in srgb, var(--survivor) 75%, white);
}

.tag.box-tag {
  border-color: color-mix(in srgb, var(--mystic) 60%, transparent);
  color: color-mix(in srgb, var(--mystic) 85%, white);
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

.actions {
  display: flex;
  flex: none;
  flex-wrap: wrap;
  gap: 0.4rem;
}

button {
  align-items: center;
  background: rgba(255, 255, 255, 0.08);
  border: 1px solid var(--box-border);
  border-radius: 4px;
  color: var(--title);
  cursor: pointer;
  display: inline-flex;
  font-size: 0.8rem;
  gap: 0.35rem;
  padding: 0.35rem 0.75rem;

  &:hover:not(:disabled) {
    background: rgba(255, 255, 255, 0.14);
  }

  &:disabled {
    cursor: default;
    opacity: 0.45;
  }
}

button.ghost {
  background: none;
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

.pager {
  align-items: center;
  border-top: 1px solid var(--box-border);
  display: flex;
  gap: 12px;
  justify-content: space-between;
  padding-top: 0.6rem;
}

.range {
  color: color-mix(in srgb, var(--title) 65%, transparent);
  font-size: 0.78rem;
  font-variant-numeric: tabular-nums;
  text-align: center;
}

@media (max-width: 700px) {
  .head {
    flex-direction: column;
  }

  .actions {
    width: 100%;
  }

  .scopes {
    min-width: 0;
    width: 100%;
  }
}
</style>
