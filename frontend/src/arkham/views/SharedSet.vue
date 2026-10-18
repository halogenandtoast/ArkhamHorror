<script lang="ts" setup>
/* A set somebody shared the link to.
 *
 * Not a marketplace page: this set has not been published and may never be, so
 * there is no version, no listing and nothing to like -- only the author's cards
 * as they stand this minute, and the one decision worth making about them, which
 * is whether to put them in your own library.
 *
 * Opening this page is what puts you on the author's list of who has the link.
 * There is no invitation to accept, because a share is a url: reading it is the
 * only record there can be.
 */
import { computed, ref, watch } from 'vue'
import { useI18n } from 'vue-i18n'
import * as Api from '@/arkham/api'
import { subscribeToSharedSet } from '@/arkham/customCardLibrary'
import { setLinkLabel } from '@/arkham/setLink'
import { normalizeCardDef, type CustomCard } from '@/arkham/customCards'
import CardListView from '@/arkham/components/CardListView.vue'
import CardOverlay from '@/arkham/components/CardOverlay.vue'
import CardSetStrip from '@/arkham/components/CardSetStrip.vue'
import CustomCardsPage from '@/arkham/components/CustomCardsPage.vue'
import FilterBar from '@/arkham/components/FilterBar.vue'
import MetaChip from '@/arkham/components/MetaChip.vue'
import SegmentedToggle from '@/components/SegmentedToggle.vue'

const props = defineProps<{ token: string }>()

const { t } = useI18n()
const K = 'customCardSets.'

const set = ref<Api.SharedCardSet | null>(null)
const loaded = ref(false)
const busy = ref(false)
const status = ref<string | null>(null)
const error = ref<string | null>(null)
const gone = ref(false)

/* A dead link and a revoked one answer the same way on purpose, and so does the
 * sentence: the author has decided who has this set, and the page is not the
 * place to argue about which of the two it was. */
async function load() {
  error.value = null
  gone.value = false
  loaded.value = false
  try {
    set.value = await Api.fetchSharedCardSet(props.token)
  } catch (e) {
    console.error(e)
    set.value = null
    gone.value = true
  } finally {
    loaded.value = true
  }
}

watch(() => props.token, load, { immediate: true })

/* Printed order -- the order the author put them in, which is the order the set
 * reads in. */
const cards = computed(() =>
  [...((set.value?.cards ?? []) as CustomCard[])].sort((a, b) =>
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

/* Pictures or a table, the way the card browser offers both: the pictures are
 * how a set is browsed, the table is how 200 of them are read. Remembered,
 * because whichever one you want you want it every time. */
type CardView = 'images' | 'list'
const VIEW_KEY = 'arkham:card-builder:card-view'

const view = ref<CardView>(localStorage.getItem(VIEW_KEY) === 'list' ? 'list' : 'images')
watch(view, (value) => localStorage.setItem(VIEW_KEY, value))

const viewOptions = computed(() => [
  { value: 'images' as CardView, label: t(`${K}viewImages`) },
  { value: 'list' as CardView, label: t(`${K}viewList`) },
])

/* The table reads fields a hand-written def may simply not carry -- a card with
 * no skill icons has no `skills` key -- and this payload has not been through
 * the decoder that fills them in. */
const shownDefs = computed(() => shown.value.map((card) => normalizeCardDef(card.def)))

const have = computed(() => !!set.value?.setId)

/* Which of the two you took it as. A subscribed copy is locked and catches up
 * with the author; an imported one is an ordinary set of yours. */
const following = computed(() => have.value && !!set.value?.subscribed)

const changed = computed(() =>
  set.value ? new Date(set.value.updatedAt).toLocaleString() : '',
)

/* Whether there is anything to take, and what the button says about it.
 *
 * Only two states, because a subscribed copy cannot be edited: you do not have
 * it, or the author has moved on since you took it. Subscribing when neither is
 * true would write the same cards over themselves, so there is no button for it
 * -- the "Subscribed" chip is the whole of what there is to say. */
const canTake = computed(() => !following.value || !!set.value?.behind)

const subscribeLabel = computed(() =>
  following.value ? t(`${K}takeSharedUpdate`) : t(`${K}subscribeShared`),
)

async function take() {
  if (!set.value) return
  busy.value = true
  status.value = null
  error.value = null
  const name = set.value.name
  try {
    set.value = await subscribeToSharedSet(props.token)
    status.value = t(`${K}sharedSubscribed`, { name })
  } catch (e) {
    console.error(e)
    error.value = t(`${K}sharedTakeFailed`)
  } finally {
    busy.value = false
  }
}
</script>

<template>
  <CustomCardsPage
    :title="set?.name ?? t(`${K}sharedTitle`)"
    :icon="set?.icon"
    :experimental="false"
    :status="status"
    :error="error"
    wide
  >
    <template #actions>
      <router-link class="back" :to="{ name: 'CardBuilder' }">
        <font-awesome-icon icon="arrow-left" />
        <span>{{ t(`${K}backToSets`) }}</span>
      </router-link>
    </template>

    <p v-if="!loaded" class="muted">{{ t(`${K}loading`) }}</p>

    <!-- A link that has been replaced, or that this account has been removed
         from. One sentence: there is nothing here to act on. -->
    <section v-else-if="gone" class="empty">
      <font-awesome-icon icon="link-slash" />
      <h2>{{ t(`${K}sharedGoneTitle`) }}</h2>
      <p class="lede">{{ t(`${K}sharedGoneLede`) }}</p>
    </section>

    <template v-else-if="set">
      <header class="set-head">
        <div class="about">
          <div class="facts">
            <span class="author">{{ t(`${K}byAuthor`, { author: set.author }) }}</span>
            <MetaChip v-if="set.mine" tone="mine">{{ t(`${K}yours`) }}</MetaChip>
            <MetaChip icon="link">{{ t(`${K}sharedChip`) }}</MetaChip>
            <MetaChip>{{ t(`${K}cardCount`, cards.length) }}</MetaChip>
            <MetaChip>{{ t(`${K}sharedChangedAt`, { date: changed }) }}</MetaChip>
            <MetaChip v-if="following" tone="good" icon="lock">
              {{ t(`${K}sharedFollowing`) }}
            </MetaChip>
          </div>

          <p v-if="set.description" class="description">{{ set.description }}</p>
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

          <!-- An unpublished set, said plainly: these cards are being tried out
               rather than finished, they can change under you, and they stay the
               author's -- which is what the locked copy and the missing export
               button would otherwise leave somebody guessing at. -->
          <p v-if="!set.listing" class="unpublished">
            <font-awesome-icon icon="flask" />
            <span>{{ t(`${K}sharedUnpublished`) }}</span>
          </p>
          <p v-if="!set.mine && !set.listing" class="unpublished">
            <font-awesome-icon icon="lock" />
            <span>{{ t(`${K}sharedStaysTheirs`) }}</span>
          </p>
          <!-- Published since they were given the link, which changes what they
               may do with it: a published set is public, so a copy of it can be
               taken into the editor and exported. Subscribing from the
               marketplace adopts the copy they already have rather than making
               a second, but they have to know to go and do it. -->
          <p v-if="!set.mine && set.listing" class="unpublished">
            <font-awesome-icon icon="store" />
            <span>
              {{ t(`${K}sharedNowPublished`) }}
              <router-link
                :to="{ name: 'CardMarketplaceSet', params: { publishedId: set.listing } }"
              >
                {{ t(`${K}sharedSeeListing`) }}
              </router-link>
            </span>
          </p>

          <p v-if="have && set.behind" class="behind">
            <font-awesome-icon icon="refresh" />
            <span>{{ t(`${K}${following ? 'sharedBehind' : 'sharedBehindImported'}`) }}</span>
          </p>

        </div>

        <div class="actions">
          <!-- Your own copy, where you can change it. The whole point of taking
               a set is being able to build with it, so the way to it is here
               rather than back through the list. -->
          <router-link
            v-if="set.setId"
            class="open-copy"
            :to="{ name: 'CardBuilderSet', params: { setId: set.setId } }"
          >
            <font-awesome-icon icon="layer-group" />
            {{ t(`${K}sharedOpenCopy`) }}
          </router-link>
          <!-- One way to take it, and it follows the author: the copy is locked,
               so it stays what they shared, and this is also how it catches up.
               The marketplace's other way -- a copy of your own, to edit and
               keep -- is not offered, because this set is unpublished and
               passing it on is the author's to do.

               The author following their own link has nothing to take. -->
          <button
            v-if="!set.mine && canTake"
            type="button"
            class="go"
            :disabled="busy || !cards.length"
            @click="take"
          >
            {{ subscribeLabel }}
          </button>
        </div>
      </header>

      <p v-if="!cards.length" class="empty-state">
        <font-awesome-icon icon="layer-group" />
        <span>{{ t(`${K}sharedEmpty`) }}</span>
      </p>

      <template v-else>
        <FilterBar
          v-model="query"
          :placeholder="t(`${K}setFilterPlaceholder`)"
          :clear-label="t(`${K}clearFilter`)"
        >
          <span v-if="query" class="shown-count">{{ t(`${K}cardCount`, shown.length) }}</span>
          <SegmentedToggle v-model="view" :options="viewOptions" :label="t(`${K}viewLabel`)" />
        </FilterBar>

        <p v-if="!shown.length" class="empty-state">
          <font-awesome-icon icon="search" />
          <span>{{ t(`${K}noCardMatches`) }}</span>
        </p>
        <CardListView
          v-else-if="view === 'list'"
          :cards="shownDefs"
          :show-counts="false"
          class="all-cards"
        />
        <CardSetStrip v-else gallery :cards="shown" class="all-cards" />
      </template>
    </template>

    <template #outside><CardOverlay /></template>
  </CustomCardsPage>
</template>

<style scoped lang="scss">
.muted {
  opacity: 0.65;
}

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

.empty {
  padding: 3rem 1rem;
  text-align: center;

  svg {
    font-size: 2rem;
    opacity: 0.35;
  }

  h2 {
    margin: 0.8rem 0 0.3rem;
  }

  .lede {
    margin: 0 auto;
    max-width: 44ch;
    opacity: 0.7;
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
}

.description {
  font-size: 0.95rem;
  line-height: 1.5;
  margin: 0.7rem 0 0;
  max-width: 64ch;
  white-space: pre-wrap;
}

.site-link {
  align-items: center;
  color: color-mix(in srgb, var(--title) 70%, transparent);
  display: inline-flex;
  font-size: 0.82rem;
  gap: 0.35rem;
  margin-top: 0.5rem;

  &:hover {
    color: var(--title);
  }
}

/* The two lines that say where this set stands with you. Quiet: they are facts
   about your copy, not warnings. */
.unpublished,
.behind {
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

.actions {
  align-items: center;
  display: flex;
  flex-wrap: wrap;
  gap: 0.5rem;
}

.open-copy {
  align-items: center;
  border: 1px solid var(--box-border);
  border-radius: 5px;
  color: var(--title);
  display: inline-flex;
  font-size: 0.85rem;
  gap: 0.4rem;
  min-height: 36px;
  padding: 0 0.9rem;
  text-decoration: none;

  &:hover {
    border-color: var(--background-mid);
  }
}

/* The marketplace's own button language, so the two pages that hand you a set
   look like the same decision. */
button.go,
button.quiet,
.confirm-actions button {
  align-items: center;
  border-radius: 5px;
  cursor: pointer;
  display: inline-flex;
  font-size: 0.85rem;
  gap: 0.35rem;
  justify-content: center;
  min-height: 36px;
  padding: 0 0.9rem;
  transition:
    background 120ms ease,
    border-color 120ms ease;

  &:disabled {
    cursor: not-allowed;
    opacity: 0.5;
  }
}

button.go,
.confirm-actions .confirm {
  background: var(--button-1);
  border: 1px solid transparent;
  color: white;

  &:hover:not(:disabled) {
    background: var(--button-1-highlight);
  }
}

button.quiet,
.confirm-actions .cancel {
  background: none;
  border: 1px solid var(--box-border);
  color: var(--title);

  &:hover:not(:disabled) {
    border-color: var(--background-mid);
  }
}

.confirm-take {
  background: var(--background-dark);
  border: 1px solid var(--box-border);
  border-radius: 6px;
  margin-bottom: 1rem;
  padding: 0.8rem 1rem;

  p {
    margin: 0 0 0.6rem;
    max-width: 64ch;
  }
}

.confirm-actions {
  display: flex;
  gap: 0.5rem;
}

.empty-state {
  align-items: center;
  display: flex;
  gap: 0.5rem;
  justify-content: center;
  opacity: 0.6;
  padding: 2rem 0;
}

.shown-count {
  font-size: 0.8rem;
  opacity: 0.65;
}

.all-cards {
  margin-top: 0.8rem;
}
</style>
