<script lang="ts" setup>
/* Granting an investigator a story card they were supposed to earn.
 *
 * The candidates are narrowed to what this game can actually produce -- the
 * loaded scenario's encounter sets, the campaign's own cards, and each side
 * story already played -- because that is almost always what you are reaching
 * for. "View all" is there for everything else, including a Return-To campaign
 * replaying another cycle's scenarios, where the narrowing under-reports. */
import { computed, onMounted, ref } from 'vue'
import { useDebug } from '@/arkham/debug'
import { useCardStore } from '@/stores/cards'
import { cardFrontImage } from '@/arkham/cardArt'
import { allEarnableByOwner, bareCardCode, earnableGroups } from '@/arkham/earnableCards'
import type { CardDef } from '@/arkham/types/CardDef'
import type { Game } from '@/arkham/types/Game'
import { fullName } from '@/arkham/types/Name'
import { useEscape } from '@/composable/escape'

const props = defineProps<{ game: Game; investigatorId: string }>()
const emit = defineEmits<{ close: [] }>()

useEscape(() => emit('close'))

const debug = useDebug()
const cardStore = useCardStore()

const selected = ref<string | null>(null)
const search = ref('')
const viewAll = ref(false)
const busy = ref(false)
const error = ref<string | null>(null)

/* The store fetches the whole pool (`cardPool=both`), which is what this needs:
 * every earnable card is an encounter card by definition. */
onMounted(() => cardStore.fetchCards())

const allCards = computed(() => cardStore.cards)

const groups = computed(() =>
  viewAll.value
    ? allEarnableByOwner(allCards.value)
    : earnableGroups(props.game, allCards.value),
)

function matches(card: CardDef, query: string) {
  return [bareCardCode(card.cardCode), fullName(card.name)]
    .join(' ')
    .toLocaleLowerCase()
    .includes(query)
}

/* A search looks across everything earnable, not just the suggested sections --
 * otherwise the card you typed the name of appears to not exist. */
const results = computed(() => {
  const query = search.value.trim().toLocaleLowerCase()
  if (!query) return groups.value

  const source = viewAll.value ? groups.value : allEarnableByOwner(allCards.value)
  return source
    .map((group) => ({ ...group, cards: group.cards.filter((c) => matches(c, query)) }))
    .filter((group) => group.cards.length > 0)
})

const selectedCard = computed(() =>
  allCards.value.find((c) => c.cardCode === selected.value) ?? null,
)

async function add() {
  const card = selectedCard.value
  if (!card) return

  busy.value = true
  error.value = null

  try {
    /* DebugAddToCampaignDeck earns the card for the rest of the campaign: into
     * the deck now, and into the campaign's story cards so it survives deck
     * loading between scenarios. */
    const cardId = crypto.randomUUID()
    await debug.send(props.game.id, { tag: 'CreateCard', contents: [cardId, card.cardCode] })
    await debug.send(props.game.id, {
      tag: 'DebugAddToCampaignDeck',
      contents: [props.investigatorId, cardId],
    })
    emit('close')
  } catch (e) {
    console.error(e)
    error.value = 'Could not add that card.'
  } finally {
    busy.value = false
  }
}
</script>

<template>
  <!-- Teleported out: the picker opens from an investigator row that sits
       inside a tab panel, and a `position: fixed` overlay still collapses to
       nothing when an ancestor is hidden or clipped. -->
  <Teleport to="body">
  <div class="picker-overlay" data-card-overlay-above @click.self="emit('close')">
    <div class="picker-modal">
      <header>
        <h3>{{ $t('debug.earnedCards.title') }}</h3>
        <label class="view-all">
          <input v-model="viewAll" type="checkbox" />
          {{ $t('debug.earnedCards.viewAll') }}
        </label>
      </header>

      <input
        v-model="search"
        type="text"
        class="search"
        :placeholder="$t('debug.earnedCards.search')"
      />

      <p v-if="!cardStore.loaded" class="muted">{{ $t('debug.earnedCards.loading') }}</p>
      <p v-else-if="results.length === 0" class="muted">{{ $t('debug.earnedCards.empty') }}</p>

      <div v-else class="groups">
        <section v-for="group in results" :key="group.key">
          <h4>{{ group.titleKey ? $t(`debug.earnedCards.${group.titleKey}`) : group.label }}</h4>
          <div class="grid">
            <!-- No name or code: `data-image-id` opts the image into the
                 shared card overlay, which shows both on hover. -->
            <div
              v-for="card in group.cards"
              :key="`${group.key}-${card.cardCode}`"
              class="pick"
              :class="{ on: selected === card.cardCode }"
              @click="selected = card.cardCode"
            >
              <img
                :src="cardFrontImage(card)"
                :data-image-id="bareCardCode(card.cardCode)"
                :alt="fullName(card.name)"
              />
            </div>
          </div>
        </section>
      </div>

      <p v-if="error" class="error">{{ error }}</p>

      <div class="actions">
        <button type="button" :disabled="busy || !selectedCard" @click="add">
          {{ $t('debug.earnedCards.add') }}
        </button>
        <button type="button" class="secondary" @click="emit('close')">{{ $t('close') }}</button>
      </div>
    </div>
  </div>
  </Teleport>
</template>

<style scoped lang="scss">
.picker-overlay {
  position: fixed;
  inset: 0;
  background: rgba(0, 0, 0, 0.7);
  display: flex;
  align-items: center;
  justify-content: center;
  z-index: var(--z-modal-overlay);
}

.picker-modal {
  background: #1a1a2e;
  border: 1px solid var(--button-highlight);
  border-radius: 8px;
  color: #eee;
  padding: 1.25rem 1.5rem 1.5rem;
  width: min(860px, 94vw);
  max-height: 92vh;
  display: flex;
  flex-direction: column;

  header {
    align-items: baseline;
    display: flex;
    gap: 1rem;
    justify-content: space-between;
    margin-bottom: 0.75rem;
  }

  h3 {
    color: #adf;
    font-size: 1.1rem;
    margin: 0;
  }
}

.view-all {
  display: flex;
  align-items: center;
  font-size: 0.85rem;
  gap: 0.35rem;
  opacity: 0.85;
}

.search {
  background: rgba(255, 255, 255, 0.08);
  border: 1px solid #4b5563;
  border-radius: 4px;
  color: #eee;
  margin-bottom: 0.75rem;
  padding: 0.45rem 0.6rem;
  width: 100%;
}

.groups {
  overflow: auto;
  min-height: 0;

  h4 {
    color: #adf;
    font-size: 0.9rem;
    font-weight: normal;
    letter-spacing: 0.04em;
    margin: 0.75rem 0 0.35rem;
    text-transform: uppercase;
  }

  section:first-child h4 {
    margin-top: 0;
  }
}

.grid {
  display: grid;
  grid-template-columns: repeat(auto-fill, minmax(110px, 1fr));
  gap: 0.75rem;
  padding: 0.25rem;
}

.pick {
  border: 2px solid transparent;
  border-radius: 8px;
  cursor: pointer;
  padding: 0.25rem;

  img {
    border-radius: 6px;
    background: #111827;
    display: block;
    width: 100%;
  }

  &:hover {
    background: rgba(255, 255, 255, 0.06);
  }

  &.on {
    border-color: var(--button-highlight);
  }
}

.actions {
  display: flex;
  flex-wrap: wrap;
  gap: 0.5rem;
  margin-top: 1rem;

  button {
    background: rgba(255, 255, 255, 0.08);
    border: 1px solid var(--button-highlight);
    border-radius: 4px;
    color: #eee;
    cursor: pointer;
    padding: 0.5rem 0.8rem;

    &:disabled {
      cursor: default;
      opacity: 0.5;
    }

    &.secondary {
      border-color: #4b5563;
      margin-left: auto;
    }
  }
}

.muted {
  opacity: 0.8;
}

.error {
  color: #f88;
}
</style>
