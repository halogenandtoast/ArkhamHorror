<script lang="ts" setup>
/* Adding one of your custom cards to the game.
 *
 * Building and editing live on the card builder page, where there is room for
 * them; in a game you only pick a card and say where it goes. */
import { computed, onMounted, ref } from 'vue'
import { useDebug } from '@/arkham/debug'
import { useCardStore } from '@/stores/cards'
import {
  PLAYER_CARD_TYPES,
  registerCustomCards,
  renderCardPlaceholder,
  type CustomCard,
} from '@/arkham/customCards'
import { libraryCards, libraryLoaded, loadLibrary } from '@/arkham/customCardLibrary'
import type { Game } from '@/arkham/types/Game'

const props = defineProps<{ game: Game; investigatorId: string }>()
const emit = defineEmits<{ close: [] }>()

const debug = useDebug()
const cardStore = useCardStore()

type Placement = 'play' | 'hand' | 'campaignDeck' | 'encounterDeck'

const selected = ref<string | null>(null)
const busy = ref(false)
const error = ref<string | null>(null)

onMounted(() => loadLibrary())

const cards = computed(() => libraryCards())
const selectedCard = computed(() => cards.value.find((c) => c.def.cardCode === selected.value))
const cardArt = (card: CustomCard) => card.art ?? renderCardPlaceholder(card.def)

const isPlayerCard = computed(
  () => !!selectedCard.value && PLAYER_CARD_TYPES.includes(selectedCard.value.def.cardType),
)

/* Registration is by card code and idempotent, so adding a card the game
 * already knows makes another copy of it rather than a lookalike. */
async function add(placement: Placement) {
  const card = selectedCard.value
  if (!card) return

  busy.value = true
  error.value = null

  try {
    await debug.send(props.game.id, { tag: 'DebugRegisterCustomCard', contents: card })
    registerCustomCards([card])
    if (!cardStore.cards.some((c) => c.cardCode === card.def.cardCode)) {
      cardStore.cards = [...cardStore.cards, card.def]
    }

    const cardId = crypto.randomUUID()
    await debug.send(props.game.id, { tag: 'CreateCard', contents: [cardId, card.def.cardCode] })

    await debug.send(
      props.game.id,
      {
        play: { tag: 'DebugPlaceCard', contents: [props.investigatorId, cardId] },
        hand: { tag: 'DebugAddToHand', contents: [props.investigatorId, cardId] },
        campaignDeck: { tag: 'DebugAddToCampaignDeck', contents: [props.investigatorId, cardId] },
        encounterDeck: { tag: 'DebugAddToEncounterDeck', contents: [{ tag: 'EncounterDeck' }, cardId] },
      }[placement],
    )

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
  <div class="picker-overlay" @click.self="emit('close')">
    <div class="picker-modal">
      <header>
        <h3>Add a custom card</h3>
        <router-link to="/card-builder" target="_blank" class="builder-link">Open card builder</router-link>
      </header>

      <p v-if="!libraryLoaded" class="muted">Loading your library…</p>
      <p v-else-if="!cards.length" class="muted">
        Your library is empty. Build a card in the card builder and it will show up here.
      </p>

      <div v-else class="grid">
        <div
          v-for="card in cards"
          :key="card.def.cardCode"
          class="card"
          :class="{ on: selected === card.def.cardCode }"
          @click="selected = card.def.cardCode"
        >
          <img :src="cardArt(card)" :data-image-id="card.def.cardCode" alt="" />
          <span class="name">{{ card.def.name.title }}</span>
          <small>{{ card.def.cardType.replace(/Type$/, '') }}</small>
        </div>
      </div>

      <p v-if="error" class="error">{{ error }}</p>

      <div class="actions">
        <button type="button" :disabled="busy || !selectedCard" @click="add('play')">Put into play</button>
        <button type="button" :disabled="busy || !selectedCard" @click="add('hand')">Add to hand</button>
        <button v-if="isPlayerCard" type="button" :disabled="busy" @click="add('campaignDeck')">
          Add to deck for campaign
        </button>
        <button v-if="selectedCard && !isPlayerCard" type="button" :disabled="busy" @click="add('encounterDeck')">
          Shuffle into encounter deck
        </button>
        <button type="button" class="secondary" @click="emit('close')">{{ $t('close') }}</button>
      </div>
    </div>
  </div>
</template>

<style scoped lang="scss">
.picker-overlay {
  position: fixed;
  inset: 0;
  background: rgba(0, 0, 0, 0.7);
  display: flex;
  align-items: center;
  justify-content: center;
  z-index: var(--z-index-max);
}

.picker-modal {
  background: #1a1a2e;
  border: 1px solid var(--button-highlight);
  border-radius: 8px;
  color: #eee;
  padding: 1.25rem 1.5rem 1.5rem;
  width: min(760px, 94vw);
  max-height: 92vh;
  overflow: auto;

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

.builder-link {
  color: #adf;
  font-size: 0.85rem;
}

.grid {
  display: grid;
  grid-template-columns: repeat(auto-fill, minmax(110px, 1fr));
  gap: 0.75rem;
  max-height: 55vh;
  overflow: auto;
  padding: 0.25rem;
}

.card {
  border: 2px solid transparent;
  border-radius: 8px;
  cursor: pointer;
  display: flex;
  flex-direction: column;
  gap: 0.15rem;
  padding: 0.25rem;

  img {
    border-radius: 6px;
    background: #111827;
    width: 100%;
  }

  .name {
    font-size: 0.85rem;
  }

  small {
    opacity: 0.65;
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
