<script lang="ts" setup>
import { computed } from 'vue';
import type { Game } from '@/arkham/types/Game';
import { type Card, type EncounterCard, type PlayerCard, toCardContents } from '@/arkham/types/Card';
import CardView from '@/arkham/components/Card.vue'
import Enemy from '@/arkham/components/Enemy.vue';
import CardsUnderIndicator from '@/arkham/components/CardsUnderIndicator.vue';
import { useI18n } from 'vue-i18n';
const { t } = useI18n();

export interface Props {
  game: Game
  playerId: string
  victoryDisplay: Card[]
}

const props = defineProps<Props>()
const emit = defineEmits(['choose'])

const choose = async (idx: number) => emit('choose', idx)

const unfinishedBusinessCodes = new Set(['c05178b', 'c05178d', 'c05178f', 'c05178h', 'c05178j', 'c05178l', 'c54038b', 'c54039b'])

function victoryDisplayRegularCard(card: EncounterCard | PlayerCard): EncounterCard | PlayerCard {
  const contents = toCardContents(card)
  if (!unfinishedBusinessCodes.has(contents.cardCode)) return card
  return { ...card, contents: { ...contents, isFlipped: true } }
}

function victoryDisplayCard(card: Card): Card {
  if (card.tag === 'VengeanceCard') return { ...card, contents: victoryDisplayRegularCard(card.contents) }
  return victoryDisplayRegularCard(card)
}

const displayVictoryDisplay = computed(() => props.victoryDisplay.map(victoryDisplayCard))

const enemiesInVictoryDisplay = computed(() => {
  return Object.values(props.game.enemies).filter((e) => e.placement.tag === 'OutOfPlay' && (['VictoryDisplayZone'] as string[]).includes(e.placement.contents))
})
const topOfVictoryDisplay = computed(() => {
  const enemyCardIds = enemiesInVictoryDisplay.value.map(e => e.cardId)
  return displayVictoryDisplay.value.filter((c) => !enemyCardIds.includes(toCardContents(c).id))[0]
})

const viewVictoryDisplayLabel = computed(() => t('scenario.victoryDisplay'))
</script>
<template>
  <div v-if="topOfVictoryDisplay || enemiesInVictoryDisplay.length > 0" class="victory-display" :aria-label="viewVictoryDisplayLabel" :title="viewVictoryDisplayLabel">
    <div v-if="topOfVictoryDisplay" class="victory-display-card">
      <CardView :game="game" :card="topOfVictoryDisplay" :playerId="playerId" />

    </div>

    <Enemy
      v-for="enemy in enemiesInVictoryDisplay"
      :enemy="enemy"
      :game="game"
      :playerId="playerId"
      @choose="choose"
    />


    <CardsUnderIndicator
      v-if="victoryDisplay.length > 0"
      :cards="displayVictoryDisplay"
      :label="viewVictoryDisplayLabel"
      :game="game"
      :playerId="playerId"
      :isDiscards="true"
      :fullWidth="true"
      placement="right"
      @choose="choose"
    />
  </div>
</template>

<style scoped>
.card {
  width: 100px;
  border-radius: 6px;
  box-shadow: unset;
}

.victory-display {
  position: relative;
  display: flex;
  flex-direction: column;
  gap: 5px;
  align-items: center;
  padding: 5px;
  border: 1px solid rgba(214, 178, 92, 0.55);
  border-radius: 8px;
  background: linear-gradient(180deg, rgba(89, 67, 24, 0.38), rgba(0, 0, 0, 0.22));
  box-shadow: inset 0 0 10px rgba(214, 178, 92, 0.12);


  &:deep(.card-wrapper) {
    &::after {
      border-radius: 6px;
      pointer-events: none;
      content: "";
      position: absolute;
      inset: 0;
      background-color: #FFF;
      opacity: .85;
      mix-blend-mode: saturation;
      z-index: var(--z-index-20);
    }
  }
  &:deep(.card) {
    box-shadow: unset;
    margin: 0;
  }
}

.victory-display-card {
  position: relative;
  width: fit-content;
  line-height: 0;
  box-shadow: 1px 1px 6px rgba(0, 0, 0, 0.45);

  &::after {
    border-radius: 6px;
    pointer-events: none;
    content: "";
    position: absolute;
    inset: 0;
    background-color: #FFF;
    opacity: .85;
    mix-blend-mode: saturation;
  }
}
</style>
