<script lang="ts" setup>
import { computed, ref } from 'vue'
import { Dropdown } from 'floating-vue'
import { type Card as ArkhamCard, type CardContents, cardImage, toCardContents } from '@/arkham/types/Card'
import { imgsrc } from '@/arkham/helpers'
import type { Game } from '@/arkham/types/Game'
import * as ArkhamGame from '@/arkham/types/Game'
import CardView from '@/arkham/components/Card.vue'

const props = withDefaults(defineProps<{
  cards: (ArkhamCard | CardContents)[]
  label?: string
  placement?: 'top' | 'bottom' | 'left' | 'right'
  game?: Game
  playerId?: string
  isDiscards?: boolean
  highlighted?: boolean
  showLabel?: boolean
  shown?: boolean
  fullWidth?: boolean
}>(), {
  label: 'Cards underneath',
  placement: 'bottom',
})

const emit = defineEmits<{
  choose: [value: number]
  'update:shown': [value: boolean]
}>()

const internalShown = ref(false)
const shown = computed({
  get: () => props.shown ?? internalShown.value,
  set: (value: boolean) => {
    if (props.shown === undefined) internalShown.value = value
    emit('update:shown', value)
  },
})

const count = computed(() => props.cards.length)
const tooltip = computed(() => `${props.label} (${count.value}) — click to view`)
const choices = computed(() => props.game && props.playerId ? ArkhamGame.choices(props.game, props.playerId) : [])
const interactive = computed(() => props.game !== undefined && props.playerId !== undefined)

function isCardInChoices(card: ArkhamCard | CardContents): boolean {
  const cardId = toCardContents(card).id
  return choices.value.some(choice => choice.tag === 'TargetLabel' && cardId === choice.target.contents)
}
</script>

<template>
  <Dropdown
    :placement="placement"
    :distance="8"
    v-model:shown="shown"
    :disabled="count === 0"
    :triggers="['click']"
    :auto-hide="true"
    theme="cards-under-popover"
  >
    <button
      type="button"
      class="cards-under-indicator"
      :class="{ 'cards-under-indicator--highlighted': highlighted, 'cards-under-indicator--with-label': showLabel, 'cards-under-indicator--full-width': fullWidth }"
      :aria-label="tooltip"
      v-tooltip="tooltip"
    >
      <span class="cards-under-indicator__icon" aria-hidden="true">
        <span class="cards-under-indicator__card cards-under-indicator__card--back" />
        <span class="cards-under-indicator__card cards-under-indicator__card--front" />
      </span>
      <span v-if="showLabel" class="cards-under-indicator__label">{{ label }}</span>
      <span class="cards-under-indicator__count">{{ count }}</span>
    </button>

    <template #popper>
      <div class="cards-under-popover">
        <div class="cards-under-popover__header">{{ label }} ({{ count }})</div>
        <div class="cards-under-popover__cards">
          <div
            v-for="(card, i) in cards"
            :key="i"
            class="cards-under-popover__card-wrap"
            :class="{ discard: isDiscards && !isCardInChoices(card) }"
          >
            <CardView
              v-if="interactive && game && playerId"
              :game="game"
              :playerId="playerId"
              :card="card"
              @choose="emit('choose', $event)"
            />
            <img
              v-else
              :src="imgsrc(cardImage(card))"
              class="card cards-under-popover__card"
            />
          </div>
        </div>
      </div>
    </template>
  </Dropdown>
</template>

<style scoped>
.cards-under-indicator {
  display: flex !important;
  align-items: center;
  justify-content: center;
  gap: 7px;
  height: 22px;
  padding: 0 7px;
  border-radius: 999px;
  border: 1px solid rgba(255, 255, 255, 0.16);
  background: rgba(0, 0, 0, 0.46);
  color: #fff;
  line-height: 1;
  cursor: pointer;
  backdrop-filter: blur(4px);
  box-shadow: 0 2px 8px rgba(0, 0, 0, 0.35);
  transition: background 0.15s ease, border-color 0.15s ease, transform 0.15s ease;
}

.cards-under-indicator:hover {
  background: rgba(0, 0, 0, 0.68);
  border-color: rgba(255, 255, 255, 0.32);
  transform: translateY(-1px);
}

.cards-under-indicator--highlighted {
  border-color: var(--select);
  box-shadow: 0 0 0 1px var(--select), 0 0 10px color-mix(in srgb, var(--select) 65%, transparent);
}

.cards-under-indicator--with-label {
  width: 100%;
  justify-content: center;
}

.cards-under-indicator--full-width {
  width: var(--card-width) !important;
  min-width: var(--card-width);
  max-width: var(--card-width);
  justify-content: center;
}

.cards-under-indicator__icon {
  width: 20px;
  height: 15px;
  flex: 0 0 20px;
  display: flex;
  align-items: center;
  justify-content: center;
  overflow: hidden;
}

.cards-under-indicator__card {
  flex: 0 0 auto;
  width: 10px;
  height: 13px;
  border-radius: 2px;
  border: 1px solid rgba(255, 255, 255, 0.8);
  background: rgba(255, 255, 255, 0.18);
  box-shadow: 0 1px 2px rgba(0, 0, 0, 0.45);
}

.cards-under-indicator__card--back {
  opacity: 0.55;
  transform: rotate(-6deg) translateY(1px);
}

.cards-under-indicator__card--front {
  margin-left: -4px;
  background: rgba(255, 255, 255, 0.28);
  transform: rotate(6deg) translateY(-1px);
}

.cards-under-indicator__label {
  min-width: 0;
  overflow: hidden;
  text-overflow: ellipsis;
  white-space: nowrap;
  font-size: 0.72rem;
  font-weight: 800;
  letter-spacing: 0.02em;
  color: rgba(255, 255, 255, 0.86);
}

.cards-under-indicator__count {
  min-width: 1.35em;
  height: 1.35em;
  display: inline-grid;
  place-items: center;
  border-radius: 999px;
  font-size: 0.72rem;
  font-weight: 800;
  line-height: 1;
  background: rgba(255, 255, 255, 0.14);
  border: 1px solid rgba(255, 255, 255, 0.22);
  font-variant-numeric: tabular-nums;
}

@media (max-width: 800px) {
  .cards-under-indicator {
    gap: 0;
    padding: 0 5px;
  }

  .cards-under-indicator__icon {
    display: none;
  }
}

.cards-under-popover {
  min-width: 0;
  max-width: 80vw;
  padding: 10px;
}

.cards-under-popover__header {
  margin: 0 0 8px;
  color: rgba(255, 255, 255, 0.82);
  font-size: 0.85rem;
  font-weight: 700;
  letter-spacing: 0.02em;
  text-transform: uppercase;
}

.cards-under-popover__cards {
  display: flex;
  flex-direction: row;
  align-items: flex-end;
  gap: 6px;
  overflow-x: auto;
}

.cards-under-popover__card-wrap {
  position: relative;
  flex: 0 0 auto;
}

.cards-under-popover__card-wrap.discard {
  filter: grayscale(0.85);
}

.cards-under-popover__card-wrap :deep(.card),
.cards-under-popover__card {
  width: calc(var(--card-width, 100px) * 1.1);
  border-radius: 6px;
  box-shadow: 0 2px 8px rgba(0, 0, 0, 0.6);
}

.cards-under-popover__card-wrap :deep(.card-container) {
  margin: 0;
}
</style>

<style>
.v-popper--theme-cards-under-popover .v-popper__inner {
  background: rgba(15, 15, 20, 0.92);
  backdrop-filter: blur(8px);
  border: 1px solid rgba(255, 255, 255, 0.12);
  border-radius: 10px;
  color: #fff;
  box-shadow: 0 8px 30px rgba(0, 0, 0, 0.5);
}

.v-popper--theme-cards-under-popover .v-popper__arrow-outer {
  border-color: rgba(255, 255, 255, 0.12);
}

.v-popper--theme-cards-under-popover .v-popper__arrow-inner {
  border-color: rgba(15, 15, 20, 0.92);
}
</style>
