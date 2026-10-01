<script lang="ts" setup>
/* One location's cell on the map.
 *
 * Extracted from Scenario.vue so the same markup serves a location placed directly in
 * the scenario grid and a location placed inside a group's box — a grouped location is
 * laid out by the box's own grid, so it has to be a child of the box rather than a
 * sibling in the outer grid. */
import Location from '@/arkham/components/Location.vue'
import type { ComputedRef } from 'vue'
import type { Card } from '@/arkham/types/Card'
import type { Game } from '@/arkham/types/Game'
import type { Location as ArkhamLocation } from '@/arkham/types/Location'

const props = defineProps<{
  game: Game
  playerId: string
  location: ArkhamLocation
  /* Omitted for a location inside a group's box: the box holds the grid area and the
   * box's own grid places its members. */
  gridArea?: string
  cellStyle?: Record<string, string>
  offsetStyle?: Record<string, string>
  canInteract: boolean
  locationsUnlocked: boolean
  dragging: boolean
  abyssIsLocation: boolean
  abyssDeckCount: number
  onPointerDownCapture: (event: PointerEvent, location: ArkhamLocation) => void
  onClickCapture: (event: MouseEvent) => void
}>()

// Mirrors Location.vue's own emits so the payload passes straight through.
const emit = defineEmits<{
  choose: [value: number]
  show: [cards: ComputedRef<Card[]>, title: string, isDiscards: boolean, revealed?: boolean]
}>()
</script>

<template>
  <div
    class="location-cell"
    :class="{ 'location-cell--can-interact': props.canInteract }"
    :data-location-id="props.location.id"
    :data-label="props.location.label"
    :style="[
      props.gridArea ? { 'grid-area': props.gridArea, 'justify-self': 'center' } : {},
      props.cellStyle ?? {},
    ]"
  >
    <div
      class="location-wrapper"
      :style="props.offsetStyle"
      @pointerdown.capture="props.onPointerDownCapture($event, props.location)"
      @click.capture="props.onClickCapture($event)"
    >
      <div
        v-if="props.abyssIsLocation && props.location.label === 'theAbyss'"
        class="abyss-location-count"
        v-tooltip="`${props.abyssDeckCount} cards in The Abyss`"
      >
        {{ props.abyssDeckCount }}
      </div>
      <Location
        class="location"
        :class="{
          'location--unlocked': props.locationsUnlocked,
          'location--dragging': props.dragging,
        }"
        :game="props.game"
        :playerId="props.playerId"
        :location="props.location"
        @choose="emit('choose', $event)"
        @show="(cards, title, isDiscards, revealed) => emit('show', cards, title, isDiscards, revealed)"
      />
    </div>
  </div>
</template>

<style scoped>
.location-wrapper {
  width: fit-content;
  padding-top: 5px;
}
.abyss-location-count {
  display: block;
  width: fit-content;
  margin: 0 auto 6px;
  padding: 2px 8px;
  border-radius: 999px;
  background: rgba(10, 13, 25, 0.9);
  border: 1px solid rgba(111, 225, 210, 0.8);
  box-shadow: 0 0 8px rgba(111, 225, 210, 0.45);
  color: white;
  font-size: 0.85rem;
  font-weight: bold;
  cursor: help;
}
.location-cell > .location-wrapper {
  pointer-events: auto;

  /* Animate the offset along with the wrapper's FLIP move during rotation so
     the offset doesn't snap to its rotated value before the wrapper slides
     into place. Same easing/duration as .map-move keeps them in sync. */
  transition: transform 0.6s cubic-bezier(0.23, 1, 0.32, 1);
}
.location--unlocked {
  cursor: grab;
  outline: 1px dashed var(--spooky-green);
  outline-offset: 4px;
  border-radius: 6px;
  touch-action: none;
}
.location--dragging {
  cursor: grabbing;
  z-index: var(--z-index-50);
  transition: none !important;
}
.location {
  &:hover {
    z-index: var(--z-index-100);
  }
}
</style>
