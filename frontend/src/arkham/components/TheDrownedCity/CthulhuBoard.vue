<script lang="ts" setup>
import { computed } from 'vue'
import { imgsrc } from '@/arkham/helpers'
import EnemyView from '@/arkham/components/Enemy.vue'
import * as Arkham from '@/arkham/types/Enemy'
import { Game } from '@/arkham/types/Game'
import { CTHULHU_BOARD_SLOTS } from '@/arkham/components/TheDrownedCity/cthulhuBoard'

/* The Doom of Arkham Pt II's Cthulhu Board. Cthulhu is one enemy on the map —
 * Cthulhu (Ancient Evil) — but the three facets you actually fight and evade live
 * here. Mechanically they stand at Ancient Evil's location and are engaged with the
 * investigators there, so this is purely how they are presented: the board pulls
 * them out of the location's enemy row and out of every threat area and seats each
 * in its own printed slot, so "return it to its place on the Cthulhu Board" stays
 * meaningful and it is obvious at a glance which facets are Enraged or banished. */

const props = defineProps<{
  game: Game
  playerId: string
  enemies: Arkham.Enemy[]
}>()

defineEmits<{ choose: [value: number] }>()

const boardStyle = computed(() => ({
  backgroundImage: `url(${imgsrc('extra/the-drowned-city/cthulhu-board.jpg')})`,
}))

/* Slot order is fixed by card, matching cthulhuBoardSlots on the backend: Hoary
 * Wings, Fierce Visage, Wicked Claw. Either face of a facet keeps its slot. */
const slotted = computed(() =>
  CTHULHU_BOARD_SLOTS.map((codes) => props.enemies.find((e) => codes.includes(e.cardCode)) ?? null)
)
</script>

<template>
  <div class="cthulhu-board" :style="boardStyle">
    <div
      v-for="(enemy, idx) in slotted"
      :key="idx"
      class="slot"
      :class="[`slot-${idx + 1}`, { empty: !enemy }]"
    >
      <EnemyView
        v-if="enemy"
        :enemy="enemy"
        :game="game"
        :playerId="playerId"
        @choose="$emit('choose', $event)"
      />
    </div>
  </div>
</template>

<style scoped>
/* The slots are printed on the art, measured as a fraction of the board's full
   width and height. Everything else is derived from them, so the board scales with
   the app's card size and the cards always land in their printed slots. */
.cthulhu-board {
  --slot-top: 12.5%;
  --slot-bottom: 69%;
  --slot-1-left: 7%;
  --slot-1-right: 31%;
  --slot-2-left: 38%;
  --slot-2-right: 62%;
  --slot-3-left: 69%;
  --slot-3-right: 93%;

  /* A slot is 24% of the board's width, so sizing the board at card-width/0.24
     renders each facet at exactly the app's card size. */
  position: relative;
  width: calc(var(--card-width) / 0.24);
  aspect-ratio: 1565 / 940;
  flex: none;

  /* 100% 100% rather than cover: the percentages above are measured against the
     whole image, so it has to fill the box exactly. The slot geometry implies a
     1.68 aspect and the art is 1.665, so the stretch is under 1%. */
  /* background-image itself is bound inline, so the asset path stays in script. */
  background-size: 100% 100%;
  background-repeat: no-repeat;
  border-radius: 4px;
}

.slot {
  position: absolute;
  top: var(--slot-top);
  bottom: calc(100% - var(--slot-bottom));
}

.slot-1 { left: var(--slot-1-left); right: calc(100% - var(--slot-1-right)); }
.slot-2 { left: var(--slot-2-left); right: calc(100% - var(--slot-2-right)); }
.slot-3 { left: var(--slot-3-left); right: calc(100% - var(--slot-3-right)); }

/* A banished facet leaves its printed slot visible rather than an empty gap. */
.slot.empty {
  border: 1px dashed rgba(255, 255, 255, 0.3);
  border-radius: 3px;
  background: rgba(0, 0, 0, 0.3);
}

/* Fill the slot rather than the app-wide card width. */
.slot :deep(img) {
  width: 100%;
  max-width: 100%;
  height: 100%;
}
</style>
