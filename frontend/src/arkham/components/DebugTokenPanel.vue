<script lang="ts" setup>
/* Debug-only pill of generic tokens, dragged onto any card that accepts a token
 * pool (and onto the scenario reference card). Hold shift while dropping to place
 * five at a time.
 *
 * Renders as a pill at the left of the location zoom control, so it sits with the
 * other board-level controls instead of floating over the table.
 *
 * Deliberately only the tokens that mean the same thing on every card; the per-card
 * debug menus still cover the specialised ones (charges, uses, ...).
 *
 * The trash can at the end is the reverse trip: drag a token off any card's pool onto
 * it and that token comes off the card.
 */
import { computed, ref } from 'vue'
import PoolItem from '@/arkham/components/PoolItem.vue'
import { imgsrc } from '@/arkham/helpers'
import {
  PLACEABLE_TOKENS,
  TOKEN_POOL_TYPE,
  beginTokenDrag,
  draggedDrop,
  endCardDrag,
  trashDropHandlers,
  type PlaceableToken,
} from '@/arkham/debugCardDrop'

const props = defineProps<{ gameId: string }>()

const trash = trashDropHandlers(props.gameId)
const removing = computed(() => draggedDrop.value?.kind === 'remove')

/* The art the drag ghost uses. The pill's own icons are small so they fit beside the
 * zoom slider, but the browser's default ghost is a copy of the dragged element --
 * so hand it a full-size copy instead, which also has to be in the document to be
 * usable as a drag image. */
const TOKEN_IMAGE: Record<PlaceableToken, string> = {
  Resource: 'tokens/resource.png',
  Clue: 'tokens/clue.png',
  Doom: 'tokens/doom.png',
  Horror: 'horror.png',
  Damage: 'health.png',
}

const ghosts = ref<HTMLImageElement[]>([])

function onDragStart(event: DragEvent, token: PlaceableToken, index: number) {
  if (event.dataTransfer) {
    event.dataTransfer.effectAllowed = 'copy'
    const ghost = ghosts.value[index]
    if (ghost) event.dataTransfer.setDragImage(ghost, ghost.width / 2, ghost.height / 2)
  }
  beginTokenDrag(token)
}
</script>

<template>
  <div class="debug-token-panel" v-tooltip="$t('debug.tokenPanel.hint')">
    <div
      v-for="(token, index) in PLACEABLE_TOKENS"
      :key="token"
      class="debug-token-panel__token"
      draggable="true"
      @dragstart="onDragStart($event, token, index)"
      @dragend="endCardDrag"
    >
      <PoolItem :type="TOKEN_POOL_TYPE[token]" />
    </div>
    <div
      class="debug-token-panel__trash"
      :class="{ 'debug-token-panel__trash--armed': removing }"
      v-tooltip="$t('debug.tokenPanel.trash')"
      v-bind="trash"
    >
      <font-awesome-icon icon="trash" />
    </div>
    <img
      v-for="token in PLACEABLE_TOKENS"
      :key="`ghost-${token}`"
      ref="ghosts"
      class="debug-token-panel__ghost"
      :src="imgsrc(TOKEN_IMAGE[token])"
      aria-hidden="true"
    />
  </div>
</template>

<style scoped>
/* Same pill vocabulary as `.count-pill` in styles/components.css -- the inset
   highlight is what makes it read as recessed into the control bar rather than
   floating on top of it. */
.debug-token-panel {
  display: flex;
  flex-direction: row;
  align-items: center;
  gap: 8px;
  padding: 3px 9px;
  margin-right: 4px;
  border-radius: 999px;
  border: 1px solid rgba(255, 255, 255, 0.22);
  background: rgba(0, 0, 0, 0.45);
  box-shadow: inset 0 1px 0 rgba(255, 255, 255, 0.08);
  flex-shrink: 0;
}

.debug-token-panel__token {
  --pool-token-width: 20px;
  cursor: grab;
  display: flex;
  align-items: center;
}

/* Sits past a divider so it reads as the opposite operation rather than a sixth token. */
.debug-token-panel__trash {
  display: flex;
  align-items: center;
  justify-content: center;
  width: 22px;
  height: 22px;
  margin-left: 2px;
  border-radius: 50%;
  border: 1px dashed rgba(255, 255, 255, 0.35);
  color: rgba(255, 255, 255, 0.6);
  font-size: 11px;
}

.debug-token-panel__trash--armed {
  border-style: solid;
  border-color: var(--select);
  color: var(--select);
  background: color-mix(in srgb, var(--select) 20%, transparent);
}

/* Parked off-screen rather than `display: none`: a drag image has to be rendered. */
.debug-token-panel__ghost {
  position: fixed;
  top: -200px;
  left: -200px;
  width: var(--pool-token-width);
  height: auto;
  pointer-events: none;
}
</style>
