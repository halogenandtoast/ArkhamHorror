<script lang="ts" setup>
/* The hint that follows the cursor while a debug drag is over a card, saying what
 * the drop will do.
 *
 * Mounted once by Scenario.vue rather than by each card: only one thing can be in
 * flight over one card at a time, and the four card components that accept the drop
 * would otherwise each carry a copy of this markup. Styled to match Draw.vue's
 * `deck-drop-indicator`, which is the same affordance for cards.
 */
import PoolItem from '@/arkham/components/PoolItem.vue'
import { TOKEN_POOL_TYPE, draggedDrop, dropAmount, dropPosition, dropUseType } from '@/arkham/debugCardDrop'
import { chaosTokenImage } from '@/arkham/types/ChaosToken'
</script>

<template>
  <div
    v-if="draggedDrop && dropPosition"
    class="card-drop-indicator"
    :style="{ left: `${dropPosition.x}px`, top: `${dropPosition.y}px` }"
  >
    <template v-if="draggedDrop.kind === 'seal'">
      <img class="card-drop-indicator__token" :src="chaosTokenImage(draggedDrop.chaosToken.face)" />
      <span class="card-drop-indicator__label">{{ $t('debug.cardMove.sealToken') }}</span>
    </template>
    <template v-else-if="draggedDrop.kind === 'remove'">
      <span class="card-drop-indicator__label">
        {{ $t('debug.cardMove.moveTokens', { count: dropAmount }) }}
      </span>
    </template>
    <template v-else-if="draggedDrop.kind === 'tokens'">
      <PoolItem class="card-drop-indicator__pool" :type="TOKEN_POOL_TYPE[draggedDrop.token]" />
      <span class="card-drop-indicator__label">
        <template v-if="draggedDrop.token === 'Resource' && dropUseType">
          {{ $t('debug.cardMove.placeUses', { count: dropAmount, use: dropUseType }) }}
        </template>
        <template v-else>
          {{ $t('debug.cardMove.placeTokens', { count: dropAmount }) }}
        </template>
      </span>
    </template>
  </div>
</template>

<style scoped>
.card-drop-indicator {
  position: fixed;
  display: inline-flex;
  flex-direction: row;
  align-items: center;
  justify-content: center;
  gap: 6px;
  padding: 5px 8px;
  border-radius: 999px;
  border: 1px solid color-mix(in srgb, var(--select) 45%, rgba(255, 255, 255, 0.3));
  background: rgba(0, 0, 0, 0.46);
  color: rgba(255, 255, 255, 0.92);
  pointer-events: none;
  z-index: var(--z-index-max);
  transform: translate(18px, -50%);
  box-shadow: 0 2px 8px rgba(0, 0, 0, 0.28);
  backdrop-filter: blur(2px);
  text-shadow: 0 1px 2px rgba(0, 0, 0, 0.75);
}

.card-drop-indicator__token {
  width: 18px;
  height: 18px;
}

.card-drop-indicator__pool {
  --pool-token-width: 18px;
}

.card-drop-indicator__label {
  font-size: 0.72rem;
  font-weight: 700;
  white-space: nowrap;
}
</style>
