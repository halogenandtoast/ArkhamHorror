<script setup lang="ts">
import { computed } from 'vue'
import { img, isBroken, markBroken } from '@/assets'
import { useGame } from '@/game/context'
import { zoomState } from '@/game/overlays'
import { deckTag } from '@/game/util'

const props = withDefaults(
  defineProps<{ back: string; name: string; count: number; src?: string | null; deckKey?: string }>(),
  { src: null, deckKey: '' },
)
const ctx = useGame()

const image = computed(() => props.src ?? img(`backs/${props.back}.webp`))
const layers = computed(() => Math.min(4, Math.max(0, props.count - 1)))
const shadow = computed(
  () =>
    Array.from(
      { length: layers.value },
      (_, k) => `${(k + 1) * 2}px ${(k + 1) * 2}px 0 -1px #111, ${(k + 1) * 2}px ${(k + 1) * 2}px 0 0 #555`,
    ).join(', ') || 'none',
)
const debuggable = computed(() => ctx.dbgOn.value && !!props.deckKey && !!deckTag(props.deckKey) && !!ctx.dbgIid())
const deal = () => void ctx.debugAction('DebugDrawDeck', [ctx.dbgIid(), deckTag(props.deckKey)])
const viewDeck = () => {
  zoomState.value = { kind: 'browse', key: props.deckKey }
}
</script>

<template>
  <div
    class="deck"
    :class="{ empty: !count }"
    :data-deck="deckKey || undefined"
    :title="`${name}: ${count} card${count === 1 ? '' : 's'}`"
  >
    <div class="deck-stack" :class="{ 'no-art': count && isBroken(image) }">
      <img
        v-if="count && !isBroken(image)"
        :src="image"
        alt=""
        :style="{ boxShadow: shadow }"
        @error="markBroken(image)"
      />
      <div class="deck-empty">{{ count ? '' : 'empty' }}</div>
      <b class="deck-count">{{ count }}</b>
      <slot />
      <template v-if="debuggable">
        <button class="dbg-deal" title="Deal the top card of this deck" @click.stop="deal">+</button>
        <button class="dbg-view" title="Look through this deck and draw any card" @click.stop="viewDeck">&#x2315;</button>
      </template>
    </div>
    <div class="deck-name">{{ name }}</div>
  </div>
</template>
