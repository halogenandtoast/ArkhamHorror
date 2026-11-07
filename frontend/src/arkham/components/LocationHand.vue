<script lang="ts" setup>
import { computed, onBeforeUnmount, ref, watch } from 'vue'
import { useI18n } from 'vue-i18n'
import type { Game } from '@/arkham/types/Game'
import { ownInvestigator } from '@/arkham/ownInvestigator'
import HandCard from '@/arkham/components/HandCard.vue'

/* The hand, fixed to the bottom of the browser window like the offline demo: every card is shown at once,
 * each showing about 60% of itself and rising when hovered. Playing a card still goes through HandCard,
 * so it sends the choice the server offered. */
const props = defineProps<{ game: Game; playerId: string }>()
const emit = defineEmits<{ (e: 'choose', index: number): void }>()

const { t } = useI18n()
const me = computed(() => ownInvestigator(props.game, props.playerId))
const hand = computed(() => me.value?.hand ?? [])
const dock = ref<HTMLElement | null>(null)
let observer: ResizeObserver | null = null
// Other overlays keep clear of the hand through this variable.
watch(dock, (el) => {
  observer?.disconnect()
  observer = null
  const root = document.documentElement.style
  if (!el) { root.removeProperty('--hand-h'); return }
  observer = new ResizeObserver(() => root.setProperty('--hand-h', `${Math.round(el.getBoundingClientRect().height - 0.4 * 1.4 * 130)}px`))
  observer.observe(el)
})
onBeforeUnmount(() => { observer?.disconnect(); document.documentElement.style.removeProperty('--hand-h') })
</script>

<template>
  <Teleport to="body">
    <div v-if="me && hand.length" ref="dock" class="hand-dock" :aria-label="t('locationMap.hand', { count: hand.length })">
      <div class="cards">
        <div v-for="(card, i) in hand" :key="i" class="slot">
          <HandCard :card="card" :game="game" :ownerId="me.id" :playerId="playerId" @choose="emit('choose', $event)" />
        </div>
      </div>
    </div>
  </Teleport>
</template>

<style scoped>
.hand-dock { position: fixed; left: 0; right: 0; bottom: 0; z-index: 900; display: flex; flex-direction: column; align-items: center; pointer-events: none; }
/* card height is about 1.4x its width; sinking the row by 40% of that leaves 60% of every card visible */
.cards { display: flex; justify-content: center; gap: 6px; margin-bottom: calc(-0.4 * 1.4 * 130px); max-width: 100%; padding: 0 8px; box-sizing: border-box; }
.slot { pointer-events: auto; flex: 0 1 130px; min-width: 0; transition: transform 0.15s; }
.slot:hover { transform: translateY(calc(-0.4 * 1.4 * 130px - 6px)); }
.slot :deep(.card) { width: 100%; min-width: 0; }
</style>
