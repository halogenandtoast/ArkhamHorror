<script lang="ts" setup>
import { computed, ref, watch } from 'vue'
import { useI18n } from 'vue-i18n'
import type { Game } from '@/arkham/types/Game'
import { ownInvestigator } from '@/arkham/ownInvestigator'
import HandCard from '@/arkham/components/HandCard.vue'

/* The hand, fixed to the bottom of the browser window like the offline demo: each card shows about 80% of
 * itself, rises when hovered, and the hand pages with the arrows when it is long. Playing a card still goes
 * through HandCard, so it sends the choice the server offered. */
const props = defineProps<{ game: Game; playerId: string }>()
const emit = defineEmits<{ (e: 'choose', index: number): void }>()

const { t } = useI18n()
const PAGE = 7
const me = computed(() => ownInvestigator(props.game, props.playerId))
const hand = computed(() => me.value?.hand ?? [])
const page = ref(0)
const pages = computed(() => Math.max(1, Math.ceil(hand.value.length / PAGE)))
watch(pages, (n) => { if (page.value >= n) page.value = n - 1 })
const visible = computed(() => hand.value.slice(page.value * PAGE, page.value * PAGE + PAGE))
</script>

<template>
  <Teleport to="body">
    <div v-if="me && hand.length" class="hand-dock" :aria-label="t('locationMap.hand', { count: hand.length })">
      <div class="tab">
        <button v-if="pages > 1" :disabled="page === 0" @click="page--">‹</button>
        <span>{{ t('locationMap.hand', { count: hand.length }) }}</span>
        <button v-if="pages > 1" :disabled="page >= pages - 1" @click="page++">›</button>
      </div>
      <div class="cards">
        <div v-for="(card, i) in visible" :key="page * PAGE + i" class="slot">
          <HandCard :card="card" :game="game" :ownerId="me.id" :playerId="playerId" @choose="emit('choose', $event)" />
        </div>
      </div>
    </div>
  </Teleport>
</template>

<style scoped>
.hand-dock { position: fixed; left: 0; right: 0; bottom: 0; z-index: 900; display: flex; flex-direction: column; align-items: center; pointer-events: none; }
.tab { pointer-events: auto; display: flex; align-items: center; gap: 10px; padding: 2px 14px; font-size: 0.75rem; background: #0f1813f0; border: 1px solid #c6a87377; border-bottom: 0; border-radius: 10px 10px 0 0; color: #e6d6a8; }
.tab button { background: transparent; border: 0; color: inherit; font-size: 1rem; cursor: pointer; }
.tab button:disabled { opacity: 0.3; cursor: default; }
/* card height is about 1.4x its width; sinking the row by 20% of that leaves 80% of every card visible */
.cards { display: flex; justify-content: center; gap: 6px; margin-bottom: calc(-0.2 * 1.4 * 130px); }
.slot { pointer-events: auto; width: 130px; flex: none; transition: transform 0.15s; }
.slot:hover { transform: translateY(calc(-0.2 * 1.4 * 130px - 6px)); }
.slot :deep(.card) { width: 130px; min-width: 130px; }
</style>
