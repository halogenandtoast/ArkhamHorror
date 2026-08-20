<script lang="ts" setup>
import { computed, ref } from 'vue'
import { chaosTokenImage, compareTokenFaces, type TokenFace } from '@/arkham/types/ChaosToken'
import { type ChaosBagChange } from '@/arkham/types/Campaign'
import CampaignLogChaosBagChanges from '@/arkham/components/CampaignLogChaosBagChanges.vue'
import type { Game } from '@/arkham/types/Game'

const props = defineProps<{ game: Game, chaosBag: TokenFace[], history: ChaosBagChange[] }>()

const sortedFaces = computed(() =>
  [...props.chaosBag].sort(compareTokenFaces)
)

const showHistory = ref(false)
</script>

<template>
  <div class="log-section">
    <h3 class="section-title">
      {{ $t('campaignLog.chaosBag') }} <span class="count-pill">{{ chaosBag.length }}</span>
      <button v-if="history.length > 0" type="button" class="history-toggle" @click="showHistory = !showHistory">
        {{ showHistory ? $t('campaignLog.hideHistory') : $t('campaignLog.viewHistory') }}
      </button>
    </h3>
    <div class="tokens">
      <img
        v-for="(face, idx) in sortedFaces"
        :key="`${face}${idx}`"
        class="token"
        :src="chaosTokenImage(face)"
        :title="face"
      />
    </div>
    <CampaignLogChaosBagChanges v-if="showHistory" class="history" :game="game" :history="history" />
  </div>
</template>

<style scoped>
.log-section {
  background: var(--box-background);
  border: 1px solid rgba(255,255,255,0.07);
  border-radius: 8px;
  padding: 14px 16px;
}

.section-title {
  display: flex;
  align-items: center;
  gap: 8px;
  font-family: teutonic, sans-serif;
  font-size: 1.1em;
  font-weight: normal;
  color: rgba(255,255,255,0.75);
  text-transform: uppercase;
  letter-spacing: 0.08em;
  margin: 0 0 10px;
  padding-bottom: 8px;
  border-bottom: 1px solid rgba(255,255,255,0.07);
}

.history-toggle {
  margin-left: auto;
  padding: 3px 10px;
  background: rgba(255,255,255,0.06);
  border: 1px solid rgba(255,255,255,0.12);
  border-radius: 4px;
  color: rgba(255,255,255,0.6);
  font-family: inherit;
  font-size: 0.7em;
  letter-spacing: 0.08em;
  text-transform: uppercase;
  cursor: pointer;

  &:hover {
    background: rgba(255,255,255,0.1);
    color: rgba(255,255,255,0.85);
  }
}

/* Opt out of the global button press effect: these toggles reveal a panel
   rather than acting on the game. */
button.history-toggle:active:not(:disabled) {
  transform: none;
}

.history {
  margin-top: 12px;
  padding-top: 12px;
  border-top: 1px solid rgba(255,255,255,0.07);
}

.tokens {
  display: flex;
  flex-wrap: wrap;
  gap: 6px;
}

.token {
  width: 40px;
  height: 40px;
  border-radius: 50%;
  border: 1px solid rgba(255,255,255,0.2);
  box-shadow: 0 2px 4px rgba(0,0,0,0.5);
  transition: transform 0.15s;

  &:hover { transform: scale(1.15); }
}
</style>
