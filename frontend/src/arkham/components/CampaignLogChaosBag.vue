<script lang="ts" setup>
import { computed } from 'vue'
import { chaosTokenImage, tokenOrder, type TokenFace } from '@/arkham/types/ChaosToken'

const props = defineProps<{ chaosBag: TokenFace[] }>()

const sortedFaces = computed(() =>
  [...props.chaosBag].sort((a, b) => tokenOrder.indexOf(a) - tokenOrder.indexOf(b))
)
</script>

<template>
  <div class="log-section">
    <h3 class="section-title">{{ $t('campaignLog.chaosBag') }}</h3>
    <div class="tokens">
      <img
        v-for="(face, idx) in sortedFaces"
        :key="`${face}${idx}`"
        class="token"
        :src="chaosTokenImage(face)"
        :title="face"
      />
    </div>
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
