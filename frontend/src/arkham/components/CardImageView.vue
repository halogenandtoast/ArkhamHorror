<script lang="ts" setup>
import * as Arkham from '@/arkham/types/CardDef'
import { localizeArkhamDBBaseUrl } from '@/arkham/helpers'
import CardImage from '@/arkham/components/CardImage.vue'

defineProps<{ cards: Arkham.CardDef[] }>()
</script>

<template>
  <div class="cards">
    <a v-for="(card, idx) in cards" :key="idx" target="_blank" :href="`${localizeArkhamDBBaseUrl()}/card/${card.art}`">
      <CardImage :card="card" />
    </a>
  </div>
</template>

<style scoped>
.cards {
  --min-col-width: 200px;
  flex: 1;
  overflow-y: auto;
  display: grid;
  grid-template-columns: repeat(auto-fill, minmax(min(var(--min-col-width), 100%), 1fr));
  gap: 12px;
  padding: 16px;
  align-content: start;

  > a {
    display: flex;
    justify-content: center;
  }

  &:deep(.card-container) {
    width: 100%;
    max-width: unset;
    margin: 0;
  }
}
</style>
