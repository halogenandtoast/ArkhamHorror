<script lang="ts" setup>
import Draggable from '@/components/Draggable.vue';
import { computed } from 'vue';
import { useDebug } from '@/arkham/debug';
import { imgsrc } from '@/arkham/helpers';
import type { Game } from '@/arkham/types/Game';
import { concealedCardImagePath, type ConcealedCard } from '@/arkham/types/ConcealedCard';
import { useEscape } from '@/composable/escape';

const props = defineProps<{
  game: Game
  card: ConcealedCard
  playerId: string
}>()

const emit = defineEmits<{ close: [] }>()

useEscape(() => emit('close'))

const image = computed(() => imgsrc(concealedCardImagePath(props.card.kind)))

const debug = useDebug()
</script>

<template>
  <Draggable>
    <template #handle><h2>{{ $t('debug.concealedCard.title') }}</h2></template>
    <div class="debug-modal debug-window">
      <div class="concealed-card--outer">
      <div class="concealed-card" :data-index="card.id">
        <div class="card-frame">
          <div class="card-wrapper">
            <img :src="image" class="card-no-overlay" />
          </div>
        </div>
      </div>
      </div>
      <button class="debug-close" @click="emit('close')">{{ $t('debug.common.close') }}</button>
    </div>
  </Draggable>
</template>

<style scoped>
.card-no-overlay {
  width: calc(var(--card-width) * 5); 
  max-width: calc(var(--card-width) * 5);
  border-radius: 15px;
  transform: rotate(0deg);
  transition: transform 0.2s linear;
}

.concealed-card {
  display: flex;
  flex-direction: column;
}

.buttons {
  display: flex;
  flex-direction: column;
  justify-content: space-around;
  flex: 1;
  gap: 5px;
}

.concealed-card--outer {
  display: flex;
  flex-direction: row;
  /* Card pinned to the top: the button column is taller than the art, and
     centring it left the card floating mid-panel. */
  align-items: flex-start;
  gap: 10px;
}

.button{
  margin-top: 2px;
  border: 0;
  color: #fff;
  border-radius: 4px;
  border: 1px solid var(--select);
}

.card-frame {
  position: relative;
  display: flex;
  align-items: center;
  justify-content: center;
}
</style>
