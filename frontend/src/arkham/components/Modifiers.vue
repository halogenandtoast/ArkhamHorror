<script lang="ts" setup>
import Draggable from '@/components/Draggable.vue'
import { type Modifier } from '@/arkham/types/Modifier';
import View from '@/arkham/components/Modifier.vue';
import { Game } from '@/arkham/types/Game';

defineProps<{ modifiers: Modifier[], game: Game }>()
defineEmits<{ close: [] }>()
</script>

<template>
  <Draggable>
    <template #handle><header><h2>Modifiers</h2></header></template>
    <div class="modifiers">
      <View :modifier="modifier" :game="game" v-for="(modifier, idx) in modifiers" :key="idx" />
    </div>
    <button class="close button" @click="$emit('close')">Close</button>
  </Draggable>
</template>

<style scoped lang="scss">
.modifiers {
  align-self: flex-start;
  display: flex;
  flex-direction: row;
  flex-wrap: wrap;
  background: rgba(0, 0, 0, 0.6);
  padding: 6px 10px;
  gap: 5px;
  font-size: 1em;
}
</style>
