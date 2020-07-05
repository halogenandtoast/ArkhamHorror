<template>
  <div class="act-container">
    <img
      v-if="game.gameState.stacks.Act.contents[0].canProgress"
      class="card card--sideways act--can-progress"
      @click="progressAct"
      :src="game.gameState.stacks.Act.contents[0].image"
    />
    <img
      v-else
      class="card card--sideways"
      :src="game.gameState.stacks.Act.contents[0].image"
    />
  </div>
</template>

<script lang="ts">
import { Component, Prop, Vue } from 'vue-property-decorator';
import { ActStack } from '@/arkham/types/game';

@Component
export default class Act extends Vue {
  @Prop(Object) readonly act!: ActStack;
}
</script>
