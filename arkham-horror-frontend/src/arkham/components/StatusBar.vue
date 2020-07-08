<template>
  <footer>
    <p v-if="resolvingEnemies">Enemy Phase: Resolve each enemy attack</p>
  </footer>
</template>

<script lang="ts">
import { Component, Prop, Vue } from 'vue-property-decorator';
import { ArkhamGame, ArkhamStepTypes } from '@/arkham/types/game';

@Component
export default class StatusBar extends Vue {
  @Prop(Object) readonly game!: ArkhamGame

  get resolvingEnemies() {
    return this.game.gameState.step.tag === ArkhamStepTypes.RESOLVE_ENEMIES;
  }
}
</script>
