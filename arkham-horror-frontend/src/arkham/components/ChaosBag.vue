<template>
  <div>
    <img
      :class="{ 'token--can-draw': drawTokenAction !== -1 }"
      class="token"
      :src="image"
      @click="$emit('choose', drawTokenAction)"
    />
  </div>
</template>

<script lang="ts">
import { Game } from '@/arkham/types/Game';
import { MessageType } from '@/arkham/types/Message';
import { Component, Prop, Vue } from 'vue-property-decorator';

@Component
export default class ChaosBag extends Vue {
  @Prop(Object) readonly game!: Game;

  get image() {
    return `/img/arkham/ct_${this.drawnToken}.png`;
  }

  get drawnToken() {
    if (this.game.currentData.skillTest === null) {
      return 'blank';
    }

    return 'blank';
  }

  get choices() {
    return this.game.currentData.question.contents;
  }

  get drawTokenAction() {
    return this.choices.findIndex((c) => c.tag === MessageType.START_SKILL_TEST);
  }
}
</script>

<style scoped lang="scss">
.token--can-draw {
  border: 5px solid #ff00ff;
  border-radius: 500px;
  cursor: pointer;
}

.token {
  width: 150px;
  height: auto;
}

</style>
