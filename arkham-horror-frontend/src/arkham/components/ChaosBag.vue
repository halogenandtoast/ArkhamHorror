<template>
  <div>
    <img
      :class="{ 'token--can-draw': drawTokenAction !== -1 }"
      class="token"
      :src="image"
      @click="$emit('choose', drawTokenAction)"
    />
    <img
      v-if="currentCard"
      :src="currentCard"
    />
  </div>
</template>

<script lang="ts">
import { choices, Game } from '@/arkham/types/Game';
import { SkillTest } from '@/arkham/types/SkillTest';
import { MessageType } from '@/arkham/types/Message';
import { Component, Prop, Vue } from 'vue-property-decorator';

@Component
export default class ChaosBag extends Vue {
  @Prop(Object) readonly game!: Game;
  @Prop(Object) readonly skillTest?: SkillTest;

  get image() {
    return `/img/arkham/ct_${this.drawnToken}.png`;
  }

  get currentCard() {
    if (!this.skillTest) {
      return null;
    }

    const { tag, contents } = this.skillTest.source;

    switch (tag) {
      case 'TreacherySource':
      {
        const { cardCode } = this.game.currentData.treacheries[contents].contents;
        return `/img/arkham/cards/${cardCode}.jpg`;
      }
      default:
        return null;
    }
  }

  get drawnToken() {
    if (this.game.currentData.skillTest === null) {
      return 'blank';
    }

    return 'blank';
  }

  get choices() {
    return choices(this.game);
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
