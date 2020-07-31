<template>
  <div>
    <img
      v-for="(revealedToken, index) in revealedTokens"
      class="token"
      :key="index"
      :src="imageFor(revealedToken)"
    />
    <img
      v-if="drawTokenAction !== -1"
      class="token token--can-draw"
      src="/img/arkham/ct_blank.png"
      @click="$emit('choose', drawTokenAction)"
    />
    <div>
      <button
        v-if="applyResultsAction !== -1"
        @click="$emit('choose', applyResultsAction)"
      >Apply Results</button>
    </div>
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

function imageFor(token: string) {
  switch (token) {
    case 'PlusOne':
      return '/img/arkham/ct_+1.png';
    case 'Zero':
      return '/img/arkham/ct_0.png';
    case 'MinusOne':
      return '/img/arkham/ct_-1.png';
    case 'MinusTwo':
      return '/img/arkham/ct_-2.png';
    case 'MinusThree':
      return '/img/arkham/ct_-3.png';
    case 'MinusFour':
      return '/img/arkham/ct_-4.png';
    case 'MinusFive':
      return '/img/arkham/ct_-5.png';
    case 'MinusSix':
      return '/img/arkham/ct_-6.png';
    case 'MinusSeven':
      return '/img/arkham/ct_-7.png';
    case 'MinusEight':
      return '/img/arkham/ct_-8.png';
    case 'AutoFail':
      return '/img/arkham/ct_autofail.png';
    case 'ElderSign':
      return '/img/arkham/ct_eldersign.png';
    case 'Skull':
      return '/img/arkham/ct_skull.png';
    case 'Cultist':
      return '/img/arkham/ct_cultist.png';
    case 'Tablet':
      return '/img/arkham/ct_tablet.png';
    case 'ElderThing':
      return '/img/arkham/ct_elderthing.png';
    default:
      return '/img/arkham/ct_blank.png';
  }
}

@Component
export default class ChaosBag extends Vue {
  @Prop(Object) readonly game!: Game;
  @Prop(Object) readonly skillTest?: SkillTest;

  imageFor = imageFor.bind(this);

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

  get revealedTokens() {
    if (this.game.currentData.skillTest !== null) {
      return this.game.currentData.skillTest.setAsideTokens;
    }

    return [];
  }

  get choices() {
    return choices(this.game);
  }

  get drawTokenAction() {
    return this.choices.findIndex((c) => c.tag === MessageType.START_SKILL_TEST);
  }

  get applyResultsAction() {
    return this.choices.findIndex((c) => c.tag === MessageType.SKILL_TEST_RESULTS);
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
