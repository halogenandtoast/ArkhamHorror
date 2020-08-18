<template>
  <div>
    <img
      v-if="investigatorPortrait"
      class="portrait"
      :src="investigatorPortrait"
    />
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
      <table>
        <tr><th>Source</th><th>Modifier</th></tr>
        <tr><td>Test Difficulty</td><td>{{skillTest.difficulty}}</td></tr>
        <tr><td>{{skillTest.investigator}}</td><td></td></tr>
        <tr
          v-for="(committedCard, index) in committedCards"
          :key="index"
        ><td>{{committedCard.name}}</td><td></td></tr>
      </table>
      <button
        v-if="applyResultsAction !== -1"
        @click="$emit('choose', applyResultsAction)"
      >Apply Results</button>
    </div>
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
      return '/img/arkham/ct_plus1.png';
    case 'Zero':
      return '/img/arkham/ct_0.png';
    case 'MinusOne':
      return '/img/arkham/ct_minus1.png';
    case 'MinusTwo':
      return '/img/arkham/ct_minus2.png';
    case 'MinusThree':
      return '/img/arkham/ct_minus3.png';
    case 'MinusFour':
      return '/img/arkham/ct_minus4.png';
    case 'MinusFive':
      return '/img/arkham/ct_minus5.png';
    case 'MinusSix':
      return '/img/arkham/ct_minus6.png';
    case 'MinusSeven':
      return '/img/arkham/ct_minus7.png';
    case 'MinusEight':
      return '/img/arkham/ct_minus8.png';
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
  @Prop(String) readonly investigatorId!: string
  @Prop(Object) readonly skillTest?: SkillTest;

  imageFor = imageFor.bind(this);

  get revealedTokens() {
    if (this.game.currentData.skillTest !== null) {
      return this.game.currentData.skillTest.setAsideTokens;
    }

    return [];
  }

  get choices() {
    return choices(this.game, this.investigatorId);
  }

  get drawTokenAction() {
    return this.choices.findIndex((c) => c.tag === MessageType.START_SKILL_TEST);
  }

  get investigatorPortrait() {
    const choice = this.choices.find((c) => c.tag === MessageType.START_SKILL_TEST);
    if (choice) {
      return `/img/arkham/portraits/${choice.contents}.jpg`;
    }

    if (this.skillTest) {
      return `/img/arkham/portraits/${this.skillTest.investigator}.jpg`;
    }

    return null;
  }

  get applyResultsAction() {
    return this.choices.findIndex((c) => c.tag === MessageType.SKILL_TEST_RESULTS);
  }

  get skillTest() {
    return this.game.currentData.focusedCards;
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

.portrait {
  width: 100px;
}
</style>
