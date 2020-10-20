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
  </div>
</template>

<script lang="ts">
import { defineComponent, computed } from 'vue';
import { Game } from '@/arkham/types/Game';
import * as ArkhamGame from '@/arkham/types/Game';
import { SkillTest } from '@/arkham/types/SkillTest';
import { MessageType } from '@/arkham/types/Message';


export default defineComponent({
  props: {
    game: { type: Object as () => Game, required: true },
    skillTest: { type: Object as () => SkillTest, required: false },
    investigatorId: { type: String, required: true }
  },
  setup(props) {
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

    const revealedTokens = computed(() => {
      if (props.game.currentData.focusedTokens.length > 0) {
        return props.game.currentData.focusedTokens;
      }

      if (props.game.currentData.skillTest !== null) {
        return props.game.currentData.skillTest.setAsideTokens;
      }

      return [];
    })

    const choices = computed(() => ArkhamGame.choices(props.game, props.investigatorId))
    const drawTokenAction = computed(() => choices.value.findIndex((c) => c.tag === MessageType.START_SKILL_TEST))

    const investigatorPortrait = computed(() => {
      const choice = choices.value.find((c) => c.tag === MessageType.START_SKILL_TEST);
      if (choice) {
        return `/img/arkham/portraits/${choice.contents}.jpg`;
      }

      if (props.skillTest) {
        return `/img/arkham/portraits/${props.skillTest.investigator}.jpg`;
      }

      return null;
    })

    return { revealedTokens, drawTokenAction, investigatorPortrait, imageFor }
  }
})
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
