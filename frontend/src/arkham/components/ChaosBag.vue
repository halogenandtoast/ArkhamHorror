<template>
  <div>
    <img
      v-if="investigatorPortrait"
      class="portrait"
      :src="investigatorPortrait"
    />
    <template v-for="(revealedToken, index) in revealedTokens" :key="index">
      <img
        v-if="revealedTokenAction(revealedToken) !== -1"
        class="token active-token"
        :src="imageFor(revealedToken)"
        @click="$emit('choose', revealedTokenAction(revealedToken))"
      />
      <div v-else class="token-container" :class="{ ignored: isIgnored(revealedToken) }">
        <img
          class="token"
          :src="imageFor(revealedToken)"
        />
      </div>
    </template>
    <img
      v-if="tokenAction !== -1"
      class="token token--can-draw"
      src="/img/arkham/ct_blank.png"
      @click="$emit('choose', tokenAction)"
    />
  </div>
</template>

<script lang="ts">
import { defineComponent, computed } from 'vue';
import { Game } from '@/arkham/types/Game';
import * as ArkhamGame from '@/arkham/types/Game';
import { SkillTest } from '@/arkham/types/SkillTest';
import { MessageType } from '@/arkham/types/Message';
import { ChaosToken } from '@/arkham/types/ChaosToken';


export default defineComponent({
  props: {
    game: { type: Object as () => Game, required: true },
    skillTest: { type: Object as () => SkillTest, required: false },
    investigatorId: { type: String, required: true }
  },
  setup(props) {
    function imageFor(token: ChaosToken) {
      switch (token.tokenFace) {
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

      if (props.game.currentData.skillTestTokens !== []) {
        return props.game.currentData.skillTestTokens;
      }

      return [];
    })

    const choices = computed(() => ArkhamGame.choices(props.game, props.investigatorId))

    function revealedTokenAction(token: string) {
      return choices.value.findIndex((c) => c.tag === MessageType.TARGET_LABEL && c.contents[0].contents == token)
    }

    const tokenAction = computed(() => choices.value.findIndex((c) => c.tag === MessageType.START_SKILL_TEST))

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

    const isIgnored = (token: ChaosToken) => token.modifiers?.some(modifier => modifier.type.tag == 'IgnoreTokenEffects') || false

    return { isIgnored, revealedTokens, tokenAction, revealedTokenAction, investigatorPortrait, imageFor }
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

.active-token {
  border: 5px solid #ff00ff;
  border-radius: 500px;
  cursor: pointer;
}

.ignored {
  position: relative;
  &::after {
    pointer-events: none;
    content: "";
    position: absolute;
    top: 0;
    left: 0;
    width: 100%;
    height: 100%;
    background-color: #FFF;
    opacity: .85;
    mix-blend-mode: saturation;
  }
}

.token-container {
  display: inline-block;
}
</style>
