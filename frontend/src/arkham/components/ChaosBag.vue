<script lang="ts" setup>
import { computed, inject } from 'vue';
import { Game } from '@/arkham/types/Game';
import { imgsrc } from '@/arkham/helpers';
import * as ArkhamGame from '@/arkham/types/Game';
import { SkillTest } from '@/arkham/types/SkillTest';
import { MessageType } from '@/arkham/types/Message';
import { ChaosBag } from '@/arkham/types/ChaosBag';
import Token from '@/arkham/components/Token.vue';
import ChaosBagChoice from '@/arkham/components/ChaosBagChoice.vue';

export interface Props {
  game: Game
  skillTest?: SkillTest
  chaosBag: ChaosBag
  investigatorId: string
}

const props = defineProps<Props>()

const emit = defineEmits(['choose'])

function imageFor(tokenFace: string) {
  switch (tokenFace) {
    case 'PlusOne':
      return imgsrc("ct_plus1.png");
    case 'Zero':
      return imgsrc("ct_0.png");
    case 'MinusOne':
      return imgsrc("ct_minus1.png");
    case 'MinusTwo':
      return imgsrc("ct_minus2.png");
    case 'MinusThree':
      return imgsrc("ct_minus3.png");
    case 'MinusFour':
      return imgsrc("ct_minus4.png");
    case 'MinusFive':
      return imgsrc("ct_minus5.png");
    case 'MinusSix':
      return imgsrc("ct_minus6.png");
    case 'MinusSeven':
      return imgsrc("ct_minus7.png");
    case 'MinusEight':
      return imgsrc("ct_minus8.png");
    case 'AutoFail':
      return imgsrc("ct_autofail.png");
    case 'ElderSign':
      return imgsrc("ct_eldersign.png");
    case 'Skull':
      return imgsrc("ct_skull.png");
    case 'Cultist':
      return imgsrc("ct_cultist.png");
    case 'Tablet':
      return imgsrc("ct_tablet.png");
    case 'ElderThing':
      return imgsrc("ct_elderthing.png");
    default:
      return imgsrc("ct_blank.png");
  }
}

const revealedChaosTokens = computed(() => {
  if (props.game.focusedChaosTokens.length > 0) {
    return props.game.focusedChaosTokens;
  }

  if (props.game.skillTestChaosTokens !== []) {
    return props.game.skillTestChaosTokens;
  }

  return [];
})

const choices = computed(() => ArkhamGame.choices(props.game, props.investigatorId))

const tokenAction = computed(() => choices.value.findIndex((c) => c.tag === MessageType.START_SKILL_TEST_BUTTON))

const investigatorPortrait = computed(() => {
  const choice = choices.value.find((c) => c.tag === MessageType.START_SKILL_TEST_BUTTON)
  if (choice) {
    const player = props.game.investigators[choice.investigatorId]

    if (player.isYithian) {
      return imgsrc(`portraits/${choice.investigatorId.replace('c', '')}.jpg`)
    }

    return imgsrc(`portraits/${player.cardCode.replace('c', '')}.jpg`)
  }

  if (props.skillTest) {
    const player = props.game.investigators[props.skillTest.investigator]

    if (player.isYithian) {
      return imgsrc(`portraits/${props.skillTest.investigator.replace('c', '')}.jpg`)
    }

    return imgsrc(`portraits/${player.cardCode.replace('c', '')}.jpg`)
  }

  return null;
})

const debug = inject('debug')
const debugChoose = inject('debugChoose')

const tokenFaces = computed(() => [...new Set(props.chaosBag.chaosTokens.map(t => t.face))])

const choose = (idx: number) => emit('choose', idx)
</script>

<template>
  <div class="chaos-bag-contents">
    <img
      v-if="investigatorPortrait"
      class="portrait"
      :src="investigatorPortrait"
    />
    <Token v-for="(revealedToken, index) in revealedChaosTokens" :key="index" :token="revealedToken" :investigatorId="investigatorId" :game="game" @choose="choose" />
    <img
      v-if="tokenAction !== -1"
      class="token token--can-draw"
      :src="imgsrc('ct_blank.png')"
      @click="choose(tokenAction)"
    />
    <template v-if="debug && tokenAction !== -1">
      <div class="token-debug" v-for="tokenFace in tokenFaces" :key="tokenFace" @click="debugChoose({tag: 'ForceChaosTokenDraw', contents: tokenFace})">
        <img
          class="token"
          :src="imageFor(tokenFace)"
        />
        </div>
    </template>
    <ChaosBagChoice v-if="chaosBag.choice && !game.skillTestResults" :choice="chaosBag.choice" :game="game" :investigatorId="investigatorId" @choose="choose" />
  </div>
</template>

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
  width: $card-width;
}

.active-token {
  border: 5px solid #ff00ff;
  border-radius: 500px;
  cursor: pointer;
}

.ignored {
  position: relative;
  filter: sepia(1);
}

.token-container {
  display: inline-block;
}

.token-debug {
  display: inline;
  img {
    cursor: pointer;
    border: 3px solid #ff00ff;
    border-radius: 25px;
    width: 50px;
  }
}

.chaos-bag-contents {
  display: flex;
  align-items: flex-start;
  height: 113px;
}
</style>
