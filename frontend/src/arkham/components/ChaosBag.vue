<script lang="ts" setup>
import { computed } from 'vue';
import { useDebug } from '@/arkham/debug';
import { Game } from '@/arkham/types/Game';
import { imgsrc } from '@/arkham/helpers';
import * as ArkhamGame from '@/arkham/types/Game';
import { SkillTest } from '@/arkham/types/SkillTest';
import { MessageType, StartSkillTestButton } from '@/arkham/types/Message';
import { ChaosBag } from '@/arkham/types/ChaosBag';
import Token from '@/arkham/components/Token.vue';
import ChaosBagChoice from '@/arkham/components/ChaosBagChoice.vue';

const props = defineProps<{
  game: Game
  skillTest: SkillTest | null
  chaosBag: ChaosBag
  playerId: string
}>()

const emit = defineEmits<{
  choose: [value: number]
}>()

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
    case 'BlessToken':
      return imgsrc("ct_bless.png");
    case 'CurseToken':
      return imgsrc("ct_curse.png");
    default:
      return imgsrc("ct_blank.png");
  }
}

const revealedChaosTokens = computed(() => {
  if (props.game.focusedChaosTokens.length > 0) {
    const tokens = [...props.game.skillTestChaosTokens, ...props.game.focusedChaosTokens]
    return Array.from(new Set(tokens.map(JSON.stringify))).map(JSON.parse);
  }

  return props.game.skillTestChaosTokens;
})

const choices = computed(() => ArkhamGame.choices(props.game, props.playerId))
const tokenAction = computed(() => choices.value.findIndex((c) => c.tag === MessageType.START_SKILL_TEST_BUTTON))
const debug = useDebug()
const allTokenFaces = computed(() => props.chaosBag.chaosTokens.map(t => t.face).sort(sortTokenFaces))
const tokenOrder = ['PlusOne', 'Zero', 'MinusOne', 'MinusTwo', 'MinusThree', 'MinusFour', 'MinusFive', 'MinusSix', 'MinusSeven', 'MinusEight', 'Skull', 'Cultist', 'Tablet', 'ElderThing', 'AutoFail', 'ElderSign', 'CurseToken', 'BlessToken']

function sortTokenFaces(a: string, b: string) {
  return tokenOrder.indexOf(a) - tokenOrder.indexOf(b)
}

const choose = (idx: number) => emit('choose', idx)
</script>

<template>
  <div class="chaos-bag">
    <div class="chaos-bag-contents">
      <Token v-for="revealedToken in revealedChaosTokens" :key="revealedToken.id" :token="revealedToken" :playerId="playerId" :game="game" @choose="choose" />
      <img
        v-if="tokenAction !== -1"
        class="token token--can-draw"
        :src="imgsrc('ct_blank.png')"
        @click="choose(tokenAction)"
      />
      <ChaosBagChoice v-if="chaosBag.choice && 'step' in chaosBag.choice && !game.skillTestResults" :choice="chaosBag.choice.step" :game="game" :playerId="playerId" @choose="choose" />
    </div>

    <div v-if="debug.active && tokenAction !== -1" class="token-preview">
      <div class="token-debug" v-for="tokenFace in allTokenFaces" :key="tokenFace" @click="debug.send(game.id, {tag: 'ForceChaosTokenDraw', contents: tokenFace})">
        <img
          class="token"
          :src="imageFor(tokenFace)"
        />
        </div>
    </div>
    <div v-else class="token-preview">
      <img
        v-for="(tokenFace, idx) in allTokenFaces"
        :key="`${tokenFace}${idx}`"
        class="token"
        :src="imageFor(tokenFace)"
      />
    </div>
  </div>
</template>

<style scoped lang="scss">
.token--can-draw {
  border: 5px solid #ff00ff;
  border-radius: 500px;
  cursor: pointer;
}

.token {
  width: 100px;
  height: auto;
  margin-bottom: 10px;
}

.portrait {
  width: var(--card-width);
  height: auto;
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
  align-items: center;
  justify-content: center;
  flex-wrap: wrap;
}

.token-preview {
  display: flex;
  flex-wrap: wrap;
  flex-direction: row;
  justify-content: center;
  img {
    width: 30px;
    height: auto;
  }
}

.chaos-bag {
  padding: 10px;
  background: rgba(0,0,0,0.5);
  display: flex;
  flex-direction: column;
}
</style>
