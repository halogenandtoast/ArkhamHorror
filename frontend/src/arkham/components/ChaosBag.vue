<script lang="ts" setup>
import { computed, inject } from 'vue';
import { Game } from '@/arkham/types/Game';
import * as ArkhamGame from '@/arkham/types/Game';
import { SkillTest } from '@/arkham/types/SkillTest';
import { MessageType } from '@/arkham/types/Message';
import { ChaosBag } from '@/arkham/types/ChaosBag';
import Token from '@/arkham/components/Token';
import ChaosBagChoice from '@/arkham/components/ChaosBagChoice';

export interface Props {
  game: Game
  skillTest?: SkillTest
  chaosBag: ChaosBag
  investigatorId: string
}

const props = defineProps<Props>()

const emit = defineEmits(['choose'])

const baseUrl = inject('baseUrl')

function imageFor(tokenFace: string) {
  switch (tokenFace) {
    case 'PlusOne':
      return `${baseUrl}/img/arkham/ct_plus1.png`;
    case 'Zero':
      return `${baseUrl}/img/arkham/ct_0.png`;
    case 'MinusOne':
      return `${baseUrl}/img/arkham/ct_minus1.png`;
    case 'MinusTwo':
      return `${baseUrl}/img/arkham/ct_minus2.png`;
    case 'MinusThree':
      return `${baseUrl}/img/arkham/ct_minus3.png`;
    case 'MinusFour':
      return `${baseUrl}/img/arkham/ct_minus4.png`;
    case 'MinusFive':
      return `${baseUrl}/img/arkham/ct_minus5.png`;
    case 'MinusSix':
      return `${baseUrl}/img/arkham/ct_minus6.png`;
    case 'MinusSeven':
      return `${baseUrl}/img/arkham/ct_minus7.png`;
    case 'MinusEight':
      return `${baseUrl}/img/arkham/ct_minus8.png`;
    case 'AutoFail':
      return `${baseUrl}/img/arkham/ct_autofail.png`;
    case 'ElderSign':
      return `${baseUrl}/img/arkham/ct_eldersign.png`;
    case 'Skull':
      return `${baseUrl}/img/arkham/ct_skull.png`;
    case 'Cultist':
      return `${baseUrl}/img/arkham/ct_cultist.png`;
    case 'Tablet':
      return `${baseUrl}/img/arkham/ct_tablet.png`;
    case 'ElderThing':
      return `${baseUrl}/img/arkham/ct_elderthing.png`;
    default:
      return `${baseUrl}/img/arkham/ct_blank.png`;
  }
}

const revealedTokens = computed(() => {
  if (props.game.focusedTokens.length > 0) {
    return props.game.focusedTokens;
  }

  if (props.game.skillTestTokens !== []) {
    return props.game.skillTestTokens;
  }

  return [];
})

const choices = computed(() => ArkhamGame.choices(props.game, props.investigatorId))

const tokenAction = computed(() => choices.value.findIndex((c) => c.tag === "StartSkillTestButton"))

const investigatorPortrait = computed(() => {
  const choice = choices.value.find((c) => c.tag === "StartSkillTestButton");
  if (choice) {
    return `${baseUrl}/img/arkham/portraits/${choice.investigatorId.replace('c', '')}.jpg`;
  }

  if (props.skillTest) {
    return `${baseUrl}/img/arkham/portraits/${props.skillTest.investigator.replace('c', '')}.jpg`;
  }

  return null;
})

const debug = inject('debug')
const debugChoose = inject('debugChoose')

const tokenFaces = computed(() => [...new Set(props.chaosBag.tokens.map(t => t.tokenFace))])

const tokenGroups = computed(() => {
  return Object.
    entries(choices.value).
    filter(([, el]) => el.tag == "ChooseTokenGroups").
    map(([idx, el]) => [idx, el.contents[2].contents[2]])
})

const choose = (idx: number) => emit('choose', idx)
</script>

<template>
  <div>
    <img
      v-if="investigatorPortrait"
      class="portrait"
      :src="investigatorPortrait"
    />
    <Token v-for="(revealedToken, index) in revealedTokens" :key="index" :token="revealedToken" :investigatorId="investigatorId" :game="game" @choose="choose" />
    <img
      v-if="tokenAction !== -1"
      class="token token--can-draw"
      :src="`${baseUrl}/img/arkham/ct_blank.png`"
      @click="choose(tokenAction)"
    />
    <template v-if="debug && tokenAction !== -1">
      <div class="token-debug" v-for="tokenFace in tokenFaces" :key="tokenFace" @click="debugChoose({tag: 'ForceTokenDraw', contents: tokenFace})">
        <img
          class="token"
          :src="imageFor(tokenFace)"
        />
        </div>
    </template>
    <ChaosBagChoice v-if="chaosBag.choice" :choice="chaosBag.choice" />
    <div v-for="tokenGroup in tokenGroups" :key="tokenGroup[0]">
      <div v-for="(group, idx) in tokenGroup[1]" :key="idx" @click="choose(parseInt(tokenGroup[0]))">
        <img
          v-for="(token, idx) in group"
          :key="idx"
          class="token active-token"
          :src="imageFor(token.tokenFace)"
        />
        </div>
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
</style>
