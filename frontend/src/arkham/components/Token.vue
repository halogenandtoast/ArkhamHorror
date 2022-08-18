<script lang="ts" setup>
import { ChaosToken } from '@/arkham/types/ChaosToken';
import { computed, inject } from 'vue';
import { Game } from '@/arkham/types/Game';
import * as ArkhamGame from '@/arkham/types/Game';

export interface Props {
  game: Game
  token: ChaosToken
  investigatorId: string
}

const props = defineProps<Props>()
const emit = defineEmits(['choose'])
const baseUrl = inject('baseUrl')

const image = computed(() => {
  switch (props.token.tokenFace) {
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
})

const choices = computed(() => ArkhamGame.choices(props.game, props.investigatorId))

const revealedTokenAction = computed(() => -1)
  // choices.value.findIndex((c) => c.tag === MessageType.TARGET_LABEL && (c.contents[0].contents == props.token.tokenFace || c.contents[0].contents.tokenId == props.token.tokenId)))
const isIgnored = computed(() => props.token.modifiers?.some(modifier => modifier.type.tag == 'IgnoreToken') || false)

const choose = (idx: number) => emit('choose', idx)
</script>

<template>
  <img
    v-if="revealedTokenAction !== -1"
    class="token active-token"
    :src="image"
    @click="choose(revealedTokenAction)"
  />
  <div v-else class="token-container" :class="{ ignored: isIgnored }">
    <img
      class="token"
      :src="image"
    />
  </div>
</template>
