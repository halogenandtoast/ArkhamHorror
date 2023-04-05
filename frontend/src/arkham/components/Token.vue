<script lang="ts" setup>
import { ChaosToken } from '@/arkham/types/ChaosToken';
import { withDefaults, computed, inject } from 'vue';
import { Game } from '@/arkham/types/Game';
import * as ArkhamGame from '@/arkham/types/Game';

export interface Props {
  game: Game
  token: ChaosToken
  investigatorId: string
  cancelled?: boolean
  selected?: boolean
}

const props = withDefaults(defineProps<Props>(), { cancelled: false, selected: false })
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

const revealedTokenAction = computed(() => {
  if (props.cancelled || props.selected) {
    return -1
  }

  return choices.value.findIndex((c) => {
    if (c.tag === "TokenGroupChoice") {
      return c.step.tokenGroups.some((g) => g.some((t) => t.tokenId === props.token.tokenId))
    }

    if (c.tag === "TargetLabel") {
      if (c.target.tag === "TokenFaceTarget") {
        return props.token.tokenFace === c.target.contents

      }
      if (c.target.tag === "TokenTarget") {
        return props.token.tokenId === c.target.contents.tokenId
      }
    }

    return false
  })
})
const isIgnored = computed(() => props.token.modifiers?.some(modifier => modifier.type.tag == 'IgnoreToken') || false)

const choose = (idx: number) => emit('choose', idx)

const classObject = computed(() => ({
  'active-token': revealedTokenAction.value !== -1,
  ignored: isIgnored.value,
  selected: props.selected,
  cancelled: props.cancelled
}))
</script>

<template>
  <img
    class="token"
    :class="classObject"
    :src="image"
    @click="choose(revealedTokenAction)"
  />
</template>

<style lang="scss" scoped>
.active-token {
  border: 5px solid #ff00ff;
  border-radius: 500px;
  cursor: pointer;
}

.cancelled {
  filter: grayscale(100%) brightness(40%) sepia(100%) hue-rotate(-50deg) saturate(600%) contrast(0.8);
}

.selected {
  filter: grayscale(100%) brightness(40%) sepia(100%) hue-rotate(50deg) saturate(1000%) contrast(0.8);
}

.token {
  position:relative;
}
</style>
