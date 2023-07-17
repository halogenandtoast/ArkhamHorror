<script lang="ts" setup>
import { ChaosToken } from '@/arkham/types/ChaosToken';
import { withDefaults, computed } from 'vue';
import { imgsrc } from '@/arkham/helpers';
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

const image = computed(() => {
  switch (props.token.face) {
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
})

const choices = computed(() => ArkhamGame.choices(props.game, props.investigatorId))

const revealedTokenAction = computed(() => {
  if (props.cancelled || props.selected) {
    return -1
  }

  return choices.value.findIndex((c) => {
    if (c.tag === "TokenGroupChoice") {
      return c.step.tokenGroups.some((g) => g.some((t) => t.id === props.token.id))
    }

    if (c.tag === "TargetLabel") {
      if (c.target.tag === "TokenFaceTarget") {
        return props.token.face === c.target.contents

      }
      if (c.target.tag === "TokenTarget") {
        return props.token.id === c.target.contents.id
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
