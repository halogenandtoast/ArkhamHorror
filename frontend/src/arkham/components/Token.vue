<script lang="ts" setup>
import { ChaosToken, chaosTokenImage } from '@/arkham/types/ChaosToken';
import { computed } from 'vue';
import { imgsrc } from '@/arkham/helpers';
import { Game } from '@/arkham/types/Game';
import * as ArkhamGame from '@/arkham/types/Game';

const props = withDefaults(defineProps<{
  game: Game
  token: ChaosToken
  playerId: string
  cancelled?: boolean
  selected?: boolean
}>(), { cancelled: false, selected: false })
const emit = defineEmits(['choose'])

const image = computed(() => chaosTokenImage(props.token.face))
const choices = computed(() => ArkhamGame.choices(props.game, props.playerId))

const revealedTokenAction = computed(() => {
  if (props.cancelled || props.selected) {
    return -1
  }

  return choices.value.findIndex((c) => {
    if (c.tag === "ChaosTokenGroupChoice") {
      if (c.step.tag === 'Draw') {
        return false
      }
      return c.step.tokenGroups.some((g) => g.some((t) => t.id === props.token.id))
    }

    if (c.tag === "TargetLabel") {
      if (c.target.tag === "ChaosTokenFaceTarget") {
        return props.token.face === c.target.contents

      }
      if (c.target.tag === "ChaosTokenTarget" && c.target.contents) {
        return props.token.id === (c.target.contents as { face: string, id: string }).id
      }
    }

    return false
  })
})
const isIgnored = computed(() => props.token.modifiers?.some(modifier => modifier.type.tag === 'OtherModifier' && modifier.type.contents === 'IgnoreToken') || false)

const choose = (idx: number) => emit('choose', idx)

const classObject = computed(() => ({
  'active-token': revealedTokenAction.value !== -1,
  ignored: isIgnored.value,
  selected: props.selected,
  cancelled: props.cancelled
}))
</script>

<template>
  <div class="token-container">
    <img
      class="token front"
      :class="classObject"
      :src="image"
      @click="choose(revealedTokenAction)"
    />
    <img
      class="token back"
      :src="imgsrc('ct_blank.png')"
    />
  </div>
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
  width: 100px;
}

@keyframes flip {
  0% {
    transform: rotateY(180deg);
    opacity: 0;
  }

  49% {
    opacity: 0;
  }

  50% {
    opacity: 1;
  }

  100% {
    opacity: 1;
    transform: rotateY(0deg);
  }

}


.back {
  transform-style: preserve-3d;
  position: absolute;
  top: 0;
  left: 0;
  backface-visibility: hidden;
  animation: flip 0.2s linear;
  animation-direction: reverse;
  animation-fill-mode: forwards;
  animation-iteration-count: 2;
  -webkit-filter: drop-shadow(1px 1px 6px #222);
  filter: drop-shadow(1px 1px 6px #222);
}

.front {
  transform-style: preserve-3d;
  opacity: 0;
  backface-visibility: hidden;
  animation: flip 0.2s linear;
  animation-fill-mode: forwards;
  animation-iteration-count: 2;
  -webkit-filter: drop-shadow(1px 1px 6px #222);
  filter: drop-shadow(1px 1px 6px #222);
}

.token-container {
  width: 100px;
  position: relative;
}
</style>
