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
  scrutinized?: boolean
}>(), { cancelled: false, selected: false, scrutinized: false })
const emit = defineEmits(['choose'])

const image = computed(() => chaosTokenImage(props.token.face))
const treatedAsFaces = computed(() =>
  (props.token.modifiedFaces ?? []).filter((face) => face !== props.token.face)
)
const faceLabel = (face: string) => face
  .replace(/Token$/, '')
  .replace(/([a-z])([A-Z])/g, '$1 $2')
const treatmentLabel = computed(() =>
  `${faceLabel(props.token.face)} is treated as ${treatedAsFaces.value.map(faceLabel).join(' and ')}`
)
const choices = computed(() => ArkhamGame.choices(props.game, props.playerId))

const revealedTokenAction = computed(() => {
  if (props.cancelled || props.selected) {
    return -1
  }

  return choices.value.findIndex((c) => {
    if (c.tag === "ChaosTokenGroupChoice") {
      if (!('tokenGroups' in c.step)) {
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
  cancelled: props.cancelled,
  scrutinized: props.scrutinized
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
      :src="imgsrc('chaos-tokens/ct-blank.png')"
    />
    <div
      v-if="treatedAsFaces.length > 0"
      class="treated-as"
      :title="treatmentLabel"
      :aria-label="treatmentLabel"
    >
      <span class="treated-as-arrow" aria-hidden="true">→</span>
      <img
        v-for="face in treatedAsFaces"
        :key="face"
        class="treated-as-token"
        :src="chaosTokenImage(face)"
        alt=""
      />
    </div>
  </div>
</template>

<style scoped>
.active-token {
  border: 5px solid var(--select);
  border-radius: 500px;
  cursor: pointer;
}

.cancelled {
  filter: grayscale(100%) brightness(40%) sepia(100%) hue-rotate(-50deg) saturate(600%) contrast(0.8);
}

.selected {
  filter: grayscale(100%) brightness(40%) sepia(100%) hue-rotate(50deg) saturate(1000%) contrast(0.8);
}

.front.scrutinized {
  border-radius: 50%;
  outline: 5px solid #f2cf72;
  outline-offset: 4px;
  box-shadow: 0 0 0 3px rgba(242, 207, 114, 0.2), 0 0 24px rgba(242, 207, 114, 0.85);
}

.token {
  width: min(100px, calc(20vw + 10px));
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
  -webkit-filter: drop-shadow(1px 1px 6px var(--neutral-extra-dark));
  filter: drop-shadow(1px 1px 6px var(--neutral-extra-dark));
}

.front {
  transform-style: preserve-3d;
  opacity: 0;
  backface-visibility: hidden;
  animation: flip 0.2s linear;
  animation-fill-mode: forwards;
  animation-iteration-count: 2;
  -webkit-filter: drop-shadow(1px 1px 6px var(--neutral-extra-dark));
  filter: drop-shadow(1px 1px 6px var(--neutral-extra-dark));
}

.token-container {
  width: min(100px, 30vw);
  position: relative;
}

.treated-as {
  align-items: center;
  bottom: -4px;
  display: flex;
  filter: drop-shadow(1px 2px 3px var(--neutral-extra-dark));
  pointer-events: none;
  position: absolute;
  right: -8px;
  z-index: 2;
}

.treated-as-arrow {
  align-items: center;
  background: var(--neutral-dark);
  border: 1px solid rgba(255, 255, 255, 0.45);
  border-radius: 50%;
  color: white;
  display: flex;
  font-size: 14px;
  font-weight: 900;
  height: 22px;
  justify-content: center;
  margin-right: -4px;
  width: 22px;
  z-index: 1;
}

.treated-as-token {
  border: 2px solid rgba(255, 255, 255, 0.9);
  border-radius: 50%;
  height: min(42px, calc(8vw + 4px));
  width: min(42px, calc(8vw + 4px));
}
</style>
