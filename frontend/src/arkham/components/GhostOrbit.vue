<script lang="ts" setup>
import { computed } from 'vue';
import ghostOne from '@/assets/ghost-orbit-1.svg';
import ghostTwo from '@/assets/ghost-orbit-2.svg';
import ghostThree from '@/assets/ghost-orbit-3.svg';
import type { SpiritGhostKey, SpiritGhostMotion } from '@/arkham/spiritVisuals';

const props = withDefaults(defineProps<{
  variant?: 'deck' | 'card'
  ghostKey?: SpiritGhostKey
  motion?: SpiritGhostMotion | null
}>(), {
  variant: 'card',
  ghostKey: 'deck',
  motion: null,
})

const ghostImage = computed(() => {
  switch (props.ghostKey) {
    case 'mourn':
      return ghostTwo;
    case 'watcher':
      return ghostThree;
    case 'deck':
    case 'wisp':
    default:
      return ghostOne;
  }
})

const orbitStyle = computed(() => {
  if (!props.motion) {
    return {};
  }

  return {
    '--ghost-duration': props.motion.ghostDuration,
    '--ghost-delay': props.motion.ghostDelay,
    '--mist-inner-duration': props.motion.mistInnerDuration,
    '--mist-inner-delay': props.motion.mistInnerDelay,
    '--mist-outer-duration': props.motion.mistOuterDuration,
    '--mist-outer-delay': props.motion.mistOuterDelay,
  };
})
</script>

<template>
  <div class="ghost-orbit" :class="`ghost-orbit--${props.variant}`" :style="orbitStyle" aria-hidden="true">
    <div class="ghost-orbit__mist ghost-orbit__mist--inner"></div>
    <div class="ghost-orbit__mist ghost-orbit__mist--outer"></div>
    <img :src="ghostImage" class="ghost-orbit__ghost ghost-orbit__ghost--one" alt="" />
  </div>
</template>

<style scoped>
.ghost-orbit {
  --ghost-duration: 10.5s;
  --ghost-delay: 0s;
  --mist-inner-duration: 8s;
  --mist-inner-delay: 0s;
  --mist-outer-duration: 14s;
  --mist-outer-delay: 0s;
  pointer-events: none;
  position: absolute;
  inset: -5%;
  z-index: 4;
  overflow: visible;
  filter: saturate(1.08);
}

.ghost-orbit--deck {
  inset: -12%;
}

.ghost-orbit__ghost,
.ghost-orbit__mist {
  position: absolute;
  will-change: transform, opacity;
}

.ghost-orbit__ghost {
  width: 26%;
  min-width: 28px;
  max-width: 48px;
  opacity: 0.62;
  filter: drop-shadow(0 0 12px rgba(184, 244, 255, 0.55)) blur(0.2px);
  transform-origin: center;
  mix-blend-mode: screen;
}

.ghost-orbit--deck .ghost-orbit__ghost {
  width: 30%;
  min-width: 36px;
  max-width: 58px;
  opacity: 0.72;
  filter: drop-shadow(0 0 18px rgba(184, 244, 255, 0.7)) blur(0.4px);
}

.ghost-orbit__ghost--one {
  top: 8%;
  right: 10%;
  animation: drift-three var(--ghost-duration) ease-in-out infinite;
  animation-delay: var(--ghost-delay);
}

.ghost-orbit__mist {
  border-radius: 999px;
  background:
    radial-gradient(circle, rgba(235, 253, 255, 0.3) 0%, rgba(142, 199, 255, 0.18) 42%, rgba(77, 112, 255, 0) 72%);
  mix-blend-mode: screen;
}

.ghost-orbit__mist--inner {
  inset: 22%;
  animation: mist-inner var(--mist-inner-duration) ease-in-out infinite;
  animation-delay: var(--mist-inner-delay);
}

.ghost-orbit__mist--outer {
  inset: 10%;
  opacity: 0.52;
  filter: blur(10px);
  animation: mist-outer var(--mist-outer-duration) linear infinite;
  animation-delay: var(--mist-outer-delay);
}

.ghost-orbit--deck .ghost-orbit__mist--inner {
  inset: 14%;
}

.ghost-orbit--deck .ghost-orbit__mist--outer {
  inset: 0%;
}

@keyframes drift-three {
  0% { transform: translate3d(0, 0, 0) rotate(-6deg) scale(0.92); opacity: 0.38; }
  25% { transform: translate3d(-12px, 6px, 0) rotate(-11deg) scale(0.97); opacity: 0.56; }
  55% { transform: translate3d(-20px, 24px, 0) rotate(1deg) scale(1.02); opacity: 0.74; }
  78% { transform: translate3d(-8px, 42px, 0) rotate(9deg) scale(0.95); opacity: 0.5; }
  100% { transform: translate3d(0, 0, 0) rotate(-6deg) scale(0.92); opacity: 0.38; }
}

@keyframes mist-inner {
  0%, 100% { transform: scale(0.92); opacity: 0.34; }
  50% { transform: scale(1.06); opacity: 0.54; }
}

@keyframes mist-outer {
  from { transform: rotate(0deg) scale(0.98); }
  to { transform: rotate(360deg) scale(1.03); }
}
</style>
