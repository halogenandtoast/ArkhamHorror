<script setup lang="ts">
import type { Game } from '@/arkham/types/Game';
import { OnClickOutside } from '@vueuse/components'
import { ref, watch, computed, nextTick, getCurrentInstance } from 'vue';
import type { AbilityMessage } from '@/arkham/types/Message';
import AbilityButton from '@/arkham/components/AbilityButton.vue'

const props = withDefaults(defineProps<{
  game: Game;
  abilities: AbilityMessage[];
  frame: HTMLElement | null;
  position?: 'top' | 'bottom' | 'left' | 'right';
  showMove?: boolean
  hostHasSwarm?: boolean
}>(), {showMove: true, hostHasSwarm: false});

const emits = defineEmits<{
  (e: 'choose', index: number): void;
}>();

interface Position {
  bottom?: string;
  top?: string;
  left?: string;
  right?: string;
  positionAnchor?: string;
}

// Native CSS anchor positioning when available; JS math is the fallback below.
const supportsAnchor = typeof CSS !== 'undefined' && CSS.supports('anchor-name: --a');
const anchorName = `--abilities-anchor-${getCurrentInstance()?.uid ?? 0}`;

const abilitiesRef = ref<HTMLElement | null>(null);
const showAbilities = defineModel()
const abilitiesPosition = ref<Position>({ bottom: '0px', top: '0px', left: '0px' });
const positionClass = computed(() => props.position || 'top');

function calculatePosition() {
  if (props.frame && supportsAnchor) {
    // Name the frame and point the menu at it; scoped @supports CSS does the placement.
    props.frame.style.setProperty('anchor-name', anchorName);
    abilitiesPosition.value = { positionAnchor: anchorName };
    return;
  }

  if (props.frame) {
    const rect = props.frame.getBoundingClientRect();
    const menuRect = abilitiesRef.value?.getBoundingClientRect();
    const menuWidth = menuRect?.width ?? 160;
    const margin = 8;
    const maxLeft = Math.max(margin, window.innerWidth - menuWidth - margin);
    const clampedLeft = (left: number) => `${Math.min(Math.max(left, margin), maxLeft) + window.scrollX}px`;
    const positionStyle: Record<string, string> = {};

    switch (positionClass.value) {
      case 'bottom':
        positionStyle.top = `${rect.bottom + window.scrollY}px`;
        positionStyle.left = clampedLeft(rect.left);
        break;
      case 'left':
        positionStyle.top = `${rect.top + window.scrollY}px`;
        if (rect.left - menuWidth - margin < 0) {
          positionStyle.left = clampedLeft(rect.right + margin);
        } else {
          positionStyle.right = `${window.innerWidth - rect.left + window.scrollX}px`;
        }
        break;
      case 'right':
        positionStyle.top = `${rect.top + window.scrollY}px`;
        if (rect.right + menuWidth + margin > window.innerWidth) {
          positionStyle.right = `${window.innerWidth - rect.left + margin + window.scrollX}px`;
        } else {
          positionStyle.left = clampedLeft(rect.right + margin);
        }
        break;
      case 'top':
      default:
        positionStyle.bottom = `${window.innerHeight - rect.top - window.scrollY}px`;
        positionStyle.left = clampedLeft(rect.left);
        break;
    }

    abilitiesPosition.value = positionStyle;
  }
}

function chooseAbility(index: number) {
  emits('choose', index);
}

watch(
  () => props.abilities,
  (newAbilities) => {
    if (newAbilities.some(a => 'ability' in a.contents && a.contents.ability.type.tag === 'ForcedAbility')) {
      showAbilities.value = true;
      nextTick(() => calculatePosition());
    } else if (newAbilities.length === 0) {
      showAbilities.value = false;
    }
  },
  { immediate: true }
);

watch(showAbilities, (newValue) => {
  if (newValue) {
    nextTick(() => calculatePosition());
  } else if (supportsAnchor) {
    props.frame?.style.removeProperty('anchor-name');
  }
});
</script>

<template>
  <Teleport to="body">
    <OnClickOutside @trigger="showAbilities = false" v-if="showAbilities" :options="{ ignore: [frame] }">
      <div class="abilities" :class="positionClass" :style="abilitiesPosition" ref="abilitiesRef" >
        <AbilityButton
          v-for="{index, contents} in abilities"
          :key="index"
          :ability="contents"
          :show-move="showMove"
          :host-has-swarm="hostHasSwarm"
          :game="game"
          @click="chooseAbility(index)"
        />
      </div>
    </OnClickOutside>
  </Teleport>
</template>


<style scoped>
.abilities {
  position: absolute;
  padding: min(3px, 1vw);
  background: rgba(0, 0, 0, 0.8);
  border-radius: calc(10px - min(3px, 1vw));
  display: flex;
  flex-direction: column;
  gap: 5px;
  z-index: var(--z-modal-overlay);
  button {
    padding: 0;
    margin-top: 0;
    display: flex;
    align-items: center;
    flex-wrap: wrap;
    width: max(100%, 0.1vw);
    justify-content: center;
    min-height: fit-content;
    @media (max-width: 800px) and (orientation: portrait) {
      
      &:before {
        font-size: 2.0em !important;
      }
    }
  }

  :deep(.button-label) {
    padding-block: min(3px, 1vw);
    padding-inline: min(6px, 2vw);
  }

  :deep(span) {
    @media (max-width: 800px) and (orientation: portrait) {
      &:before {
        font-size: 2.0em !important;
      }
    }
  }
}

/* Progressive enhancement: pin to the frame via CSS anchor positioning.
   position-anchor is set inline (per-instance name); these replicate the JS
   placement for each variant. position-try flips off-screen menus back on. */
@supports (anchor-name: --a) {
  .abilities {
    position-try-fallbacks: flip-block, flip-inline, flip-block flip-inline;
  }
  .abilities.top {
    inset: auto;
    bottom: anchor(top);
    left: anchor(left);
  }
  .abilities.bottom {
    inset: auto;
    top: anchor(bottom);
    left: anchor(left);
  }
  .abilities.left {
    inset: auto;
    top: anchor(top);
    right: anchor(left);
  }
  .abilities.right {
    inset: auto;
    top: anchor(top);
    left: anchor(right);
  }
}
</style>
