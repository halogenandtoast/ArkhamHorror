<script setup lang="ts">
import type { Game } from '@/arkham/types/Game';
import { OnClickOutside } from '@vueuse/components'
import { ref, watch, computed, nextTick, onMounted, onUnmounted } from 'vue';
import type { AbilityMessage } from '@/arkham/types/Message';
import AbilityButton from '@/arkham/components/AbilityButton.vue'

const props = withDefaults(defineProps<{
  game: Game;
  abilities: AbilityMessage[];
  frame: HTMLElement | null;
  position?: 'top' | 'bottom' | 'left' | 'right';
  showMove?: boolean
  hostHasSwarm?: boolean
  playAction?: number
}>(), {showMove: true, hostHasSwarm: false});

const emits = defineEmits<{
  (e: 'choose', index: number): void;
}>();

interface Position {
  bottom?: string;
  top?: string;
  left?: string;
  right?: string;
}

const abilitiesRef = ref<HTMLElement | null>(null);
const showAbilities = defineModel()
const abilitiesPosition = ref<Position>({ bottom: '0px', top: '0px', left: '0px' });
const positionClass = computed(() => props.position || 'top');

function calculatePosition() {
  if (props.frame) {
    const rect = props.frame.getBoundingClientRect();
    const menuRect = abilitiesRef.value?.getBoundingClientRect();
    const menuWidth = menuRect?.width ?? 160;
    const margin = 8;
    const maxLeft = Math.max(margin, window.innerWidth - menuWidth - margin);
    const clampedLeft = (left: number) => `${Math.min(Math.max(left, margin), maxLeft)}px`;
    const positionStyle: Record<string, string> = {};

    switch (positionClass.value) {
      case 'bottom':
        positionStyle.top = `${rect.bottom}px`;
        positionStyle.left = clampedLeft(rect.left);
        break;
      case 'left':
        positionStyle.top = `${rect.top}px`;
        if (rect.left - menuWidth - margin < 0) {
          positionStyle.left = clampedLeft(rect.right + margin);
        } else {
          positionStyle.right = `${window.innerWidth - rect.left}px`;
        }
        break;
      case 'right':
        positionStyle.top = `${rect.top}px`;
        if (rect.right + menuWidth + margin > window.innerWidth) {
          positionStyle.right = `${window.innerWidth - rect.left + margin}px`;
        } else {
          positionStyle.left = clampedLeft(rect.right + margin);
        }
        break;
      case 'top':
      default:
        positionStyle.bottom = `${window.innerHeight - rect.top}px`;
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
  }
});

function updatePosition() {
  if (showAbilities.value) calculatePosition();
}

onMounted(() => {
  window.addEventListener('resize', updatePosition);
  window.addEventListener('scroll', updatePosition, true);
});

onUnmounted(() => {
  window.removeEventListener('resize', updatePosition);
  window.removeEventListener('scroll', updatePosition, true);
});
</script>

<template>
  <Teleport to="body">
    <OnClickOutside @trigger="showAbilities = false" v-if="showAbilities" :options="{ ignore: [frame] }">
      <div class="abilities" :class="positionClass" :style="abilitiesPosition" ref="abilitiesRef" >
        <button
          v-if="playAction !== undefined"
          class="play-card-button"
          @click="chooseAbility(playAction)"
        >
          <span class="button-label">{{ $t('label.play') }}</span>
        </button>
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
  position: fixed;
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

  :deep(.button-label),
  .button-label {
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

.play-card-button {
  border: 0;
  color: #fff;
  cursor: pointer;
  border-radius: 4px;
  background-color: var(--button);
  z-index: var(--z-index-1000);
  width: 100%;
  min-width: max-content;
  display: inline-flex;
  align-items: stretch;
  justify-content: center;
  gap: 0;
  overflow: hidden;
}

.play-card-button::before {
  content: "\1F0CF";
  display: inline-flex;
  align-items: center;
  justify-content: center;
  align-self: stretch;
  padding: 3px 6px;
  background: rgba(255, 255, 255, 0.12);
}
</style>
