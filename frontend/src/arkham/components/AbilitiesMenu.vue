<script setup lang="ts">
import type { Game } from '@/arkham/types/Game';
import { OnClickOutside } from '@vueuse/components'
import { ref, watch, computed, nextTick, onUnmounted, useId } from 'vue';
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
  (e: 'hover', value: boolean): void;
}>();

interface Position {
  bottom?: string;
  top?: string;
  left?: string;
  right?: string;
}

const abilitiesRef = ref<HTMLElement | null>(null);
const showAbilities = defineModel()
const abilitiesPosition = ref<Position>({});
// The measured path has nothing to say until it has read a rect. Painting before
// that pinned the menu to the top-left corner of the viewport (#6004).
const measured = ref(false);
const positionClass = computed(() => props.position || 'top');

// Every property the anchored path relies on has to be tested: anchor-name
// shipped ahead of position-area (Chromium 125-128 spelled it inset-area), and a
// browser in that gap would pass an anchor-name-only check, lose position-area,
// and render the menu at its static position -- far below the viewport, since it
// is teleported to the end of <body> (#5626).
const supportsAnchor =
  typeof CSS !== 'undefined'
  && !!CSS.supports
  && CSS.supports('anchor-name: --a')
  && CSS.supports('position-anchor: --a')
  && CSS.supports('position-area: right span-bottom')
  && CSS.supports('position-try-fallbacks: flip-block');
const anchorFailed = ref(false);
const useAnchor = computed(() => supportsAnchor && !anchorFailed.value);
const anchorName = `--ability-anchor-${useId().replace(/[^a-zA-Z0-9]/g, '')}`;
const anchorStyle = computed(() => {
  if (useAnchor.value) return { 'position-anchor': anchorName };
  if (!measured.value) return { ...abilitiesPosition.value, visibility: 'hidden' as const };
  return abilitiesPosition.value;
});

// Anchor positioning keeps the menu pinned when the card moves; browsers without
// it fall back to the measured fixed position below.
watch([() => props.frame, showAbilities], ([frame], old) => {
  if (!useAnchor.value) return;
  old?.[0]?.style.removeProperty('anchor-name');
  if (frame && showAbilities.value) {
    frame.style.setProperty('anchor-name', anchorName);
    // The frame ref is still null while the parent mounts, so a menu opened during
    // setup (a forced ability) only becomes verifiable once the frame lands here.
    nextTick(() => verifyAnchorPlacement());
  } else {
    frame?.style.removeProperty('anchor-name');
  }
}, { immediate: true });

function onScreen(rect: DOMRect) {
  return rect.bottom > 0 && rect.right > 0
    && rect.top < window.innerHeight && rect.left < window.innerWidth;
}

// An unresolved anchor leaves the menu fixed at its static position, which -- the
// menu being teleported to the end of <body> -- is off screen and unreachable
// (#5626). So an off-screen menu is the tell that the anchored path failed, but
// only while the card it points at is on screen: a forced ability auto-opens its
// menu whether or not the card has been scrolled out of the board, and latching
// there stranded every later menu on that card, the [fight] action among them,
// on the measured path (#6004).
function verifyAnchorPlacement() {
  if (!useAnchor.value || !abilitiesRef.value) return;
  const frame = props.frame;
  if (!frame || !frame.isConnected) return;
  if (onScreen(abilitiesRef.value.getBoundingClientRect())) return;
  if (!onScreen(frame.getBoundingClientRect())) return;

  anchorFailed.value = true;
  frame.style.removeProperty('anchor-name');
  nextTick(() => calculatePosition());
}

function calculatePosition() {
  if (useAnchor.value) return;
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
    measured.value = true;
  }
}

// The measured position is a snapshot, and the board moves in ways that fire
// neither resize nor scroll: the zoom slider scales the grid, the rotate control
// reorders it, a revealed location adds a column, an enemy moves between cells.
// Re-reading the frame's rect each frame is the only thing that catches them all,
// and it only runs while a menu is actually open on the measured path.
let trackHandle = 0;
let lastRect = '';

function trackFrame() {
  trackHandle = requestAnimationFrame(trackFrame);
  const frame = props.frame;
  if (!frame) return;
  const r = frame.getBoundingClientRect();
  // The menu's own width feeds clampedLeft and is wrong until it has been painted.
  const menuWidth = abilitiesRef.value?.getBoundingClientRect().width ?? 0;
  const key = `${r.left},${r.top},${r.right},${r.bottom},${menuWidth},${window.innerWidth},${window.innerHeight}`;
  if (key === lastRect) return;
  lastRect = key;
  calculatePosition();
}

function startTracking() {
  if (trackHandle) return;
  lastRect = '';
  trackHandle = requestAnimationFrame(trackFrame);
}

function stopTracking() {
  if (!trackHandle) return;
  cancelAnimationFrame(trackHandle);
  trackHandle = 0;
}

function chooseAbility(index: number) {
  emits('choose', index);
}

watch(
  () => props.abilities,
  (newAbilities) => {
    if (newAbilities.some(a => 'ability' in a.contents && a.contents.ability.type.tag === 'ForcedAbility')) {
      showAbilities.value = true;
      nextTick(() => { calculatePosition(); verifyAnchorPlacement(); });
    } else if (newAbilities.length === 0) {
      showAbilities.value = false;
    }
  },
  { immediate: true }
);

watch(showAbilities, (newValue) => {
  if (newValue) {
    nextTick(() => { calculatePosition(); verifyAnchorPlacement(); });
  }
});

function updatePosition() {
  if (showAbilities.value) calculatePosition();
}

// One place decides what the measured path costs: it listens and tracks only while
// it is the path in use and a menu is open, and gives both up again otherwise.
watch([useAnchor, showAbilities], ([anchored, open]) => {
  if (anchored) {
    window.removeEventListener('resize', updatePosition);
    window.removeEventListener('scroll', updatePosition, true);
  } else {
    window.addEventListener('resize', updatePosition);
    window.addEventListener('scroll', updatePosition, true);
  }

  if (!open) measured.value = false;
  if (!anchored && open) startTracking();
  else stopTracking();
}, { immediate: true });

onUnmounted(() => {
  props.frame?.style.removeProperty('anchor-name');
  stopTracking();
  window.removeEventListener('resize', updatePosition);
  window.removeEventListener('scroll', updatePosition, true);
});
</script>

<template>
  <Teleport to="body">
    <OnClickOutside @trigger="showAbilities = false" v-if="showAbilities" :options="{ ignore: [frame] }">
      <div class="abilities" :class="[positionClass, { anchored: useAnchor }]" :style="anchorStyle" ref="abilitiesRef" @mouseenter="emits('hover', true)" @mouseleave="emits('hover', false)" >
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

.abilities.anchored {
  position: fixed;
  position-try-fallbacks: flip-block, flip-inline;
}

.abilities.anchored.top { position-area: top span-right; }
.abilities.anchored.bottom { position-area: bottom span-right; }
.abilities.anchored.left { position-area: left span-bottom; margin-right: 8px; }
.abilities.anchored.right { position-area: right span-bottom; margin-left: 8px; }

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
