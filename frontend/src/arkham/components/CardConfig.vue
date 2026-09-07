<script lang="ts" setup>
import { computed, nextTick, onMounted, onUnmounted, ref } from 'vue';
import { OnClickOutside } from '@vueuse/components';
import CardOptionGroups from '@/arkham/components/CardOptionGroups.vue';
import { cardOptionName, useCardOptions } from '@/arkham/composables/useCardOptions';
import type { Game } from '@/arkham/types/Game';

/* A card's player-configurable options. Renders nothing at all unless the card
 * declares options in its CardDef (`cdOptions` on the backend), so it is safe to
 * mount unconditionally on any card component. */
const props = defineProps<{
  game: Game
  playerId: string
  cardCode: string
  /* Cards are small at the tightest breakpoint, so the caller can ask for the
   * compact glyph. */
  compact?: boolean
}>();

const frame = ref<HTMLElement | null>(null);
const panelRef = ref<HTMLElement | null>(null);
const shown = ref(false);
const panelPosition = ref<Record<string, string>>({});

const cardName = computed(() => cardOptionName(props.cardCode));

const { options, groups, configured, isOn, valueOf, valuesOf, label, valueLabel, inputId, set } =
  useCardOptions(
    computed(() => props.game),
    computed(() => props.playerId),
    computed(() => props.cardCode),
  );

function calculatePosition() {
  if (!frame.value) return;
  const rect = frame.value.getBoundingClientRect();
  const menuWidth = panelRef.value?.getBoundingClientRect().width ?? 330;
  const margin = 8;
  const maxLeft = Math.max(margin, window.innerWidth - menuWidth - margin);
  panelPosition.value = {
    bottom: `${window.innerHeight - rect.top + margin}px`,
    left: `${Math.min(Math.max(rect.left + rect.width / 2 - menuWidth / 2, margin), maxLeft)}px`,
  };
}

function open() {
  shown.value = true;
  nextTick(calculatePosition);
}

const close = () => { shown.value = false };

function updatePosition() {
  if (shown.value) calculatePosition();
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
  <button
    v-if="options.length > 0"
    ref="frame"
    type="button"
    class="card-config-gear"
    :class="{ 'card-config-gear--on': configured, 'card-config-gear--compact': compact }"
    :aria-label="$t('cardOption.configure')"
    v-tooltip="$t('cardOption.configure')"
    @click.stop.prevent="shown ? close() : open()"
  >
    <font-awesome-icon icon="gear" />
  </button>

  <Teleport to="body">
    <OnClickOutside v-if="shown" @trigger="close" :options="{ ignore: [frame] }">
      <div ref="panelRef" class="card-config no-card-overlay" :style="panelPosition">
        <div class="card-config__header">
          <h2 class="card-config__title">{{ cardName }}</h2>
        </div>
        <div class="card-config__body">
          <CardOptionGroups
            compact
            :groups="groups"
            :label="label"
            :isOn="isOn"
            :valueOf="valueOf"
            :valuesOf="valuesOf"
            :valueLabel="valueLabel"
            :inputId="inputId"
            :set="set"
          />
        </div>
      </div>
    </OnClickOutside>
  </Teleport>
</template>

<style scoped>
/* Top-left is the only corner the token pool cannot reach: `.pool` is anchored
   at `top: 50%` and only ever grows downward, spilling past the card's bottom
   edge once it wraps. Asset art is ~60px wide, so the glyph stays small and
   bare — no pill, no border — with drop-shadows to survive light art. The one
   other tenant of this corner is the jammed wrench, which steps aside in
   Asset.vue when a card declares options. */
.card-config-gear {
  position: absolute;
  left: 1px;
  top: 1px;
  z-index: var(--z-index-3);
  display: grid;
  place-items: center;
  width: 18px;
  height: 18px;
  padding: 0;
  margin: 0;
  border: 0;
  background: none;
  cursor: pointer;
  color: rgba(255, 255, 255, 0.62);
  font-size: 12px;
  filter:
    drop-shadow(0 0 1px rgba(0, 0, 0, 0.95))
    drop-shadow(0 1px 2px rgba(0, 0, 0, 0.85));
  transition: color 0.15s ease;
}

.card-config-gear :deep(svg) {
  transition: transform 0.15s ease;
}

.card-config-gear:hover {
  color: var(--text);
}

.card-config-gear:hover :deep(svg) {
  transform: rotate(35deg);
}

.card-config-gear--on {
  color: var(--highlight);
  filter:
    drop-shadow(0 0 1px rgba(0, 0, 0, 0.95))
    drop-shadow(0 0 4px rgba(45, 212, 191, 0.75));
}

.card-config-gear--compact {
  width: 16px;
  height: 16px;
  font-size: 11px;
}

@media (max-width: 800px) and (orientation: portrait) {
  .card-config-gear {
    font-size: 11px;
  }
}
</style>

<style>
/* Unscoped: the panel is teleported to body. Deliberately mirrors
   arkham/components/Settings.vue — same header/section/toggle-row/segmented
   vocabulary — so card options read as the same kind of thing as game settings. */
.card-config {
  position: fixed;
  z-index: var(--z-modal-overlay);
  display: flex;
  flex-direction: column;
  width: min(330px, calc(100vw - 16px));
  max-height: 70vh;
  background: var(--background);
  color: var(--text);
  border: 1px solid var(--box-border);
  border-radius: 5px;
  box-shadow: 0 8px 30px rgba(0, 0, 0, 0.6);
  overflow: hidden;
}

.card-config__header {
  flex-shrink: 0;
  padding: 8px 14px;
  background: var(--background-dark);
  border-bottom: 1px solid var(--box-border);
}

.card-config__title {
  margin: 0;
  font-family: Teutonic, serif;
  font-size: 17px;
  color: var(--text);
  text-transform: none;
}

.card-config__body {
  flex: 1 1 auto;
  min-height: 0;
  overflow-y: auto;
  padding: 12px 14px;
  display: flex;
  flex-direction: column;
  gap: 16px;
}

/* The ability's printed text heads its group the way `.section-title` does in
   the settings pane, so the settings below plainly belong to it — but set as
   card text rather than an uppercase label, since it is a sentence. */












</style>
