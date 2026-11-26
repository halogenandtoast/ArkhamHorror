<script setup lang="ts">
import { computed, ref } from 'vue'
import { useI18n } from 'vue-i18n'
import { Dropdown } from 'floating-vue'
import { useEscape } from '@/composable/escape'
import ModifierPanel from '@/arkham/components/ModifierPanel.vue'
import { dominantPolarity, hasAlteredStat, type ModifierRow, type StatReading } from '@/arkham/modifierDisplay'

/* The whole of a card's modifier list, behind one pip.
 *
 * A board card is 60px wide (`--card-width`), so a badge per modifier was never
 * on. This is the smallest thing that can still be found: a count in the card's
 * one free corner, coloured by the most urgent thing it stands for, which opens
 * the full list on click or tap.
 *
 * The visible dot is 12px but the hit area is 28px and overhangs the card
 * corner, so a thumb can reach it without the card itself losing target area.
 */

const props = withDefaults(
  defineProps<{
    rows: ModifierRow[]
    /* The card's printed stats and what they have become. */
    stats?: StatReading[]
    /* Which corner to sit in. A card's top-right is free; an investigator
     * portrait has its action row along the top, so it takes the bottom. */
    corner?: 'top-right' | 'bottom-right'
  }>(),
  { corner: 'top-right' },
)

const { t } = useI18n()
const shown = ref(false)

/* Escape closes it, the way every other panel in the app behaves.
 * floating-vue's auto-hide covers the outside click but not the key. */
useEscape(() => { shown.value = false }, shown)

const polarity = computed(() => dominantPolarity(props.rows))

/* Has one of the card's PRINTED numbers actually moved?
 *
 * This is what decides how loud the pip is. A scenario-wide modifier -- The
 * Familiar gives every non-weakness enemy +2 health -- otherwise puts an
 * identical pip on every card in play, and a signal that is always on is
 * wallpaper. A card whose fight or health really changed should stand out
 * from one that merely behaves differently; both stay reachable. */
const altered = computed(() => hasAlteredStat(props.stats ?? []))

const title = computed(() =>
  props.rows.length === 1
    ? t('modifierPanel.count', { count: props.rows.length })
    : t('modifierPanel.countPlural', { count: props.rows.length }),
)
</script>

<template>
  <Dropdown
    v-if="rows.length > 0"
    v-model:shown="shown"
    class="modifier-pip-anchor"
    :class="`modifier-pip-anchor--${corner}`"
    :triggers="['click']"
    :auto-hide="true"
    :distance="6"
    placement="right-start"
    theme="modifier-popover"
  >
    <button
      type="button"
      class="modifier-pip"
      :class="[
        altered ? `modifier-pip--${polarity}` : 'modifier-pip--quiet',
        { 'modifier-pip--open': shown },
      ]"
      :aria-label="`${t('modifierPanel.open')} (${title})`"
      v-tooltip="shown ? undefined : title"
      @click.stop
    >
      <span class="modifier-pip__count">{{ rows.length }}</span>
    </button>

    <template #popper>
      <div class="modifier-popper">
        <ModifierPanel :rows="rows" :stats="stats" :magnify-sources="true" />
      </div>
    </template>
  </Dropdown>
</template>

<style scoped>
.modifier-pip-anchor {
  position: absolute;
  right: 0;
  /* Centred ON the corner, so three quarters of the hit area hangs off the
     card instead of covering art the player is trying to click. */
  transform: translate(50%, -50%);
  /* Above every in-wrapper badge (those run 1..7); the wrapper is its own
     stacking context, so this is local. There is no --z-index-8. */
  z-index: var(--z-index-10);
  line-height: 0;
}

.modifier-pip-anchor--top-right {
  top: 0;
}

.modifier-pip-anchor--bottom-right {
  bottom: 0;
  transform: translate(50%, 50%);
}

.modifier-pip {
  --pip-ink: #cfd4dc;
  display: grid;
  place-items: center;
  /* Sized for a thumb by default. A board card is only ~60px wide, so every
     pixel of hit area is a pixel the card loses -- a precise pointer gets a
     smaller one (see the `pointer: fine` rule below). */
  width: var(--modifier-pip-size, 30px);
  height: var(--modifier-pip-size, 30px);
  padding: 0;
  border: none;
  background: transparent;
  cursor: pointer;
  pointer-events: auto;
  -webkit-tap-highlight-color: transparent;
}

/* The dot itself, drawn inside the larger target. */
.modifier-pip__count {
  display: grid;
  place-items: center;
  width: 13px;
  height: 13px;
  border-radius: 999px;
  background: #15161a;
  border: 1px solid var(--pip-ink);
  box-shadow:
    0 0 0 1px rgba(0, 0, 0, 0.85),
    0 1px 3px rgba(0, 0, 0, 0.8),
    0 0 5px color-mix(in srgb, var(--pip-ink) 45%, transparent);
  color: var(--pip-ink);
  font-size: 9px;
  font-weight: 900;
  font-variant-numeric: tabular-nums;
  line-height: 1;
  transition: transform 0.12s ease, box-shadow 0.12s ease;
}

.modifier-pip:hover .modifier-pip__count,
.modifier-pip--open .modifier-pip__count {
  transform: scale(1.18);
  box-shadow:
    0 0 0 1px rgba(0, 0, 0, 0.85),
    0 1px 4px rgba(0, 0, 0, 0.85),
    0 0 9px color-mix(in srgb, var(--pip-ink) 70%, transparent);
}

.modifier-pip:focus-visible .modifier-pip__count {
  outline: 2px solid var(--select);
  outline-offset: 2px;
}

/* Same hues as the chips, so a pip and the rows behind it agree. */
.modifier-pip--buff,
.modifier-pip--grant {
  --pip-ink: #6fd3bf;
}

.modifier-pip--nerf {
  --pip-ink: #e8a33d;
}

.modifier-pip--ban {
  --pip-ink: #e05252;
}

/* Nothing printed changed: still findable, but it must not compete with a card
   whose numbers really moved. A scenario-wide modifier (The Familiar gives
   every non-weakness enemy +2 health) would otherwise put an identical pip on
   every card in play, and a signal that is always on is wallpaper. */
.modifier-pip--quiet {
  --pip-ink: #98a0ab;
}

.modifier-pip--quiet .modifier-pip__count {
  opacity: 0.7;
  box-shadow:
    0 0 0 1px rgba(0, 0, 0, 0.85),
    0 1px 2px rgba(0, 0, 0, 0.7);
}

.modifier-pip--quiet:hover .modifier-pip__count,
.modifier-pip--quiet.modifier-pip--open .modifier-pip__count {
  opacity: 1;
}

.modifier-popper {
  display: flex;
  flex-direction: column;
  gap: 8px;
  padding: 10px;
  min-width: 190px;
  max-width: min(78vw, 340px);
}

/* The panel scrolls rather than growing past the viewport on a phone. */
.modifier-popper :deep(.modifier-panel__body) {
  max-height: min(52vh, 420px);
}

/* A mouse hits a 13px dot without help; reclaim the rest for the card. */
@media (max-width: 800px) {

  .modifier-pip__count {
    width: 15px;
    height: 15px;
    font-size: 10px;
  }
}
</style>

<style>
/* Registered globally: floating-vue resolves a theme by class on the popper,
   which is teleported out of this component's scope. */
.v-popper--theme-modifier-popover .v-popper__inner {
  background: rgba(14, 15, 19, 0.95);
  backdrop-filter: blur(9px);
  border: 1px solid rgba(255, 255, 255, 0.13);
  border-radius: 11px;
  box-shadow: 0 10px 34px rgba(0, 0, 0, 0.6);
  color: #fff;
}

.v-popper--theme-modifier-popover .v-popper__arrow-outer {
  border-color: rgba(255, 255, 255, 0.13);
}

.v-popper--theme-modifier-popover .v-popper__arrow-inner {
  border-color: rgba(14, 15, 19, 0.95);
}
</style>
