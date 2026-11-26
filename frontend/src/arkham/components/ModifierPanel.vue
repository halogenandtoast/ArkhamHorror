<script setup lang="ts">
import { computed } from 'vue'
import { useI18n } from 'vue-i18n'
import { cardImg } from '@/arkham/helpers'
import ModifierChip from '@/arkham/components/ModifierChip.vue'
import ModifierGlyph from '@/arkham/components/ModifierGlyph.vue'
import {
  groupModifierRows,
  limitModifierSections,
  type ModifierRow,
  type StatReading,
} from '@/arkham/modifierDisplay'

/* Every modifier on one card, grouped, with what put it there.
 *
 * Used in two places with the same markup: beside the magnified card, and as a
 * panel the player can keep open. `magnifySources` is the difference -- inside
 * the magnifier a hoverable thumbnail would re-target the magnifier at the
 * source card and the panel would vanish under the cursor, so there the
 * thumbnails are inert.
 */

const props = withDefaults(
  defineProps<{
    rows: ModifierRow[]
    /* The card's printed stats and what they have become. Drawn above the
     * list, because "1 -> 3" is the answer and the rows below are the why. */
    stats?: StatReading[]
    /* Let a thumbnail be hovered/long-pressed to magnify the source card. */
    magnifySources?: boolean
    /* Cut the list off after this many rows and say how many were dropped.
     * For a copy of the panel that cannot be scrolled -- see
     * `limitModifierSections`. Left out, the whole list is drawn. */
    maxRows?: number
  }>(),
  { magnifySources: false },
)

const { t } = useI18n()

const grouped = computed(() => limitModifierSections(groupModifierRows(props.rows), props.maxRows))

/* Each reading names the glossary entry for the modifier that changes it, so
 * the accessible label is translated without a second table. */
const statLabel = (r: StatReading) =>
  `${t(`modifierTag.${r.labelKey}`)}: ${r.printed} \u2192 ${r.modified}`
</script>

<template>
  <div class="modifier-panel">
    <!-- Printed value, then what it has become. An arrow and two numbers, so
         it needs no translating. Only a stat that actually CHANGED gets a
         line: "Shroud 4" against a card printing 4 is the stat-line version
         of restating the card's own text. -->
    <ul v-if="stats && stats.length" class="modifier-panel__stats">
      <li
        v-for="reading in stats"
        :key="reading.key"
        class="modifier-panel__stat"
        :class="{
          'modifier-panel__stat--raised': reading.modified > reading.printed,
          'modifier-panel__stat--lowered': reading.modified < reading.printed,
        }"
        v-tooltip="statLabel(reading)"
        :aria-label="statLabel(reading)"
      >
        <ModifierGlyph :glyph="reading.glyph" />
        <!-- Named, not just drawn. A fist or a heart is vocabulary a player
             already reads off their cards; "shroud" has no printed icon, so
             an unlabelled glyph beside a number says nothing. -->
        <span class="modifier-panel__stat-name">{{ t(`modifierTag.${reading.labelKey}`) }}</span>
        <span class="modifier-panel__stat-was">{{ reading.printed }}</span>
        <span class="modifier-panel__stat-arrow" aria-hidden="true">&#8594;</span>
        <span class="modifier-panel__stat-value">{{ reading.modified }}</span>
      </li>
    </ul>

    <p v-if="rows.length === 0" class="modifier-panel__empty">{{ t('modifierPanel.none') }}</p>

    <div v-else class="modifier-panel__body">
      <section v-for="section in grouped.sections" :key="section.group" class="modifier-panel__section">
        <h4 class="modifier-panel__group">{{ t(`modifierGroup.${section.group}`) }}</h4>
        <ul class="modifier-panel__rows">
          <li v-for="(row, idx) in section.rows" :key="`${row.display.tag}-${idx}`" class="modifier-panel__row">
            <img
              v-if="row.art"
              class="modifier-panel__source"
              :src="cardImg(row.art)"
              :data-image-id="magnifySources ? row.art : undefined"
              :alt="t('modifierPanel.source')"
            />
            <span v-else class="modifier-panel__source modifier-panel__source--none" aria-hidden="true"></span>
            <ModifierChip :display="row.display" variant="row" />
          </li>
        </ul>
      </section>
    </div>

    <!-- Outside the list, not the last entry in it: it describes the cut, so it
         has to stay put even where the list above it does scroll. -->
    <p v-if="grouped.hidden > 0" class="modifier-panel__more">
      {{ t('modifierPanel.more', { count: grouped.hidden }) }}
    </p>
  </div>
</template>

<style scoped>
.modifier-panel {
  --source-size: 40px;
  display: flex;
  flex-direction: column;
  min-width: 0;
  color: #f0f0f0;
  font-family: sans-serif;
}

.modifier-panel__empty {
  margin: 0;
  font-size: 0.78rem;
  color: rgba(255, 255, 255, 0.55);
}

.modifier-panel__stats {
  list-style: none;
  margin: 0 0 9px;
  padding: 0 0 8px;
  display: flex;
  flex-wrap: wrap;
  gap: 4px 12px;
  border-bottom: 1px solid rgba(255, 255, 255, 0.12);
}

.modifier-panel__stat {
  display: inline-flex;
  align-items: center;
  gap: 5px;
  font-size: 1rem;
  font-variant-numeric: tabular-nums;
  line-height: 1;
  color: rgba(255, 255, 255, 0.92);
}

/* The printed number is kept beside the one that replaced it: seeing what the
   card says and what it now is together is the whole point. Dimmed rather than
   struck through -- a line across a single narrow digit ("1") swallows it. */
.modifier-panel__stat > .modifier-glyph {
  flex: 0 0 auto;
  width: 1em;
  height: 1em;
}

.modifier-panel__stat-name {
  font-size: 0.68em;
  font-weight: 700;
  letter-spacing: 0.04em;
  text-transform: uppercase;
  color: rgba(255, 255, 255, 0.55);
  white-space: nowrap;
}

.modifier-panel__stat-was {
  font-size: 0.86em;
  color: rgba(255, 255, 255, 0.42);
}

.modifier-panel__stat-arrow {
  font-size: 0.78em;
  color: rgba(255, 255, 255, 0.4);
}

.modifier-panel__stat-value {
  font-weight: 800;
}

.modifier-panel__stat--raised .modifier-panel__stat-value {
  color: #6fd3bf;
}

.modifier-panel__stat--lowered .modifier-panel__stat-value {
  color: #e8a33d;
}

.modifier-panel__body {
  display: flex;
  flex-direction: column;
  gap: 10px;
  min-width: 0;
  overflow-y: auto;
  overscroll-behavior: contain;
  /* Momentum scrolling inside a popover that is itself fixed-positioned. */
  -webkit-overflow-scrolling: touch;
}

.modifier-panel__more {
  margin: 7px 0 0;
  padding-top: 6px;
  border-top: 1px solid rgba(255, 255, 255, 0.12);
  font-size: 0.7rem;
  font-weight: 700;
  letter-spacing: 0.04em;
  color: rgba(255, 255, 255, 0.5);
}

.modifier-panel__section {
  min-width: 0;
}

.modifier-panel__group {
  margin: 0 0 4px;
  font-size: 0.64rem;
  font-weight: 700;
  letter-spacing: 0.1em;
  text-transform: uppercase;
  color: rgba(255, 255, 255, 0.42);
}

.modifier-panel__rows {
  list-style: none;
  margin: 0;
  padding: 0;
  display: flex;
  flex-direction: column;
  gap: 3px;
}

.modifier-panel__row {
  display: flex;
  align-items: center;
  gap: 7px;
  min-width: 0;
  margin: 0;
  padding: 3px 4px;
  border-radius: 6px;
  background: rgba(255, 255, 255, 0.045);
  font-size: 0.86rem;
}

/* The source card's own art identifies it faster than its name would, and the
   top of a card is where its name is. */
.modifier-panel__source {
  flex: 0 0 auto;
  width: var(--source-size);
  /* The top half of a card: its own 0.705 aspect, halved. A square crop cut
     across the middle of the art and read as a meaningless band. */
  height: calc(var(--source-size) * 0.705);
  border-radius: 3px;
  object-fit: cover;
  object-position: center top;
  background: rgba(0, 0, 0, 0.5);
  box-shadow: 0 1px 3px rgba(0, 0, 0, 0.6);
}

.modifier-panel__source--none {
  border: none;
  background: rgba(255, 255, 255, 0.05);
  box-shadow: none;
}

@media (max-width: 800px) {
  .modifier-panel {
    --source-size: 46px;
  }

  .modifier-panel__row {
    padding: 5px 4px;
    font-size: 0.92rem;
  }
}
</style>
