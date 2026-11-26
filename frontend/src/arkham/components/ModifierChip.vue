<script setup lang="ts">
import { computed } from 'vue'
import { useI18n } from 'vue-i18n'
import ModifierGlyph from '@/arkham/components/ModifierGlyph.vue'
import { modifierLabel, type ModifierDisplay } from '@/arkham/modifierDisplay'

/* One modifier, as a glyph and a number.
 *
 * The number is drawn with real signs -- + for a gain, U+2212 for a loss, = for
 * a replacement, x for a doubling -- so the chip says what changed and by how
 * much without a single word. `needsLabel` modifiers get the localized sentence
 * instead, because no arrangement of icons says "cards cannot leave your
 * discard pile".
 *
 * Either way the chip carries the localized name as its tooltip and its
 * accessible label, so an icon-only chip is still readable and still
 * translated.
 */

const props = withDefaults(
  defineProps<{
    display: ModifierDisplay
    /* `rail` is the compact form that sits on a card; `row` is the panel's. */
    variant?: 'rail' | 'row'
  }>(),
  { variant: 'row' },
)

const { t, te } = useI18n()

/* `en` carries the full glossary (guarded by tests/modifierLabels.test.mjs),
 * so a miss is a new or campaign-specific tag. */
const label = computed(() => modifierLabel(props.display.tag, te, t))

/* A keyword, trait, action or slot name the engine supplied.
 *
 * Slots have a locale namespace (`slot.hand`), so `HandSlot` is translated.
 * A TRAIT resolves through `trait.*`, which is generated from the engine's own
 * `Arkham.Trait` and mirrors its `displayTrait = splitCamelCase . show`. That
 * matters even in English: the wire value is the bare constructor, so without
 * it a multi-word trait renders as "AncientOne". A homebrew trait is declared
 * in the database and so has no key -- it falls back to the same camel split.
 *
 * Keywords and action names still do NOT resolve: the whole app shows those as
 * the English game term, and inventing a glossary for them here would make
 * this one panel disagree with every other surface. */
const splitCamel = (s: string) =>
  s.replace(/(?<=[a-z0-9])(?=[A-Z])/g, ' ').replace(/(?<=[A-Z])(?=[A-Z][a-z])/g, ' ')

const detail = computed(() => {
  const raw = props.display.detail
  if (!raw) return null
  if (isTrait.value) return te(`trait.${raw}`) ? t(`trait.${raw}`) : splitCamel(raw)
  const slotKey = `slot.${raw.replace(/Slot$/, '').toLowerCase()}`
  if (te(slotKey)) return t(slotKey)
  const key = `modifierDetail.${raw}`
  return te(key) ? t(key) : raw
})

const MINUS = '−'

const numeral = computed(() => {
  const { op, value } = props.display
  if (op === 'none' || value === undefined) return null
  if (op === 'set') return `=${value}`
  if (op === 'scale') return `×${value}`
  if (value === 0) return '±0'
  return value > 0 ? `+${value}` : `${MINUS}${Math.abs(value)}`
})

/* The rail -- the pip's compact preview -- stays wordless: it has no room, and
 * its tooltip carries the sentence. A ROW always names itself.
 *
 * The glyph and the signed number are still what you scan; the name is what
 * stops a column of "+2 / =5 / +2" being a decoding exercise. It is the one
 * place text is worth its width, and `modifierTag.*` translates it, so the
 * row reads in any language while the glyph still carries the meaning at a
 * glance. */
const showLabel = computed(() => props.variant === 'row')
const isTrait = computed(() => props.display.tag === 'AddTrait' || props.display.tag === 'RemoveTrait')
const showDetail = computed(() => props.variant === 'row' && !!detail.value)

/* A prohibition with no number is drawn struck through. Without it the glyph
 * says the opposite of what it means: "cannot be damaged" would be a damage
 * token on its own, which reads as more damage, not none. */
const barred = computed(() => props.display.polarity === 'ban' && !numeral.value)

const title = computed(() => {
  const parts = [label.value]
  if (detail.value) parts.push(detail.value)
  if (numeral.value) parts.push(numeral.value)
  return parts.join(' · ')
})
</script>

<template>
  <span
    class="modifier-chip"
    :class="[`modifier-chip--${display.polarity}`, `modifier-chip--${variant}`, { 'modifier-chip--barred': barred }]"
    v-tooltip="title"
    :aria-label="title"
    role="img"
  >
    <span class="modifier-chip__glyphs">
      <ModifierGlyph :glyph="display.glyph" />
      <ModifierGlyph v-if="display.qualifier" class="modifier-chip__qualifier" :glyph="display.qualifier" />
    </span>
    <span v-if="showLabel" class="modifier-chip__label">{{ label }}</span>
    <span
      v-if="showDetail"
      class="modifier-chip__detail"
      :class="{ 'modifier-chip__detail--trait': isTrait }"
    >{{ detail }}</span>
    <span v-if="numeral" class="modifier-chip__numeral">{{ numeral }}</span>
  </span>
</template>

<style scoped>
.modifier-chip {
  --chip-accent: rgba(255, 255, 255, 0.35);
  --chip-ink: #f2f2f2;
  flex: 1;
  display: inline-flex;
  align-items: center;
  gap: 0.25em;
  min-width: 0;
  color: var(--chip-ink);
  font-family: sans-serif;
  line-height: 1;
  user-select: none;
}

.modifier-chip__glyphs {
  position: relative;
  display: inline-flex;
  align-items: center;
  justify-content: center;
  flex: 0 0 auto;
  width: 1em;
  color: var(--chip-accent);
}

/* The qualifier rides the corner of the glyph it narrows, so the pair still
   reads as one symbol rather than two modifiers. */
.modifier-chip__qualifier {
  position: absolute;
  right: -0.3em;
  bottom: -0.25em;
  width: 0.62em;
  height: 0.62em;
  font-size: 0.62em;
  opacity: 0.95;
  filter: drop-shadow(0 0 1px #000) drop-shadow(0 0 2px #000);
}

/* Traits are printed bold italic on a card; the panel should not be the one
   surface that sets them as plain running text. */
.modifier-chip__detail--trait {
  font-weight: 700;
  font-style: italic;
  color: #f0f0f0;
}

.modifier-chip__numeral {
  flex: 0 0 auto;
  margin-left: auto;
  padding-left: 0.4em;
  font-weight: 800;
  font-variant-numeric: tabular-nums;
  letter-spacing: -0.02em;
  color: var(--chip-accent);
  text-shadow: 0 1px 1px rgba(0, 0, 0, 0.85);
}

/* Wraps rather than truncating: the sentence IS the content for a modifier no
   glyph can draw, so cutting it off defeats the row. Two lines is the budget. */
.modifier-chip__label,
.modifier-chip__detail {
  min-width: 0;
  font-size: 0.82em;
  color: rgba(255, 255, 255, 0.82);
  line-height: 1.25;
  overflow: hidden;
  display: -webkit-box;
  -webkit-box-orient: vertical;
  -webkit-line-clamp: 2;
  line-clamp: 2;
  overflow-wrap: anywhere;
}

.modifier-chip__detail {
  font-style: italic;
  opacity: 0.85;
}

/* Direction of change, not whose fortune: +2 enemy health and +2 of your
   willpower are the same colour, because which one is good news depends on
   where you are sitting. */
.modifier-chip--buff,
.modifier-chip--grant {
  --chip-accent: #6fd3bf;
}

.modifier-chip--nerf {
  --chip-accent: #e8a33d;
}

.modifier-chip--ban {
  --chip-accent: #e05252;
}

/* The strike, drawn over the glyph rather than beside it so the pair is one
   symbol. Outlined in black so it stays visible over token art. */
.modifier-chip--barred .modifier-chip__glyphs::after {
  content: "";
  position: absolute;
  left: -12%;
  right: -12%;
  top: 50%;
  height: 2px;
  border-radius: 2px;
  background: #ff6b6b;
  box-shadow: 0 0 0 1px rgba(0, 0, 0, 0.9);
  transform: rotate(-45deg);
  transform-origin: center;
  pointer-events: none;
}

.modifier-chip--neutral {
  --chip-accent: #cfd4dc;
}

.modifier-chip--rail {
  gap: 0.15em;
  font-size: 11px;
}

.modifier-chip--row {
  gap: 0.3em;
  font-size: 1em;
}
</style>
