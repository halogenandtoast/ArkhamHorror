<script setup lang="ts">
import { computed } from 'vue'
import { imgsrc } from '@/arkham/helpers'
import type { ModifierGlyph } from '@/arkham/modifierDisplay'

/* The picture half of a modifier row.
 *
 * Skills and tokens already have art the game uses everywhere, so those are
 * drawn with it and need no translation. The rest are inline paths: a stat an
 * entity prints (health, sanity, fight, evade, shroud) and a small set of
 * abstract shapes for things with no art of their own. Inline keeps them
 * sizing off font-size and recolouring with `currentColor`, which is what lets
 * one chip be 11px on a card rail and 20px in the panel.
 */

const props = defineProps<{ glyph: ModifierGlyph }>()

/* Beware the file names, which are the other way round from what they say:
 * `health.png` is the round damage token and `damage.png` is the heart. The
 * round tokens are what a card means by "deals 1 more damage"... */
const TOKEN_ART: Record<string, string> = {
  damage: 'health.png',
  horror: 'horror.png',
  clue: 'tokens/clue.png',
  doom: 'tokens/doom.png',
  resource: 'tokens/resource.png',
}

/* ...and the heart and brain are what it means by health and sanity, which is
 * how the printed cards mark those stats. */
const STAT_ART: Record<string, string> = {
  health: 'damage.png',
  sanity: 'tokens/horror-token.png',
}

const tokenSrc = computed(() =>
  props.glyph.kind === 'token' ? imgsrc(TOKEN_ART[props.glyph.token] ?? 'damage.png') : null,
)

const statSrc = computed(() =>
  props.glyph.kind === 'stat' ? (STAT_ART[props.glyph.stat] ? imgsrc(STAT_ART[props.glyph.stat]) : null) : null,
)

/* Fight and evade reuse the Arkham font's fist and boot, which is how the
 * printed cards show them. */
const ARKHAM_FOR_STAT: Record<string, string> = {
  fight: 'combat',
  evade: 'agility',
}

const arkhamClass = computed(() => {
  if (props.glyph.kind === 'arkham') return `${props.glyph.icon}-icon`
  if (props.glyph.kind === 'skill') return `${props.glyph.skill}-icon`
  if (props.glyph.kind === 'stat' && ARKHAM_FOR_STAT[props.glyph.stat]) {
    return `${ARKHAM_FOR_STAT[props.glyph.stat]}-icon`
  }
  return null
})

const skillColor = computed(() =>
  props.glyph.kind === 'skill' && props.glyph.skill !== 'wild' ? `var(--${props.glyph.skill})` : undefined,
)

/* A single 24x24 path per abstract shape, all drawn on the same grid so they
 * read as one family. `shroud` is here rather than in art because no shroud
 * icon exists in the project. */
const SHAPES: Record<string, string> = {
  shroud:
    'M12 3C6.8 3 2.7 7 1.5 12 2.7 17 6.8 21 12 21s9.3-4 10.5-9C21.3 7 17.2 3 12 3Zm0 14a5 5 0 1 1 0-10 5 5 0 0 1 0 10Zm0-7.5a2.5 2.5 0 1 0 0 5 2.5 2.5 0 0 0 0-5Z',
  ban: 'M12 2a10 10 0 1 0 0 20 10 10 0 0 0 0-20Zm0 2.5c1.7 0 3.2.5 4.4 1.4L5.9 16.4A7.5 7.5 0 0 1 12 4.5Zm0 15c-1.7 0-3.2-.5-4.4-1.4L18.1 7.6A7.5 7.5 0 0 1 12 19.5Z',
  allow:
    'M12 2a10 10 0 1 0 0 20 10 10 0 0 0 0-20Zm-1.1 14.6-4-4 1.8-1.8 2.2 2.2 5-5 1.8 1.8-6.8 6.8Z',
  card: 'M6 2.5h12A1.5 1.5 0 0 1 19.5 4v16A1.5 1.5 0 0 1 18 21.5H6A1.5 1.5 0 0 1 4.5 20V4A1.5 1.5 0 0 1 6 2.5Zm1.5 3v3h9v-3h-9Zm0 5v1.5h9V10.5h-9Zm0 3.5V15h6v-1.5h-6Z',
  deck: 'M4 6.5h11A1.5 1.5 0 0 1 16.5 8v12A1.5 1.5 0 0 1 15 21.5H4A1.5 1.5 0 0 1 2.5 20V8A1.5 1.5 0 0 1 4 6.5Zm3-4h11A1.5 1.5 0 0 1 19.5 4v12h-2V5.5H7v-3Z',
  hand: 'M4 11.5 7 6l2 1.2-1.6 2.8h3.1V3.5A1.5 1.5 0 0 1 13 2h.5v8h1.8l.5-4.5 2.4.3-.6 5.2a4 4 0 0 1-1 2.3l-2.3 2.6V21H6.5v-4.3L4 13.6a1.5 1.5 0 0 1 0-2.1Z',
  slot: 'M12 2.5 20.5 7v10L12 21.5 3.5 17V7L12 2.5Zm0 3.2L6.5 8.6v6.8L12 18.3l5.5-2.9V8.6L12 5.7Z',
  move: 'M12 2.5 15 7h-2v4h4V9l4.5 3-4.5 3v-2h-4v4h2l-3 4.5L9 17h2v-4H7v2l-4.5-3L7 9v2h4V7H9l3-4.5Z',
  cost: 'M12 2a10 10 0 1 0 0 20 10 10 0 0 0 0-20Zm1.2 15.4v1.4h-2v-1.3c-1.7-.2-3-1.2-3.3-2.9l2-.5c.2 1 .9 1.6 2 1.6 1 0 1.7-.5 1.7-1.2 0-.8-.6-1.1-2.1-1.5-2-.5-3.3-1.2-3.3-3 0-1.5 1.1-2.6 2.9-2.9V5.3h2v1.4c1.6.3 2.7 1.3 3 2.8l-2 .5c-.2-.9-.8-1.4-1.8-1.4-1 0-1.6.4-1.6 1.1 0 .7.6 1 2 1.4 2.1.5 3.4 1.2 3.4 3.1 0 1.6-1.1 2.8-2.8 3.2Z',
  test: 'M3.5 12.5 9 18l11.5-11.5L18.4 4.4 9 13.8l-3.4-3.4-2.1 2.1Z',
  keyword: 'M3 5.5h18v3H3v-3Zm0 5h12v3H3v-3Zm0 5h18v3H3v-3Z',
  trait: 'M12 2.5 14.9 9l7 .6-5.3 4.6 1.6 6.8-6.2-3.7-6.2 3.7 1.6-6.8L2.1 9.6l7-.6L12 2.5Z',
  victory: 'M7 3h10v2h3v4a4 4 0 0 1-3.6 4 5.5 5.5 0 0 1-2.4 2.8V18h3v3H7v-3h3v-2.2A5.5 5.5 0 0 1 7.6 13A4 4 0 0 1 4 9V5h3V3Zm0 4H6v2a2 2 0 0 0 1 1.7V7Zm10 0v3.7A2 2 0 0 0 18 9V7h-1Z',
  eye: 'M12 5c-5 0-8.7 3.5-10 7 1.3 3.5 5 7 10 7s8.7-3.5 10-7c-1.3-3.5-5-7-10-7Zm0 11a4 4 0 1 1 0-8 4 4 0 0 1 0 8Z',
  bolt: 'M13.5 2 5 13.5h5L9 22l9-12h-5.5L13.5 2Z',
  spawn: 'M12 2.5a6 6 0 0 1 6 6c0 4-6 12.5-6 12.5S6 12.5 6 8.5a6 6 0 0 1 6-6Zm0 3.5a2.5 2.5 0 1 0 0 5 2.5 2.5 0 0 0 0-5Z',
  ui: 'M3.5 4.5h17v12h-17v-12Zm2 2v8h13v-8h-13ZM8 19h8v2H8v-2Z',
  cog: 'M12 8.5a3.5 3.5 0 1 0 0 7 3.5 3.5 0 0 0 0-7Zm9 3.5c0 .6 0 1.1-.1 1.6l2 1.6-1.9 3.3-2.4-1a7.6 7.6 0 0 1-2.7 1.6l-.4 2.6h-3.8l-.4-2.6a7.6 7.6 0 0 1-2.7-1.6l-2.4 1L2.1 15.2l2-1.6a9 9 0 0 1 0-3.2l-2-1.6L4 5.5l2.4 1A7.6 7.6 0 0 1 9.1 5l.4-2.6h3.8l.4 2.6a7.6 7.6 0 0 1 2.7 1.6l2.4-1 1.9 3.3-2 1.6c.1.5.1 1 .1 1.5Z',
}

const shapePath = computed(() => {
  if (props.glyph.kind === 'shape') return SHAPES[props.glyph.shape] ?? SHAPES.cog
  if (props.glyph.kind === 'stat' && !statSrc.value && !arkhamClass.value) return SHAPES.shroud
  return null
})
</script>

<template>
  <img v-if="tokenSrc" class="modifier-glyph modifier-glyph--art" :src="tokenSrc" alt="" />
  <img v-else-if="statSrc" class="modifier-glyph modifier-glyph--art" :src="statSrc" alt="" />
  <i
    v-else-if="arkhamClass"
    class="modifier-glyph modifier-glyph--font"
    :class="arkhamClass"
    :style="{ color: skillColor }"
    aria-hidden="true"
  ></i>
  <svg
    v-else-if="shapePath"
    class="modifier-glyph modifier-glyph--shape"
    viewBox="0 0 24 24"
    aria-hidden="true"
  >
    <path :d="shapePath" fill="currentColor" />
  </svg>
</template>

<style scoped>
.modifier-glyph {
  flex: 0 0 auto;
  width: 1em;
  height: 1em;
  display: block;
}

.modifier-glyph--art {
  object-fit: contain;
  filter: drop-shadow(0 1px 1px rgba(0, 0, 0, 0.8));
}

.modifier-glyph--font {
  width: 1em;
  font-size: 1em;
  line-height: 1;
  text-align: center;
  color: var(--title, #fff);
}

.modifier-glyph--shape {
  filter: drop-shadow(0 1px 1px rgba(0, 0, 0, 0.6));
}
</style>
