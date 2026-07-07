<script lang="ts" setup>
import { imgsrc } from '@/arkham/helpers'
import type { LogContents } from '@/arkham/types/Log'
import { computed, onMounted, onUnmounted, ref } from 'vue'
import { useI18n } from 'vue-i18n'
import { useDebug } from '@/arkham/debug'

const props = defineProps<{ log: LogContents; gameId?: string }>()
const emit = defineEmits<{ refresh: [] }>()
const { t } = useI18n()

// Hidden debug: hold Shift and hover to reveal a Debug toggle; while active,
// clicking an undiscovered glyph translates it (mirrors Feast of Hemlock Vale).
const debug = useDebug()
const glyphDebug = ref(false)
const hovering = ref(false)
const shiftHeld = ref(false)
const showDebugToggle = computed(() => glyphDebug.value || (hovering.value && shiftHeld.value))
const canDebug = computed(() => glyphDebug.value && !!props.gameId)

const onKeyDown = (e: KeyboardEvent) => { if (e.key === 'Shift') shiftHeld.value = true }
const onKeyUp = (e: KeyboardEvent) => { if (e.key === 'Shift') shiftHeld.value = false }
onMounted(() => { window.addEventListener('keydown', onKeyDown); window.addEventListener('keyup', onKeyUp) })
onUnmounted(() => { window.removeEventListener('keydown', onKeyDown); window.removeEventListener('keyup', onKeyUp) })

async function discover(letter: string) {
  if (!canDebug.value || !props.gameId || discovered.value.has(letter)) return
  const l = letter.toLowerCase()
  const word = t(`theDrownedCity.glyphs.runes.${l}`)
  // Reuse the translateGlyph campaign handler so the record path is identical to play.
  await debug.send(props.gameId, { tag: 'CampaignSpecific', contents: ['translateGlyph', [`rune_${l}`, word]] })
  emit('refresh')
}

// Each entry's image lives at extra/the-drowned-city/runes/<letter>.png and its
// translated word at theDrownedCity.glyphs.runes.<letter>.
const order = Array.from({ length: 26 }, (_, i) => String.fromCharCode(65 + i))

// A glyph is discovered/translated once its letter shows up in the campaign log's
// "discovered glyphs" recorded set. Values may be a bare letter or wrapped in a
// recordable ({ contents } / { recordVal: { contents } }); take the first char.
const discovered = computed(() => {
  const set = new Set<string>()
  for (const [key, values] of Object.entries(props.log.recordedSets ?? {})) {
    if (!key.toLowerCase().includes('discoveredglyph')) continue
    for (const v of (values as any[]) ?? []) {
      const c = typeof v === 'string' ? v : (v?.contents ?? v?.recordVal?.contents)
      if (typeof c === 'string' && c.length) set.add(c[0].toUpperCase())
    }
  }
  return set
})
</script>

<template>
  <div
    class="log-section"
    :class="{ debugging: canDebug }"
    @mouseenter="hovering = true"
    @mouseleave="hovering = false"
  >
    <h3 class="section-title">
      {{ t('theDrownedCity.glyphs.title') }}
      <button
        v-if="showDebugToggle"
        type="button"
        class="glyph-debug-toggle"
        :class="{ active: glyphDebug }"
        @click="glyphDebug = !glyphDebug"
      >Debug</button>
    </h3>
    <div class="glyph-grid">
      <div
        v-for="letter in order"
        :key="letter"
        class="glyph"
        :class="{ discovered: discovered.has(letter), 'glyph-debug': canDebug && !discovered.has(letter) }"
        @click="discover(letter)"
      >
        <div
          class="rune"
          role="img"
          :aria-label="letter"
          :style="{ '--rune-url': `url('${imgsrc(`extra/the-drowned-city/runes/${letter.toLowerCase()}.png`)}')` }"
        ></div>
        <span class="word">{{ discovered.has(letter) ? t(`theDrownedCity.glyphs.runes.${letter.toLowerCase()}`) : '' }}</span>
      </div>
    </div>
  </div>
</template>

<style scoped>
.log-section {
  background: var(--box-background);
  border: 1px solid rgba(255,255,255,0.07);
  border-radius: 8px;
  padding: 14px 16px;
}

.section-title {
  display: flex;
  align-items: center;
  gap: 10px;
  font-family: teutonic, sans-serif;
  font-size: 1.1em;
  font-weight: normal;
  color: rgba(255,255,255,0.75);
  text-transform: uppercase;
  letter-spacing: 0.08em;
  margin: 0 0 12px;
  padding-bottom: 8px;
  border-bottom: 1px solid rgba(255,255,255,0.07);
}

.glyph-debug-toggle {
  appearance: none;
  border: 1px solid rgba(90, 70, 45, 0.5);
  border-radius: 3px;
  background: rgba(255, 255, 255, 0.35);
  color: rgba(45, 32, 18, 0.9);
  padding: 2px 7px;
  font-size: 0.65em;
  letter-spacing: 0.05em;
  cursor: pointer;
}

.glyph-debug-toggle.active {
  background: #6d1f1f;
  border-color: #9d3030;
  color: white;
}

.log-section.debugging {
  outline: 2px dashed #6d1f1f;
}

.glyph-debug {
  cursor: pointer;
}

.glyph-debug:hover {
  opacity: 0.75;
  border-color: #9d3030;
}

.glyph-grid {
  display: flex;
  flex-wrap: wrap;
  gap: 10px;
}

.glyph {
  display: flex;
  flex-direction: column;
  align-items: center;
  gap: 6px;
  width: 84px;
  padding: 10px 6px 8px;
  border-radius: 6px;
  /* the glyphs are solid black art; use that tone as the tile so the
     white-ish (recolored) glyph reads clearly on top */
  background: #101114;
  border: 1px solid rgba(255,255,255,0.08);
  opacity: 0.4;
  transition: opacity 0.15s, border-color 0.15s;
}

.glyph.discovered {
  opacity: 1;
  border-color: rgba(255,255,255,0.16);
}

/* recolor the black PNG to the discovered-text color via a mask */
.rune {
  width: 48px;
  height: 48px;
  background-color: var(--title);
  -webkit-mask: var(--rune-url) center / contain no-repeat;
  mask: var(--rune-url) center / contain no-repeat;
}

.word {
  min-height: 1.3em;
  font-family: teutonic, sans-serif;
  font-size: 1.1rem;
  letter-spacing: 0.03em;
  text-align: center;
  color: var(--title);
  border-top: 1px solid rgba(255,255,255,0.12);
  padding-top: 5px;
  width: 100%;
}
</style>
