<script lang="ts" setup>
/* Taking the log away with you.
 *
 * One menu entry opens this; the format and the scope are chosen here rather
 * than being three menu entries, because they multiply: three formats across a
 * campaign's scenarios is not a menu.
 *
 * The transcript is rendered on this side (see logTranscript.ts -- the
 * sentences are i18n keys and the locales are here); the other two formats are
 * files the server hands over whole.
 */
import { computed, onMounted, ref } from 'vue'
import { useI18n } from 'vue-i18n'
import * as Api from '@/arkham/api'
import { logRowsToEntries, type LegacyLocationLookup } from '@/arkham/legacyLogParse'
import { logRefName } from '@/arkham/logRefName'
import { sampleLogEntries } from '@/arkham/logExportSample'
import { logTranscript, logTranscriptLines, logTraceLines } from '@/arkham/logTranscript'
import { useDbCardStore } from '@/stores/dbCards'

const props = withDefaults(
  defineProps<{
    gameId: string
    /* Only for the sample of the JSON envelope, which carries it. */
    gameName?: string
    /* This client's seat: an entry addressed to one player belongs in their
       transcript and in nobody else's, as in the panel. */
    playerId?: string | null
    /* What the legacy parser needs to tell a revealed location from an
       unrevealed one. Only pre-overhaul rows consult it. */
    legacyLocations?: LegacyLocationLookup
  }>(),
  { gameName: '', playerId: null, legacyLocations: () => ({}) },
)

const emit = defineEmits<{ close: [] }>()

const { t } = useI18n()
const dbCards = useDbCardStore()

type Format = 'transcript' | 'json' | 'trace'

const dialogRef = ref<HTMLDialogElement>()
const format = ref<Format>('transcript')
/* 0 is the whole log; anything else is a section's `index`. */
const scope = ref(0)
const sections = ref<Api.LogExportSection[]>([])
const exporting = ref(false)
const failed = ref(false)

onMounted(async () => {
  dialogRef.value?.showModal()
  try {
    sections.value = await Api.fetchLogSections(props.gameId)
  } catch (e) {
    /* A game whose log predates the structured overhaul has no scenario
       banners to find, and so no scope to pick: the whole log is the only
       thing on offer. A failed lookup lands in the same place. */
    console.log(e)
    sections.value = []
  }
})

/* Only worth asking when there is more than one scenario in there. */
const canPickScope = computed(() => sections.value.length > 1)

const selected = computed(() => sections.value.find((s) => s.index === scope.value))

/* A scenario's own card name, localized, the way the log draws any other ref --
   the server sends the code for exactly this. Falls back to the name it sent,
   then to saying nothing more than "a scenario". */
function sectionName(section: Api.LogExportSection): string {
  const resolved = logRefName(
    {
      kind: 'RefScenario',
      name: section.name ?? '',
      cardCode: section.cardCode,
      cardId: null,
      entityId: null,
      faceDown: false,
    },
    dbCards,
  )
  return resolved || t('gameBar.exportLogScenario')
}

/* A few lines of what the chosen format produces, rendered by the same code the
   export uses -- so the sample is localized and cannot drift from the file. */
const PREVIEW_LINES = 12

const preview = computed(() => {
  const entries = sampleLogEntries()
  const lines =
    format.value === 'transcript'
      ? logTranscriptLines(entries, { t, dbCards, playerId: null })
      : format.value === 'trace'
        ? logTraceLines(entries)
        : jsonPreviewLines(entries)
  return lines.length > PREVIEW_LINES ? [...lines.slice(0, PREVIEW_LINES), '…'] : lines
})

/* The envelope, with one entry in it: enough to show the shape a reader would
   have to work with, which is the only thing this format is for. */
function jsonPreviewLines(entries: ReturnType<typeof sampleLogEntries>): string[] {
  const row = { tag: 'LogRowStructured', contents: entries[3] }
  return JSON.stringify(
    {
      gameName: props.gameName,
      scenario: selected.value ? sectionName(selected.value) : null,
      fromStep: selected.value?.fromStep ?? null,
      untilStep: selected.value?.untilStep ?? null,
      rows: [row],
    },
    null,
    2,
  ).split('\n')
}

function close() {
  dialogRef.value?.close()
  emit('close')
}

function slug(text: string): string {
  return (
    text
      .toLowerCase()
      .replace(/[^a-z0-9]+/g, '-')
      .replace(/^-|-$/g, '') || 'log'
  )
}

function save(blob: Blob, filename: string) {
  const url = window.URL.createObjectURL(blob)
  const a = document.createElement('a')
  a.style.display = 'none'
  a.href = url
  a.download = filename
  document.body.appendChild(a)
  a.click()
  document.body.removeChild(a)
  window.URL.revokeObjectURL(url)
}

async function doExport() {
  const section = selected.value
  const range: Api.LogExportRange = section
    ? {
        fromStep: section.fromStep,
        scenario: sectionName(section),
        ...(section.untilStep !== null ? { untilStep: section.untilStep } : {}),
      }
    : {}

  exporting.value = true
  failed.value = false
  try {
    if (format.value === 'transcript') {
      const { gameName, scenario, rows } = await Api.fetchLogExport(props.gameId, range)
      const entries = logRowsToEntries(rows, props.legacyLocations)
      const heading = [gameName, scenario].filter((l) => l).join(' — ')
      const body = logTranscript(entries, {
        t,
        dbCards,
        playerId: props.playerId,
      })
      const text = `${heading}\n${'='.repeat(heading.length)}\n\n${body}`
      save(new Blob([text], { type: 'text/plain;charset=utf-8' }), filenameFor('txt'))
    } else {
      const blob = await Api.fetchLogExportFile(props.gameId, range, format.value)
      save(blob, filenameFor(format.value === 'json' ? 'json' : 'txt'))
    }
    close()
  } catch (e) {
    console.log(e)
    failed.value = true
  } finally {
    exporting.value = false
  }
}

function filenameFor(extension: string): string {
  const section = selected.value
  const parts = ['arkham-log', section ? slug(sectionName(section)) : null].filter((p) => p)
  return `${parts.join('-')}.${extension}`
}
</script>

<template>
  <dialog ref="dialogRef" class="export-log" @cancel.prevent="close">
    <button class="close-btn" @click.prevent="close" :aria-label="$t('close')">
      <font-awesome-icon icon="times" />
    </button>
    <h2 class="title">{{ $t('gameBar.exportLog') }}</h2>

    <fieldset>
      <legend>{{ $t('gameBar.exportLogFormatLabel') }}</legend>
      <label
        v-for="option in (['transcript', 'json', 'trace'] as const)"
        :key="option"
        :class="{ chosen: format === option }"
      >
        <input type="radio" :value="option" v-model="format" />
        <span class="option">
          <span class="option-name">{{ $t(`gameBar.exportLogFormat.${option}`) }}</span>
          <span class="option-hint">{{ $t(`gameBar.exportLogFormatHint.${option}`) }}</span>
        </span>
      </label>
    </fieldset>

    <figure class="sample">
      <figcaption>{{ $t('gameBar.exportLogSample') }}</figcaption>
      <pre>{{ preview.join('\n') }}</pre>
    </figure>

    <fieldset v-if="canPickScope">
      <legend>{{ $t('gameBar.exportLogScope') }}</legend>
      <select v-model="scope">
        <option :value="0">{{ $t('gameBar.exportLogWholeCampaign') }}</option>
        <option v-for="section in sections" :key="section.index" :value="section.index">
          {{ section.index }}. {{ sectionName(section) }}
        </option>
      </select>
    </fieldset>

    <p v-if="failed" class="failed">{{ $t('gameBar.exportLogFailed') }}</p>

    <div class="actions">
      <button @click.prevent="close">{{ $t('cancel') }}</button>
      <button class="confirm" :disabled="exporting" @click.prevent="doExport">
        {{ exporting ? $t('gameBar.exportLogWorking') : $t('gameBar.exportLogConfirm') }}
      </button>
    </div>
  </dialog>
</template>

<style scoped>
/* Tokens, not a palette of its own: --box-background / --box-border / --title
   are what every other panel in the app is built from (see .box in
   styles/components.css), and base.css already styles a <button>. Everything
   here is chrome, so nothing reaches for --select, which means "the game is
   waiting on a choice from you". */
/* Selected by class, not by element.
 *
 * Game.vue styles `dialog` for its own dialogs -- position: absolute, top: 50%,
 * a translateY(-50%) that pairs with it, width 400px, font-size 1.2em -- and
 * Vue stamps the PARENT's scope id onto a child component's root element, so
 * that rule lands on this dialog too at the same specificity as a bare `dialog`
 * here, and wins on order. The class is what clears it; without it this hangs
 * half a screen low, because Game.vue's `top: 50%` applies while its paired
 * transform is overridden below. */
dialog.export-log {
  position: fixed;
  /* With `position: fixed` and no inset, `margin: auto` has nothing to centre
     against and the dialog lands at its static position -- which, teleported to
     the end of <body>, is off the bottom of the window. */
  inset: 0;
  margin: auto;
  padding: 24px;
  width: 90%;
  max-width: 460px;
  /* The sample makes this the tallest thing the game bar opens; without a cap
     the actions fall off the bottom of a short window. */
  max-height: min(86dvh, 760px);
  overflow-y: auto;
  background: var(--box-background);
  border: 1px solid var(--box-border);
  border-radius: 5px;
  box-shadow: 0 20px 60px rgba(0, 0, 0, 0.6);
  color: var(--text);
  /* Explicit, for the same reason: Game.vue's dialog rule sets 1.2em, which
     every label in here would otherwise inherit. */
  font-size: 0.95rem;
  opacity: 0;
  transform: scale(0.94) translateY(-10px);
  transition: opacity 0.2s ease, transform 0.2s ease,
              display 0.2s allow-discrete,
              overlay 0.2s allow-discrete;

  &[open] {
    opacity: 1;
    transform: scale(1) translateY(0);
  }

  @starting-style {
    &[open] {
      opacity: 0;
      transform: scale(0.94) translateY(-10px);
    }
  }
}

/* Matches Prompt.vue, the app's other dialog. */
dialog.export-log::backdrop {
  background: rgba(0, 0, 0, 0);
  backdrop-filter: blur(0px);
  transition: background 0.2s ease, backdrop-filter 0.2s ease,
              display 0.2s allow-discrete,
              overlay 0.2s allow-discrete;
}

dialog.export-log[open]::backdrop {
  background: rgba(0, 0, 0, 0.65);
  backdrop-filter: blur(4px);
}

@starting-style {
  dialog.export-log[open]::backdrop {
    background: rgba(0, 0, 0, 0);
    backdrop-filter: blur(0px);
  }
}

/* h2.title is the app's panel heading (components.css): Teutonic, uppercase.
   Only the size is local -- 2em is built for a full-page panel. */
h2.title {
  font-size: 1.4em;
  margin: 0 0 18px;
  padding-right: 24px;
}

fieldset {
  border: none;
  margin: 0 0 16px;
  padding: 0;
}

legend {
  padding: 0 0 6px;
  font-size: 0.78rem;
  text-transform: uppercase;
  letter-spacing: 0.06em;
  color: var(--title);
  opacity: 0.75;
}

label {
  display: flex;
  align-items: flex-start;
  gap: 10px;
  padding: 7px 9px;
  border: 1px solid transparent;
  border-radius: 4px;
  cursor: pointer;

  &:hover { background: rgba(255, 255, 255, 0.06); }
}

label.chosen {
  background: rgba(255, 255, 255, 0.08);
  border-color: var(--box-border);
}

input[type="radio"] {
  margin-top: 3px;
  accent-color: var(--background-mid);
}

.option {
  display: flex;
  flex-direction: column;
  gap: 2px;
}

.option-name {
  line-height: 1.3;
}

.option-hint {
  font-size: 0.78rem;
  color: var(--title);
  opacity: 0.7;
  line-height: 1.35;
}

/* The sample. Monospace and not wrapped, because the shape of the output --
   indentation under a block, keys and braces -- is most of what it is showing. */
.sample {
  margin: 0 0 16px;
}

.sample figcaption {
  font-size: 0.78rem;
  text-transform: uppercase;
  letter-spacing: 0.06em;
  color: var(--title);
  opacity: 0.75;
  margin-bottom: 6px;
}

.sample pre {
  margin: 0;
  padding: 10px 12px;
  /* Every format gets the same box, so switching between them does not resize
     the dialog and shift it under the pointer. PREVIEW_LINES plus the line the
     ellipsis takes, at the line-height below. */
  height: calc(13 * 1.5em);
  box-sizing: content-box;
  overflow: auto;
  background: var(--background-dark);
  border: 1px solid var(--box-border);
  border-radius: 4px;
  color: var(--title);
  /* The same stack the debug tables and the admin views use. A sample is
     indentation and punctuation; the body face is a display serif. */
  font-family: ui-monospace, SFMono-Regular, Menlo, monospace;
  font-size: 0.72rem;
  line-height: 1.5;
  white-space: pre;
  tab-size: 2;
}

select {
  -webkit-appearance: none;
  appearance: none;
  width: 100%;
  padding: 0.35rem 1.6rem 0.35rem 0.55rem;
  background: var(--background-dark) var(--select-caret) no-repeat right 0.6rem center;
  background-size: var(--select-caret-size);
  border: 1px solid var(--box-border);
  border-radius: 4px;
  color: inherit;
  font-size: 0.85rem;
}

.failed {
  margin: 0 0 14px;
  font-size: 0.85rem;
  color: var(--survivor);
}

.close-btn {
  position: absolute;
  top: 10px;
  right: 10px;
  /* base.css gives every button 11px of side padding over border-box, which
     squeezes an icon-only button out of shape. */
  padding: 2px 6px;
  background: transparent;
  border: none;
  color: var(--title);
  opacity: 0.6;

  &:hover:not(:disabled) {
    background: transparent;
    opacity: 1;
  }
}

.actions {
  display: flex;
  gap: 8px;
  justify-content: flex-end;
}

/* The confirm reads as the primary action by weight, not by colour: the greens
   and purples of --button-1 / --button-2 are the campaign screens' language. */
.actions .confirm {
  border-color: rgba(255, 255, 255, 0.45);
  font-weight: 600;
}
</style>
