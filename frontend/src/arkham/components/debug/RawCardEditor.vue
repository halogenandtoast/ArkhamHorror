<script lang="ts" setup>
/* A card as the server holds it, open for editing.
 *
 * The form is the way to write a card and cannot be the only way to read one:
 * a def that came from somewhere else, or that uses something the form has no
 * control for, is still a card in your library, and the thing you need when two
 * copies have drifted apart is to see exactly what each one says.
 *
 * Saving goes down the same road the form saves by, so the server's own decode
 * is still the last word on whether this is a card.
 */
import { computed, nextTick, ref, watch } from 'vue'
import { DERIVED_CARD_KEYS, SET_STAMPED_META_KEYS } from '@/arkham/customCards'

const props = defineProps<{
  /** The def as stored, which is what is shown and what is compared against. */
  def: Record<string, unknown>
  title: string
  /** A set that follows someone else's cannot be written to. */
  readonly?: boolean
  busy?: boolean
}>()

const emit = defineEmits<{ save: [def: Record<string, unknown>]; close: [] }>()

const pretty = (value: unknown) => JSON.stringify(value, null, 2)

/* The def with what the card does not own taken out of it, which is what is
 * shown and what is compared against. Leaving them in would be an invitation to
 * edit something the next save overwrites without a word. */
// eslint-disable-next-line @typescript-eslint/no-explicit-any
const ownedBy = (def: Record<string, any>): Record<string, any> => {
  const own = { ...def }
  for (const key of DERIVED_CARD_KEYS) delete own[key]
  if (own.meta && typeof own.meta === 'object') {
    const meta = { ...own.meta }
    for (const key of SET_STAMPED_META_KEYS) delete meta[key]
    own.meta = meta
  }
  return own
}

/* ...and put back as they were. They win over anything written in their place,
 * since what wrote them is what will write them again. */
// eslint-disable-next-line @typescript-eslint/no-explicit-any
const withHeld = (edited: Record<string, any>, from: Record<string, any>): Record<string, any> => {
  const whole = { ...edited }
  for (const key of DERIVED_CARD_KEYS) if (key in from) whole[key] = from[key]
  const held = SET_STAMPED_META_KEYS.filter((key) => from.meta && key in from.meta)
  if (held.length) {
    whole.meta = { ...(whole.meta ?? {}) }
    for (const key of held) whole.meta[key] = from.meta[key]
  }
  return whole
}

/** What is being held back, by the name it has in the def. */
const heldKeys = (def: Record<string, unknown>) => [
  ...DERIVED_CARD_KEYS.filter((key) => key in def),
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  ...SET_STAMPED_META_KEYS.filter((key) => key in ((def as any).meta ?? {})).map((key) => `meta.${key}`),
]

const draft = ref('')
const error = ref<string | null>(null)
const box = ref<HTMLTextAreaElement | null>(null)
const underlay = ref<HTMLElement | null>(null)

const shown = computed(() => ownedBy(props.def))
const held = computed(() => heldKeys(props.def))

watch(
  shown,
  (def) => {
    draft.value = pretty(def)
    error.value = null
  },
  { immediate: true },
)

const dirty = computed(() => draft.value.trim() !== pretty(shown.value).trim())

const complaint = computed(() => {
  if (!draft.value.trim()) return 'Empty'
  try {
    const value = JSON.parse(draft.value)
    if (!value || typeof value !== 'object' || Array.isArray(value)) return 'A card def is an object'
  } catch (e) {
    return (e as Error).message
  }
  return null
})

const parsed = computed<Record<string, unknown> | null>(() => {
  if (complaint.value) return null
  return JSON.parse(draft.value)
})

/* Changing the code makes this a different card: the server upserts on it, so
 * the old one stays where it was and a second one appears. Worth saying before
 * it happens rather than after. */
const codeChanged = computed(() => !!parsed.value && parsed.value.cardCode !== props.def.cardCode)

const escapeHtml = (text: string) =>
  text.replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;')

/* JSON, coloured.
 *
 * A textarea cannot colour its own contents, so the text is drawn twice: this
 * underneath, and the box itself on top with transparent glyphs and a visible
 * caret. The two are laid out identically, which is the whole trick -- same
 * font, same metrics, same padding, same wrapping -- and the editing stays what
 * a textarea gives for free rather than being rebuilt on a div.
 *
 * Colours come from the palette the builder already uses, so a def reads as
 * part of this page and not as a window from another program.
 */
const highlighted = computed(() => {
  const source = draft.value.endsWith('\n') ? `${draft.value} ` : draft.value
  return escapeHtml(source).replace(
    /("(?:\\.|[^"\\])*"\s*:)|("(?:\\.|[^"\\])*")|(-?\d+(?:\.\d+)?(?:[eE][+-]?\d+)?)|\b(true|false|null)\b|([{}[\],])/g,
    (match, key, string, number, word, punct) => {
      if (key) return `<span class="t-key">${match}</span>`
      if (string) return `<span class="t-string">${match}</span>`
      if (number) return `<span class="t-number">${match}</span>`
      if (word) return `<span class="t-word">${match}</span>`
      if (punct) return `<span class="t-punct">${match}</span>`
      return match
    },
  )
})

// The two layers only line up while they scroll together.
const syncScroll = () => {
  if (!box.value || !underlay.value) return
  underlay.value.scrollTop = box.value.scrollTop
  underlay.value.scrollLeft = box.value.scrollLeft
}

watch(draft, () => nextTick(syncScroll))

function save() {
  if (complaint.value || !parsed.value) {
    error.value = complaint.value
    return
  }
  emit('save', withHeld(parsed.value, props.def))
}

const revert = () => {
  draft.value = pretty(shown.value)
  error.value = null
}
</script>

<template>
  <section class="raw-editor" @keydown.esc="emit('close')">
    <header>
      <h2>{{ title }}</h2>
      <code class="code">{{ def.cardCode }}</code>
      <span class="spacer" />
      <button type="button" class="link" @click="emit('close')">Back to the form</button>
    </header>

    <div class="code-box">
      <pre ref="underlay" class="underlay" aria-hidden="true"><code v-html="highlighted" /></pre>
      <textarea
        ref="box"
        v-model="draft"
        spellcheck="false"
        autocapitalize="off"
        autocomplete="off"
        autocorrect="off"
        :readonly="readonly"
        @scroll="syncScroll"
        @keydown.stop
        @keydown.esc="emit('close')"
      />
    </div>

    <p v-if="complaint" class="bad">{{ complaint }}</p>
    <p v-else-if="codeChanged" class="warn">
      The card code changed — saving writes a new card and leaves
      <code>{{ def.cardCode }}</code> where it is.
    </p>
    <p v-else-if="error" class="bad">{{ error }}</p>

    <div class="raw-actions">
      <button
        type="button"
        class="confirm"
        :disabled="readonly || busy || !dirty || !!complaint"
        @click="save"
      >
        {{ busy ? 'Saving…' : 'Save' }}
      </button>
      <button type="button" class="cancel" :disabled="!dirty" @click="revert">Revert</button>
      <span v-if="held.length" class="note">
        Kept as they are:
        <code v-for="key in held" :key="key">{{ key }}</code>
      </span>
      <span v-if="readonly" class="note">This set follows someone else's, so its cards are read-only.</span>
    </div>
  </section>
</template>

<style scoped lang="scss">
.raw-editor {
  display: flex;
  flex: 1;
  flex-direction: column;
  gap: 0.6rem;
  /* As tall as the window leaves it. A def runs to a few hundred lines and the
     thing you are doing with it is reading down one, so the box takes the room
     rather than the page growing a scrollbar for a box that scrolls. The
     subtraction is the editor's own head and feet plus the page's padding. */
  height: calc(100vh - var(--nav-height) - 11rem);
  min-height: 22rem;

  > header {
    align-items: baseline;
    display: flex;
    gap: 0.6rem;
  }

  h2 {
    font-family: teutonic, sans-serif;
    font-size: 1.25em;
    margin: 0;
  }
}

.code {
  font-size: 0.75rem;
  opacity: 0.6;
}

.spacer {
  flex: 1;
}

/* The two layers of the editor, which have to agree on every metric that moves
   a glyph: one wrong padding and the caret sits a character away from the
   letter it is in front of. */
.code-box {
  background: #0b1220;
  border: 1px solid var(--box-border);
  border-radius: 6px;
  flex: 1;
  min-height: 0;
  overflow: hidden;
  position: relative;

  .underlay,
  textarea {
    border: none;
    box-sizing: border-box;
    font-family: monospace;
    font-size: 0.8rem;
    height: 100%;
    left: 0;
    line-height: 1.5;
    margin: 0;
    overflow: auto;
    padding: 0.6rem 0.75rem;
    position: absolute;
    tab-size: 2;
    top: 0;
    white-space: pre;
    width: 100%;
    word-break: normal;
  }

  .underlay {
    pointer-events: none;
    z-index: 0;
  }

  textarea {
    background: transparent;
    /* Transparent glyphs over the coloured ones, and a caret of its own so
       there is still something to type at. */
    caret-color: #eee;
    color: transparent;
    resize: none;
    z-index: 1;

    &:focus {
      outline: none;
    }

    &::selection {
      background: rgba(94, 234, 212, 0.25);
    }
  }
}

/* The builder's own palette rather than an editor theme's. */
:deep(.t-key) {
  color: #adf;
}

:deep(.t-string) {
  color: #bef264;
}

:deep(.t-number) {
  color: #fcd34d;
}

:deep(.t-word) {
  color: #c4b5fd;
}

:deep(.t-punct) {
  color: #6b7280;
}

.underlay code {
  color: #d1d5db;
  font: inherit;
}

.raw-actions {
  align-items: center;
  display: flex;
  gap: 0.6rem;
}

.bad {
  color: #fca5a5;
  font-size: 0.8rem;
  margin: 0;
}

.warn {
  color: #fcd34d;
  font-size: 0.8rem;
  margin: 0;
}

.note {
  font-size: 0.8rem;
  opacity: 0.7;

  code {
    font-size: 0.75rem;

    & + code::before {
      content: ', ';
    }
  }
}
</style>
