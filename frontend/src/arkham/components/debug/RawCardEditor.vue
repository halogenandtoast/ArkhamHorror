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
import { computed, ref, watch } from 'vue'

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

const draft = ref('')
const error = ref<string | null>(null)

watch(
  () => props.def,
  (def) => {
    draft.value = pretty(def)
    error.value = null
  },
  { immediate: true },
)

const dirty = computed(() => draft.value.trim() !== pretty(props.def).trim())

/* What the text works out to, or why it does not. Read on every keystroke so
 * the error appears where it is made rather than when saving is attempted. */
const parsed = computed<Record<string, unknown> | null>(() => {
  try {
    const value = JSON.parse(draft.value)
    return value && typeof value === 'object' && !Array.isArray(value) ? value : null
  } catch {
    return null
  }
})

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

/* Changing the code makes this a different card: the server upserts on it, so
 * the old one stays where it was and a second one appears. Worth saying before
 * it happens rather than after. */
const codeChanged = computed(
  () => !!parsed.value && parsed.value.cardCode !== props.def.cardCode,
)

function save() {
  if (complaint.value || !parsed.value) {
    error.value = complaint.value
    return
  }
  emit('save', parsed.value)
}

const revert = () => {
  draft.value = pretty(props.def)
  error.value = null
}
</script>

<template>
  <div class="raw-backdrop" @click.self="emit('close')">
    <section class="raw-editor" tabindex="-1" @keydown.esc="emit('close')">
      <header>
        <h2>{{ title }}</h2>
        <code class="code">{{ def.cardCode }}</code>
        <span class="spacer" />
        <button type="button" class="link" @click="emit('close')">Close</button>
      </header>

      <textarea
        v-model="draft"
        spellcheck="false"
        :readonly="readonly"
        rows="24"
        @keydown.stop
        @keydown.esc="emit('close')"
      />

      <p v-if="complaint" class="bad">{{ complaint }}</p>
      <p v-else-if="codeChanged" class="warn">
        The card code changed — saving writes a new card and leaves
        <code>{{ def.cardCode }}</code> where it is.
      </p>
      <p v-else-if="error" class="bad">{{ error }}</p>

      <div class="raw-actions">
        <button type="button" class="confirm" :disabled="readonly || busy || !dirty || !!complaint" @click="save">
          {{ busy ? 'Saving…' : 'Save' }}
        </button>
        <button type="button" class="cancel" :disabled="!dirty" @click="revert">Revert</button>
        <span v-if="readonly" class="note">This set follows someone else's, so its cards are read-only.</span>
      </div>
    </section>
  </div>
</template>

<style scoped lang="scss">
.raw-backdrop {
  align-items: center;
  background: rgba(0, 0, 0, 0.6);
  display: flex;
  inset: 0;
  justify-content: center;
  padding: 2rem 1rem;
  position: fixed;
  z-index: 60;
}

.raw-editor {
  background: var(--background-dark);
  border: 1px solid var(--spooky-green);
  border-radius: 8px;
  display: flex;
  flex-direction: column;
  gap: 0.6rem;
  max-height: 100%;
  max-width: 64rem;
  outline: none;
  padding: 0.9rem 1rem;
  width: 100%;

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

  textarea {
    font-family: monospace;
    font-size: 0.8rem;
    min-height: 0;
    resize: vertical;
  }
}

.code {
  font-size: 0.75rem;
  opacity: 0.6;
}

.spacer {
  flex: 1;
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
}
</style>
