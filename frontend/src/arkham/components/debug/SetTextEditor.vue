<script lang="ts" setup>
/* Every word a set's cards say, as one document.
 *
 * The text itself lives on each card -- that is the only form that reaches a
 * game whose players do not own the set -- but writing it a card at a time is
 * no way to translate a box. So the set's cards are gathered into one object
 * keyed by card, edited as JSON, and saved back to the cards that changed.
 *
 * The keys under each card come from the card: every `$.name` its labels write
 * is a line here, blank until somebody says what it should say.
 */
import { computed, ref, watch } from 'vue'
import {
  LOCALE_META_KEY,
  PRIMARY_LANGUAGE,
  TEXT_LANGUAGES,
  textKeysUsed,
  cardText,
} from '@/arkham/customText'
import { saveToLibrary, setCards, type LibraryCard } from '@/arkham/customCardLibrary'

const props = defineProps<{ setId: string; disabled?: boolean }>()

const language = ref(PRIMARY_LANGUAGE)
const draft = ref('')
const error = ref<string | null>(null)
const saving = ref(false)
const saved = ref(0)

/* A card is named the way the library names it, since that is what somebody
 * writing text is reading off the screen. The level is part of the name: two
 * printings are two cards with two sets of words. */
const nameOf = (card: LibraryCard) => {
  const title = card.def.name?.title ?? card.def.cardCode
  return card.def.level === null || card.def.level === undefined ? title : `${title} (${card.def.level})`
}

const cards = computed(() => setCards(props.setId))

/* What the document holds: a line per key the card names, filled in with what
 * it already says. A card that names none is left out -- an empty object per
 * card would be most of the file. */
const document = computed(() => {
  const out: Record<string, Record<string, string>> = {}
  for (const card of cards.value) {
    const said = cardText(card.def, language.value)
    const keys = [...textKeysUsed(card.def), ...Object.keys(said).filter((k) => !textKeysUsed(card.def).includes(k))]
    if (!keys.length) continue
    out[nameOf(card)] = Object.fromEntries(keys.map((key) => [key, said[key] ?? '']))
  }
  return out
})

const asText = (value: unknown) => JSON.stringify(value, null, 2)

// Which languages this set already says anything in, marked in the picker.
const written = (lang: string) =>
  cards.value.some((card) =>
    Object.values(cardText(card.def, lang)).some((value) => typeof value === 'string' && value.trim()),
  )

watch(
  [document, language],
  () => {
    draft.value = asText(document.value)
    error.value = null
    saved.value = 0
  },
  { immediate: true },
)

const dirty = computed(() => draft.value.trim() !== asText(document.value).trim())

async function save() {
  let parsed: Record<string, Record<string, string>>
  try {
    parsed = JSON.parse(draft.value)
  } catch (e) {
    error.value = (e as Error).message
    return
  }

  const byName = new Map(cards.value.map((card) => [nameOf(card), card]))
  const unknown = Object.keys(parsed).filter((name) => !byName.has(name))
  if (unknown.length) {
    error.value = `No card in this set is called ${unknown[0]}`
    return
  }

  saving.value = true
  error.value = null
  let written = 0
  try {
    for (const [name, entries] of Object.entries(parsed)) {
      const card = byName.get(name)!
      const written_ = Object.fromEntries(
        Object.entries(entries ?? {}).filter(([, value]) => typeof value === 'string' && value.trim()),
      )
      const before = cardText(card.def, language.value)
      if (asText(before) === asText(written_)) continue
      // eslint-disable-next-line @typescript-eslint/no-explicit-any
      const meta = { ...((card.def as any).meta ?? {}) }
      const locale = { ...(meta[LOCALE_META_KEY] ?? {}) }
      if (Object.keys(written_).length) locale[language.value] = written_
      else delete locale[language.value]
      if (Object.keys(locale).length) meta[LOCALE_META_KEY] = locale
      else delete meta[LOCALE_META_KEY]
      await saveToLibrary({ def: { ...card.def, meta }, art: card.art }, props.setId)
      written += 1
    }
    saved.value = written
  } catch (e) {
    error.value = (e as Error).message
  } finally {
    saving.value = false
  }
}
</script>

<template>
  <fieldset class="set-text">
    <legend>Text</legend>
    <div class="row">
      <label>
        Language
        <select v-model="language">
          <option v-for="[code, name] in TEXT_LANGUAGES" :key="code" :value="code">
            {{ name }}{{ written(code) ? ' •' : '' }}
          </option>
        </select>
      </label>
      <button type="button" :disabled="disabled || saving || !dirty" @click="save">
        {{ saving ? 'Saving…' : 'Save text' }}
      </button>
      <span v-if="error" class="bad">{{ error }}</span>
      <span v-else-if="saved" class="good">{{ saved }} card{{ saved === 1 ? '' : 's' }} saved</span>
    </div>
    <textarea v-model="draft" rows="18" spellcheck="false" :disabled="disabled" @keydown.stop />
  </fieldset>
</template>

<style lang="scss" scoped>
.set-text {
  border: 1px solid #f0abfc44;
  border-radius: 5px;
  display: flex;
  flex-direction: column;
  gap: 0.5rem;
  padding: 0.5rem 0.75rem;

  legend {
    color: #f0abfc;
    font-size: 0.72rem;
    padding: 0 0.3rem;
  }

  textarea {
    color: #eee;
    font-family: monospace;
    font-size: 0.78rem;
  }
}

.row {
  align-items: center;
  display: flex;
  gap: 0.75rem;
}

.bad {
  color: #fca5a5;
  font-size: 0.75rem;
}

.good {
  color: #f0abfc;
  font-size: 0.75rem;
}
</style>
