<script lang="ts" setup>
/* Choosing a card where a card code is wanted.
 *
 * A code is not something anyone can recall -- a custom card's is a minted uuid
 * -- so the field is searched by name and shows the name back, with the code it
 * actually holds underneath. Typing a code directly still works, for a card the
 * lists do not have.
 */
import { computed, onMounted, ref } from 'vue'
import { vFocus } from '@/arkham/components/debug/vFocus'
import { onClickOutside } from '@vueuse/core'
import { useCardStore } from '@/stores/cards'
import { libraryCards, loadLibrary } from '@/arkham/customCardLibrary'
import { stripCardCodePrefix } from '@/arkham/customCards'

const props = defineProps<{ modelValue: string | null | undefined; placeholder?: string }>()
const emit = defineEmits<{ 'update:modelValue': [v: string] }>()

const cardStore = useCardStore()
const open = ref(false)
const search = ref('')
const root = ref<HTMLElement | null>(null)
onClickOutside(root, () => (open.value = false))

onMounted(() => {
  loadLibrary()
  cardStore.fetchCards()
})

type Choice = {
  code: string
  name: string
  title: string
  subtitle: string
  kind: string
  custom: boolean
  /** Class symbols for a player card; empty for encounter cards. */
  classes: string[]
  /** The encounter set an encounter card belongs to, which is its campaign. */
  set?: string
}

/* Named the way the icon font names them, so a class shows as its symbol.
 * There are no encounter-set icons in the app, so a set says its own name. */
const CLASS_ICONS: Record<string, string> = {
  Guardian: 'guardian',
  Seeker: 'seeker',
  Rogue: 'rogue',
  Mystic: 'mystic',
  Survivor: 'survivor',
  Neutral: 'neutral',
}

/* "Title: Subtitle" is how a card is named -- two investigators can share a
 * title, and the subtitle is the half that tells them apart. Kept as two parts
 * so the subtitle can be shown as the quieter half it is, and joined only where
 * one string is wanted, as in a search. */
const titleOf = (name: any) => name?.title ?? ''
const subtitleOf = (name: any) => name?.subtitle ?? ''
const fullName = (name: any) =>
  subtitleOf(name) ? `${titleOf(name)}: ${subtitleOf(name)}` : titleOf(name)

/* Yours first: a custom card is the one whose code cannot be guessed, and the
 * one most likely meant in a card being written right now. */
const choices = computed<Choice[]>(() => {
  const mine = libraryCards().map((c) => ({
    code: c.def.cardCode,
    name: fullName(c.def.name),
    title: titleOf(c.def.name),
    subtitle: subtitleOf(c.def.name),
    kind: String(c.def.cardType).replace(/Type$/, ''),
    custom: true,
    classes: c.def.classSymbols ?? [],
    set: c.def.meta?.set,
  }))
  const rest = (cardStore.cards ?? []).map((d: any) => ({
    code: d.cardCode,
    name: fullName(d.name) || String(d.cardCode),
    title: titleOf(d.name) || String(d.cardCode),
    subtitle: subtitleOf(d.name),
    kind: String(d.cardType ?? '').replace(/Type$/, ''),
    custom: false,
    classes: d.classSymbols ?? [],
    set: d.encounterSet ?? undefined,
  }))
  return [...mine, ...rest]
})

const matching = computed(() => {
  const needle = search.value.trim().toLowerCase()
  if (!needle) return choices.value.slice(0, 40)
  return choices.value
    .filter((c) => c.name.toLowerCase().includes(needle) || c.code.toLowerCase().includes(needle))
    .slice(0, 40)
})

/* A code is written with the `c` the engine prepends in some places and without
 * it in others, so both spellings have to find the same card -- otherwise a code
 * that is perfectly correct reports itself as unknown. */
const sameCode = (a?: string | null, b?: string | null) =>
  !!a && !!b && stripCardCodePrefix(a) === stripCardCodePrefix(b)

const chosen = computed(() => choices.value.find((c) => sameCode(c.code, props.modelValue)))

function choose(code: string) {
  emit('update:modelValue', code)
  open.value = false
  search.value = ''
}
</script>

<template>
  <div ref="root" class="field-row">
    <div v-if="open" class="field-body binding-open">
      <input
        v-model="search"
        type="search"
        placeholder="Search your cards and the card pool by name or code"
        v-focus
        @keydown.enter.prevent="choose(search.trim())"
        @keydown.esc="open = false"
        @keydown.stop
      />
      <ul class="binding-menu">
        <li v-for="card in matching" :key="card.code">
          <button type="button" class="binding-option" @click="choose(card.code)">
            <span class="marks" aria-hidden="true">
              <i v-for="c in card.classes" :key="c" :class="`${CLASS_ICONS[c] ?? 'neutral'}-icon`" />
            </span>
            <code class="option-name"
              ><span class="code-note">{{ card.code }}</span>{{ card.title
              }}<span v-if="card.subtitle" class="option-subtitle">{{ card.subtitle }}</span></code
            >
            <span class="option-detail">{{ card.set ?? card.kind }}</span>
            <span class="option-origin">{{ card.custom ? 'yours' : '' }}</span>
          </button>
        </li>
        <li v-if="!matching.length" class="muted">
          Nothing matches — press enter to use what you typed as a code.
        </li>
      </ul>
    </div>

    <div v-else class="field-body">
      <div class="picked-row">
        <div class="binding" :class="{ known: !!chosen, unknown: !!modelValue && !chosen }">
          <span v-if="chosen" class="code-segment" :title="chosen.code">{{ chosen.code }}</span>
          <button
            type="button"
            class="binding-name"
            :title="modelValue ? `${modelValue} — click to choose another` : 'Choose a card'"
            @click="open = true"
          >
            <template v-if="chosen"
              >{{ chosen.title
              }}<span v-if="chosen.subtitle" class="chip-subtitle">{{ chosen.subtitle }}</span>
            </template>
            <template v-else>{{ modelValue || placeholder || 'Choose a card…' }}</template>
          </button>
        </div>
      </div>
      <span v-if="modelValue && !chosen" class="from unknown" title="No card in your library or the pool has this code">
        not a card we know
      </span>
    </div>
  </div>
</template>

<style scoped lang="scss">
.field-row {
  align-items: flex-start;
  display: flex;
  gap: 0.25rem;
  min-width: 0;
  position: relative;
}

.field-body {
  flex: 1;
  min-width: 0;
}

.field-body > .picker > .picked-row {
  align-items: stretch;
  background: #111827;
  border: 1px solid #4b5563;
  border-radius: 4px;
  overflow: hidden;
  padding: 0.35rem 0.5rem;

  > input,
  > .picked,
  > .raw {
    background: transparent;
    border-color: transparent;
    padding: 0;
  }

  > input:focus,
  > .raw:focus {
    outline: none;
  }
}

.field-body:focus-within > .picker > .picked-row {
  border-color: #6b7280;
}

.with-toggle:not(:has(> .picked-row)):not(:has(> .picker > .picked-row)) {
  padding-right: 2rem;
}

.binding-open {
  position: relative;

  input {
    background: #0b1220;
    border: 1px solid #84cc16;
    border-radius: 4px;
    color: #eee;
    padding: 0.35rem 0.5rem;
    width: 100%;

    &::placeholder {
      color: #6b7280;
    }
  }
}

.binding-menu {
  background: #0b1220;
  border: 1px solid #374151;
  border-radius: 5px;
  box-shadow: 0 8px 20px rgba(0, 0, 0, 0.45);
  left: 0;
  list-style: none;
  margin: 0.3rem 0 0;
  max-height: 15rem;
  overflow-y: auto;
  padding: 0.2rem;
  position: absolute;
  right: 0;
  top: 100%;
  z-index: 30;

  li {
    align-items: stretch;
    border-radius: 4px;
    display: flex;
    gap: 0.15rem;

    &:hover {
      background: rgba(190, 242, 100, 0.12);
    }
  }

  .muted {
    color: #9ca3af;
    font-size: 0.75rem;
    padding: 0.45rem 0.5rem;
  }
}

.binding-option {
  align-items: baseline;
  background: none;
  border: none;
  color: inherit;
  cursor: pointer;
  display: flex;
  flex: 1;
  gap: 0.5rem;
  min-width: 0;
  padding: 0.35rem 0.45rem;
  text-align: left;
}

/* Where a campaign icon would go if the app had any: the class symbols a player
 * card carries, which is the nearest at-a-glance mark of where a card is from. */
.marks {
  display: inline-flex;
  flex: none;
  gap: 0.15rem;
  min-width: 1rem;

  i {
    font-size: 0.8rem;
    font-style: normal;
  }
}

/* The quieter half of a name. Set apart rather than run together with a colon,
 * so the title is what the eye lands on when scanning a list of them. */
/* Ahead of the name rather than under it: a minted uuid on its own line pushes
 * the field about, and truncating it keeps the row one line however long it is.
 * It gives up room before the title does, since the title is what is read. */
.code-note {
  flex: 0 1 auto;
  font-size: 0.68em;
  margin-right: 0.5em;
  min-width: 0;
  opacity: 0.45;
  overflow: hidden;
  text-overflow: ellipsis;
  white-space: nowrap;
}

.option-subtitle,
.chip-subtitle {
  font-size: 0.72em;
  margin-left: 0.4em;
  opacity: 0.6;
}

.option-name {
  align-items: baseline;
  display: flex;
  min-width: 0;
  overflow: hidden;
  color: #bef264;
  flex: none;
  font-family: monospace;
  font-size: 0.82rem;
}

.option-detail {
  color: #d1d5db;
  flex: 1;
  font-size: 0.75rem;
  min-width: 0;
  overflow: hidden;
  text-overflow: ellipsis;
  white-space: nowrap;
}

.option-origin {
  color: #6b7280;
  flex: none;
  font-size: 0.72rem;
}

.option-jump {
  background: none;
  border: none;
  color: #6b7280;
  cursor: pointer;
  flex: none;
  padding: 0 0.4rem;

  &:hover {
    color: #bef264;
  }
}

.from {
  color: #9ca3af;
  font-size: 0.72rem;

  &.unknown {
    color: #fca5a5;
  }
}

.binding.unknown {
  color: #fca5a5;
}

.picked-row {
  align-items: stretch;
  display: flex;
  gap: 0.25rem;
}

.binding {
  align-items: stretch;
  background: rgba(148, 163, 184, 0.1);
  border: 1px solid #4b5563;
  border-radius: 4px;
  color: #9ca3af;
  display: flex;
  flex: 1 1 auto;
  font-family: monospace;
  min-width: 0;
  overflow: hidden;

  // The code names a card that is actually there, in your library or the pool.
  &.known {
    background: rgba(190, 242, 100, 0.12);
    border-color: #bef264;
    color: #bef264;
  }

  &.unknown {
    border-color: #fca5a5;
    color: #fca5a5;
  }
}

/* A row of parts rather than one run of text, so the title takes the room and
 * the code gives it up. */
.binding-name {
  align-items: baseline;
  background: none;
  border: none;
  color: inherit;
  cursor: pointer;
  display: flex;
  flex: 1;
  font-family: inherit;
  font-size: inherit;
  min-width: 0;
  overflow: hidden;
  padding: 0.35rem 0.5rem;
  text-align: left;
  white-space: nowrap;
}

/* A segment of the field rather than words inside it: the code is a different
 * kind of thing from the name, and dividing them says so without a label. It
 * takes only the width it needs and gives it up before the name does. */
.code-segment {
  align-items: center;
  align-self: stretch;
  background: rgba(190, 242, 100, 0.16);
  border-right: 1px solid currentColor;
  display: flex;
  flex: 0 1 auto;
  font-size: 0.72em;
  min-width: 0;
  opacity: 0.75;
  overflow: hidden;
  padding: 0 0.45rem;
  text-overflow: ellipsis;
  white-space: nowrap;
}

.binding.unknown .code-segment {
  background: rgba(252, 165, 165, 0.16);
}

.jump-segment {
  background: rgba(190, 242, 100, 0.22);
  border: none;
  border-right: 1px solid #bef264;
  color: #f7ffe0;
  cursor: pointer;
  flex: none;
  font-family: inherit;
  padding: 0 0.5rem;

  &:hover {
    background: rgba(190, 242, 100, 0.4);
    color: #fff;
  }
}

/* The row aligns to the top so a field with a note under it does not drag its
 * neighbours down; the button still has to match the field it clears, which is
 * what it opts back into here. */
.clear-value {
  align-items: center;
  align-self: flex-start;
  background: rgba(255, 255, 255, 0.06);
  border: 1px solid #4b5563;
  border-radius: 4px;
  color: #eee;
  cursor: pointer;
  display: flex;
  flex: 0 0 auto;
  height: 1.9rem;
  padding: 0 0.5rem;
}
</style>
