<script lang="ts" setup>
/* Choosing a card where a card code is wanted.
 *
 * A code is not something anyone can recall -- a custom card's is a minted uuid
 * -- so the field is searched by name and shows the name back, with the code it
 * actually holds underneath. Typing a code directly still works, for a card the
 * lists do not have.
 */
import { computed, onMounted, ref } from 'vue'
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

type Choice = { code: string; name: string; kind: string; custom: boolean }

/* Yours first: a custom card is the one whose code cannot be guessed, and the
 * one most likely meant in a card being written right now. */
const choices = computed<Choice[]>(() => {
  const mine = libraryCards().map((c) => ({
    code: c.def.cardCode,
    name: c.def.name.title + (c.def.name.subtitle ? `: ${c.def.name.subtitle}` : ''),
    kind: String(c.def.cardType).replace(/Type$/, ''),
    custom: true,
  }))
  const rest = (cardStore.cards ?? []).map((d: any) => ({
    code: d.cardCode,
    name: d.name?.title ?? String(d.cardCode),
    kind: String(d.cardType ?? '').replace(/Type$/, ''),
    custom: false,
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
        autofocus
        @keydown.enter.prevent="choose(search.trim())"
        @keydown.esc="open = false"
        @keydown.stop
      />
      <ul class="binding-menu">
        <li v-for="card in matching" :key="card.code">
          <button type="button" class="binding-option" @click="choose(card.code)">
            <code class="option-name">{{ card.name }}</code>
            <span class="option-detail">{{ card.kind }}</span>
            <span class="option-origin">{{ card.custom ? 'yours' : card.code }}</span>
          </button>
        </li>
        <li v-if="!matching.length" class="muted">
          Nothing matches — press enter to use what you typed as a code.
        </li>
      </ul>
    </div>

    <div v-else class="field-body">
      <div class="picked-row">
        <div class="binding" :class="{ valid: !!chosen, unknown: !!modelValue && !chosen }">
          <button
            type="button"
            class="binding-name"
            :title="modelValue ? `${modelValue} — click to choose another` : 'Choose a card'"
            @click="open = true"
          >
            {{ chosen ? chosen.name : (modelValue || placeholder || 'Choose a card…') }}
          </button>
        </div>
      </div>
      <span v-if="chosen" class="from">{{ chosen.code }}</span>
      <span v-else-if="modelValue" class="from unknown" title="No card in your library or the pool has this code">
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

.option-name {
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
  &.valid {
    background: rgba(190, 242, 100, 0.12);
    border-color: #bef264;
    color: #bef264;
  }

  &.unknown {
    border-color: #fca5a5;
    color: #fca5a5;
  }
}

.binding-name {
  background: none;
  border: none;
  color: inherit;
  cursor: pointer;
  flex: 1;
  font-family: inherit;
  font-size: inherit;
  min-width: 0;
  overflow: hidden;
  padding: 0.35rem 0.5rem;
  text-align: left;
  text-overflow: ellipsis;
  white-space: nowrap;
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

.clear-value {
  background: rgba(255, 255, 255, 0.06);
  border: 1px solid #4b5563;
  border-radius: 4px;
  color: #eee;
  cursor: pointer;
  flex: 0 0 auto;
  padding: 0 0.5rem;
}
</style>
