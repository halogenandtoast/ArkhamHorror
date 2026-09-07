<script lang="ts" setup>
/* The field a property is chosen in.
 *
 * The same control as a binding -- a chip that opens a searchable list -- in
 * violet rather than teal, because a property is not a value you have but a way
 * of reading one. Sharing the shape and parting on colour is what makes the two
 * legible side by side in the same expression.
 */
import { computed, ref } from 'vue'
import { onClickOutside } from '@vueuse/core'

const props = defineProps<{
  modelValue: string | null | undefined
  /** Property name to the type it yields. */
  options: Record<string, string>
  /** What kind of thing these read, for the search placeholder. */
  of: string
}>()
const emit = defineEmits<{ 'update:modelValue': [v: string] }>()

const open = ref(false)
const search = ref('')
const root = ref<HTMLElement | null>(null)
onClickOutside(root, () => (open.value = false))

const entries = computed(() => Object.entries(props.options).map(([name, type]) => ({ name, type })))

const matching = computed(() => {
  const needle = search.value.trim().toLowerCase()
  if (!needle) return entries.value
  return entries.value.filter(
    (e) => e.name.toLowerCase().includes(needle) || e.type.toLowerCase().includes(needle),
  )
})

const chosen = computed(() => entries.value.find((e) => e.name === props.modelValue))

function choose(name: string) {
  emit('update:modelValue', name)
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
        :placeholder="`Search the ${entries.length} properties of ${of}`"
        autofocus
        @keydown.esc="open = false"
        @keydown.stop
      />
      <ul class="binding-menu">
        <li v-for="entry in matching" :key="entry.name">
          <button type="button" class="binding-option" @click="choose(entry.name)">
            <code class="option-name">{{ entry.name }}</code>
            <span class="option-detail">{{ entry.type }}</span>
          </button>
        </li>
        <li v-if="!matching.length" class="muted">Nothing matches.</li>
      </ul>
    </div>

    <div v-else class="field-body">
      <div class="picked-row">
        <div class="binding" :class="{ unknown: !!modelValue && !chosen }">
          <button
            type="button"
            class="binding-name"
            :title="chosen ? `${chosen.name} :: ${chosen.type} — click to choose another` : 'Choose a property'"
            @click="open = true"
          >
            {{ chosen ? `${chosen.name} :: ${chosen.type}` : (modelValue || 'Choose a property…') }}
          </button>
        </div>
      </div>
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


.binding-open {
  position: relative;

  input {
    background: #0b1220;
    border: 1px solid #8b5cf6;
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
      background: rgba(139, 92, 246, 0.12);
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
  color: #c4b5fd;
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
  background: rgba(196, 181, 253, 0.14);
  border: 1px solid #c4b5fd;
  border-radius: 4px;
  color: #c4b5fd;
  display: flex;
  flex: 1 1 auto;
  font-family: monospace;
  min-width: 0;
  overflow: hidden;

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
