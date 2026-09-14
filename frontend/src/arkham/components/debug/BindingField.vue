<script lang="ts" setup>
/* The field a value uses when that value is a binding.
 *
 * The same control everywhere one is chosen: a light-blue chip carrying the
 * name, a way back to whatever bound it, and a searchable list of the bindings
 * that fit -- so a binding looks like a binding wherever it appears, rather than
 * like a select in one place and a text box in another.
 */
import { computed, ref } from 'vue'
import { vFocus } from '@/arkham/components/debug/vFocus'
import { onClickOutside } from '@vueuse/core'
import BindingToggle from '@/arkham/components/debug/BindingToggle.vue'
import { jumpToBinding, type Binding } from '@/arkham/customCardBindings'

const props = defineProps<{
  /** The `$name` currently held, if any. */
  modelValue: string | null
  /** The bindings that could stand here, already filtered by the caller. */
  applicable: Binding[]
  /* Everything in scope, which is not the same list: what the field is already
   * holding may be a binding that does not fit, and a name resolved against the
   * filtered list would come back unknown -- losing its type, its origin and the
   * way back to what bound it, exactly where saying so matters most. */
  inScope?: Binding[]
  /** What this position wants, for the toggle's own description. */
  type: string
}>()
const emit = defineEmits<{ 'update:modelValue': [v: string | null] }>()

const open = ref(false)
const search = ref('')
const root = ref<HTMLElement | null>(null)
onClickOutside(root, () => (open.value = false))

const matching = computed(() => {
  const needle = search.value.trim().toLowerCase().replace(/^\$/, '')
  if (!needle) return props.applicable
  return props.applicable.filter(
    (b) =>
      b.name.toLowerCase().includes(needle) ||
      (b.detail ?? '').toLowerCase().includes(needle) ||
      b.origin.toLowerCase().includes(needle),
  )
})

const boundTo = computed(() =>
  (props.inScope ?? props.applicable).find((b) => `$${b.name}` === props.modelValue),
)

function choose(name: string) {
  const trimmed = name.trim()
  emit('update:modelValue', trimmed ? (trimmed.startsWith('$') ? trimmed : `$${trimmed}`) : null)
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
        :placeholder="`Search the ${applicable.length} bindings that fit ${type}`"
        v-focus
        @keydown.enter.prevent="choose(search)"
        @keydown.esc="open = false"
        @keydown.stop
      />
      <ul class="binding-menu">
        <li v-for="bound in matching" :key="bound.name">
          <button type="button" class="binding-option" @click="choose(bound.name)">
            <code class="option-name">${{ bound.name }}</code>
            <span v-if="bound.detail" class="option-detail">{{ bound.detail }}</span>
            <span class="option-origin">{{ bound.origin }}</span>
          </button>
          <button
            v-if="bound.anchor"
            type="button"
            class="option-jump"
            title="Show where this was bound"
            @click.stop="jumpToBinding(bound.anchor)"
          >
            ↗
          </button>
        </li>
        <li v-if="!matching.length" class="muted">
          Nothing in scope matches — press enter to use what you typed anyway.
        </li>
      </ul>
    </div>

    <div v-else class="field-body">
      <div class="picked-row">
        <div class="binding" :class="{ unknown: !!modelValue && !boundTo }">
          <button
            v-if="boundTo?.anchor"
            type="button"
            class="jump-segment"
            :title="`Bound by ${boundTo.origin} — click to show`"
            @click="jumpToBinding(boundTo.anchor)"
          >
            ↗
          </button>
          <button
            type="button"
            class="binding-name"
            :title="
              boundTo
                ? `${boundTo.detail ?? ''} · ${boundTo.origin} — click to choose another`
                : 'Choose a binding'
            "
            @click="open = true"
          >
            <span class="binding-ident">{{ modelValue ?? 'Choose a binding…' }}</span>
            <span v-if="boundTo?.type" class="binding-type">:: {{ boundTo.type }}</span>
          </button>
          <BindingToggle
            chip
            :open="open"
            :count="applicable.length"
            :type="type"
            @toggle="open = !open"
          />
        </div>
      </div>
      <span v-if="modelValue && !boundTo" class="from unknown" title="Nothing in scope binds this name">
        not bound
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

.binding-open {
  position: relative;

  input {
    background: #0b1220;
    border: 1px solid #14b8a6;
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
      background: rgba(20, 184, 166, 0.1);
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
  color: #5eead4;
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
    color: #5eead4;
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
  background: rgba(170, 221, 255, 0.12);
  border: 1px solid #adf;
  border-radius: 4px;
  color: #adf;
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
  align-items: baseline;
  background: none;
  border: none;
  color: inherit;
  cursor: pointer;
  display: flex;
  flex: 1;
  font-family: inherit;
  font-size: inherit;
  gap: 0.35rem;
  min-width: 0;
  padding: 0.35rem 0.5rem;
  text-align: left;
}

// The name is what gets cut when there is no room; the type is short, and is the
// half that says whether this binding belongs where it is standing.
.binding-ident {
  overflow: hidden;
  text-overflow: ellipsis;
  white-space: nowrap;
}

.binding-type {
  flex: none;
  opacity: 0.65;
}

.jump-segment {
  background: rgba(170, 221, 255, 0.22);
  border: none;
  border-right: 1px solid #adf;
  color: #dceeff;
  cursor: pointer;
  flex: none;
  font-family: inherit;
  padding: 0 0.5rem;

  &:hover {
    background: rgba(170, 221, 255, 0.4);
    color: #fff;
  }
}
</style>
