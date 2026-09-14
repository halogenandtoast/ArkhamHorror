<script setup lang="ts" generic="T extends string">
import { computed, useId } from 'vue'
const props = defineProps<{
  modelValue: T
  options: { value: T; label: string; disabled?: boolean; count?: number }[]
  label: string
}>()
const emit = defineEmits<{ 'update:modelValue': [value: T] }>()
const name = useId()
const index = computed(() => Math.max(0, props.options.findIndex(option => option.value === props.modelValue)))

function selectWithPointer(event: MouseEvent, value: T, disabled?: boolean) {
  // Leave keyboard-generated clicks to the native radio/change behavior.
  if (event.detail === 0) return
  event.preventDefault()
  if (disabled) return
  emit('update:modelValue', value)
  const group = (event.currentTarget as HTMLElement).parentElement
  const focused = document.activeElement
  if (focused instanceof HTMLInputElement && group?.contains(focused)) focused.blur()
}
</script>

<template>
  <div class="segmented-toggle" role="radiogroup" :aria-label="label" :style="{ '--items': options.length, '--index': index }">
    <label v-for="option in options" :key="option.value" :class="{ disabled: option.disabled }" @mousedown.prevent @click="selectWithPointer($event, option.value, option.disabled)">
      <input type="radio" :name="name" :value="option.value" :checked="modelValue === option.value" :disabled="option.disabled" @change="emit('update:modelValue', option.value)" />
      <span>{{ option.label }} <small v-if="option.count !== undefined">{{ option.count }}</small></span>
    </label>
  </div>
</template>

<style scoped>
.segmented-toggle { display: grid; grid-template-columns: repeat(var(--items), minmax(0, 1fr)); border-radius: 5px; background: var(--background-dark); border: 1px solid var(--box-border); padding: 2px; gap: 2px; position: relative; }
.segmented-toggle::before { content: ''; background: var(--button-1); border-radius: 3px; position: absolute; top: 2px; bottom: 2px; left: 2px; width: calc((100% - 4px - (var(--items) - 1) * 2px) / var(--items)); transform: translateX(calc(var(--index) * (100% + 2px))); transition: transform 220ms cubic-bezier(.2, .8, .2, 1), background 150ms ease; pointer-events: none; }
label { display: flex; align-items: center; justify-content: center; position: relative; margin: 0; padding: 6px 8px; border-radius: 3px; cursor: pointer; color: var(--background-light); font-size: 11px; font-weight: 600; letter-spacing: .06em; text-transform: uppercase; text-align: center; user-select: none; min-width: 0; }
input { position: absolute; width: 1px; height: 1px; opacity: 0; }
span { overflow-wrap: anywhere; }
small { margin-left: 4px; font-size: inherit; font-variant-numeric: tabular-nums; opacity: .8; }
label:hover, label:has(input:checked) { color: var(--text); }
label:has(input:focus-visible) { outline: 2px solid var(--spooky-green); outline-offset: 2px; }
label.disabled { color: color-mix(in srgb, var(--background-light) 45%, transparent); cursor: not-allowed; }
.segmented-toggle:hover::before { background: var(--button-1-highlight); }
@media (prefers-reduced-motion: reduce) { .segmented-toggle::before { transition: none; } }
</style>
