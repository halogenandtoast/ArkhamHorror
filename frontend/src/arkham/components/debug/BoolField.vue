<script lang="ts" setup>
/* A yes/no field.
 *
 * Two named choices rather than a checkbox: a bare box leaves you reading the
 * label to work out which way is on, and an unset field looks the same as a
 * false one. The same control wherever the builder asks a yes/no question, so a
 * flag on a step and a Bool field off the schema are visibly the same kind of
 * thing.
 */
withDefaults(defineProps<{ modelValue?: boolean | null; label?: string }>(), {
  modelValue: false,
})
defineEmits<{ 'update:modelValue': [v: boolean] }>()

// Pairs the two radios without the caller having to name them.
const group = `bool-${Math.random().toString(36).slice(2, 9)}`
</script>

<template>
  <div class="bool-field">
    <span v-if="label" class="bool-caption">{{ label }}</span>
    <div class="bool-toggle">
      <span class="bool-thumb" :class="{ on: !!modelValue }" aria-hidden="true"></span>
      <label class="bool-option" :class="{ active: !modelValue }">
        <input
          type="radio"
          :name="group"
          :checked="!modelValue"
          @change="$emit('update:modelValue', false)"
        />
        false
      </label>
      <label class="bool-option" :class="{ active: !!modelValue }">
        <input
          type="radio"
          :name="group"
          :checked="!!modelValue"
          @change="$emit('update:modelValue', true)"
        />
        true
      </label>
    </div>
  </div>
</template>

<style scoped lang="scss">
/* Laid out like the builder's other fields: caption above, control below, so a
 * yes/no sits in a row of text and number fields without breaking the line. */
.bool-field {
  display: flex;
  flex: none;
  flex-direction: column;
  gap: 0.2rem;
  min-width: 0;
}

.bool-caption {
  font-size: 0.75rem;
  opacity: 0.9;
}

.bool-toggle {
  align-self: flex-start;
  background: #111827;
  border: 1px solid #4b5563;
  border-radius: 999px;
  display: inline-flex;
  padding: 2px;
  position: relative;
}

.bool-thumb {
  background: #374151;
  border-radius: 999px;
  bottom: 2px;
  left: 2px;
  position: absolute;
  top: 2px;
  transition: transform 0.15s ease, background 0.15s ease;
  width: calc(50% - 2px);

  &.on {
    background: #134e4a;
    transform: translateX(100%);
  }
}

.bool-option {
  border-radius: 999px;
  color: #9ca3af;
  cursor: pointer;
  font-size: 0.75rem;
  min-width: 3.2rem;
  padding: 0.2rem 0.6rem;
  position: relative;
  text-align: center;
  user-select: none;
  z-index: 1;

  &.active {
    color: #eee;
  }

  &:last-child.active {
    color: #5eead4;
  }

  input {
    position: absolute;
    opacity: 0;
    pointer-events: none;
  }
}
</style>
