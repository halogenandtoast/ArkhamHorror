<script lang="ts" setup>
/* The `$names` a block can refer to.
 *
 * A binding is only a string once it is written down, so nothing in the JSON says
 * where one came from and a name that was never bound fails silently. Naming them
 * where they are usable is the only thing that makes them discoverable -- which is
 * as true of an ability's own fields (its criteria can say `$controller`) as it is
 * of the steps below them, so this is one control used in both places rather than
 * a list of names in one of them.
 *
 * A chip whose binding says where it came from can be clicked to go there.
 */
import { jumpToBinding, type Binding } from '@/arkham/customCardBindings'

withDefaults(defineProps<{ bindings: Binding[]; label?: string }>(), {
  label: 'In scope here:',
})
</script>

<template>
  <p v-if="bindings.length" class="scope-bar">
    <span class="scope-label">{{ label }}</span>
    <button
      v-for="bound in bindings"
      :key="bound.name"
      type="button"
      class="scope-chip"
      :class="{ jumpable: !!bound.anchor }"
      :title="`${bound.detail ? bound.detail + ' · ' : ''}${bound.origin}${bound.anchor ? ' — click to show' : ''}`"
      @click="jumpToBinding(bound.anchor)"
    >
      ${{ bound.name }}
    </button>
  </p>
</template>

<style scoped lang="scss">
.scope-bar {
  align-items: center;
  display: flex;
  flex-wrap: wrap;
  gap: 0.25rem;
  margin: 0 0 0.2rem;
}

.scope-label {
  color: #9ca3af;
  font-size: 0.72rem;
}

.scope-chip {
  background: #1f2937;
  border: 1px solid #374151;
  border-radius: 999px;
  color: #5eead4;
  cursor: default;
  font-family: inherit;
  font-size: 0.72rem;
  padding: 0.05rem 0.45rem;

  &.jumpable {
    cursor: pointer;

    &:hover {
      border-color: #14b8a6;
    }
  }
}
</style>
