<script lang="ts" setup>
/* The segment on the right of a field that swaps it for the binding picker.
 *
 * A child of the field's own row rather than something laid over it, so it can
 * be a real segment -- full height, divided from the value by a rule, sharing
 * the field's border. It renders nothing when no binding in scope would fit the
 * field, which is what keeps it from reading as a second clear button.
 */
/* Both looks live here rather than being restyled by whoever renders it: a rule
 * in the parent's stylesheet ties on specificity with this one, so which of them
 * wins comes down to the order the two files happen to be injected in -- and the
 * toggle silently loses its box the day an unrelated component is added. */
defineProps<{ open: boolean; count: number; type: string; floating?: boolean; chip?: boolean }>()
defineEmits<{ toggle: [] }>()
</script>

<template>
  <button
    v-if="count"
    type="button"
    class="binding-toggle"
    :class="{ on: open, floating, chip }"
    :title="open ? 'Back to the value' : `Use one of the ${count} bindings that fit ${type}`"
    @click="$emit('toggle')"
  >
    $
  </button>
</template>

<style scoped lang="scss">
.binding-toggle {
  align-items: center;
  align-self: stretch;
  background: transparent;
  border: none;
  // The divider is what makes it read as a segment of the field rather than a
  // glyph floating inside it.
  border-left: 1px solid #4b5563;
  border-radius: 0;
  color: #6b7280;
  cursor: pointer;
  display: flex;
  flex: none;
  font-family: inherit;
  font-size: 0.8rem;
  line-height: 1;
  margin: -0.35rem -0.5rem -0.35rem 0;
  padding: 0 0.55rem;

  &:hover {
    background: rgba(20, 184, 166, 0.12);
    color: #5eead4;
  }

  &.on {
    background: #134e4a;
    color: #5eead4;
  }

  /* A segment of a binding chip rather than of a grey field: it takes the chip's
     own colour, which is also what turns the divider red when the chip is
     naming something nothing in scope binds. */
  &.chip {
    border-left-color: currentColor;
    color: inherit;
    margin: 0;

    &:hover {
      background: rgba(255, 255, 255, 0.14);
      color: inherit;
    }
  }

  // The fallback for shapes with no row of their own: a box of its own laid
  // over the corner of the field, so it needs the border and padding back.
  &.floating {
    border: 1px solid #4b5563;
    border-radius: 3px;
    margin: 0;
    padding: 0.25rem 0.5rem;
    position: absolute;
    right: 0;
    top: 0;
  }
}
</style>
