<script lang="ts" setup>
import { computed } from 'vue'
import { Dropdown } from 'floating-vue'
import { type Card, cardImage } from '@/arkham/types/Card'
import { imgsrc } from '@/arkham/helpers'

const props = defineProps<{
  label: string
  current: number
  total: number
  completedCards: Card[]
  currentImage: string
  remainingCards: Card[]
  placement?: 'top' | 'bottom' | 'left' | 'right'
}>()

const pips = computed(() =>
  Array.from({ length: props.total }, (_, i) => {
    const position = i + 1
    if (position < props.current) return 'completed'
    if (position === props.current) return 'current'
    return 'remaining'
  })
)

const expandable = computed(() =>
  props.completedCards.length > 0 || props.remainingCards.length > 0
)
const placement = computed(() => props.placement ?? 'bottom')

const tooltip = computed(() => {
  const base = `${props.label} ${props.current} / ${props.total}`
  return expandable.value ? `${base} — click to view deck` : base
})
</script>

<template>
  <Dropdown
    :placement="placement"
    :distance="8"
    :disabled="!expandable"
    :triggers="['click']"
    theme="stack-indicator-popover"
  >
    <div
      class="stack-indicator"
      :class="{ 'is-expandable': expandable }"
      v-tooltip="tooltip"
    >
      <span
        v-for="(state, i) in pips"
        :key="i"
        class="pip"
        :class="`pip--${state}`"
      />
    </div>
    <template #popper>
      <div class="stack-popover">
        <img
          v-for="(card, i) in completedCards"
          :key="`done-${i}`"
          :src="imgsrc(cardImage(card))"
          class="card stack-popover__card stack-popover__card--passed"
        />
        <img
          :src="currentImage"
          class="card stack-popover__card stack-popover__card--current"
        />
        <img
          v-for="(card, i) in remainingCards"
          :key="`next-${i}`"
          :src="imgsrc(cardImage(card))"
          class="card stack-popover__card"
        />
      </div>
    </template>
  </Dropdown>
</template>

<style scoped>
.stack-indicator {
  display: inline-flex;
  flex-direction: column;
  align-items: center;
  gap: 5px;
  padding: 8px 5px;
  border-radius: 999px;
  background: rgba(0, 0, 0, 0.35);
  backdrop-filter: blur(4px);
  user-select: none;
  align-self: center;
  border: 1px solid rgba(255, 255, 255, 0.08);
  transition: background 0.15s ease, border-color 0.15s ease;
}

.stack-indicator.is-expandable {
  cursor: pointer;
}

.stack-indicator.is-expandable:hover {
  background: rgba(0, 0, 0, 0.55);
  border-color: rgba(255, 255, 255, 0.25);
}

.pip {
  width: 6px;
  height: 6px;
  border-radius: 50%;
  border: 1px solid rgba(255, 255, 255, 0.55);
  background: transparent;
  transition: transform 0.15s ease, background 0.15s ease, box-shadow 0.15s ease;
}

.pip--completed {
  background: rgba(255, 255, 255, 0.55);
  border-color: rgba(255, 255, 255, 0.55);
}

.pip--current {
  background: #fff;
  border-color: #fff;
  transform: scale(1.2);
  box-shadow: 0 0 3px rgba(255, 255, 255, 0.3);
}

.pip--remaining {
  background: transparent;
  border-color: rgba(255, 255, 255, 0.35);
}

.stack-popover {
  display: flex;
  flex-direction: row;
  align-items: flex-end;
  gap: 6px;
  padding: 10px;
  max-width: 80vw;
  overflow-x: auto;
}

.stack-popover__card {
  width: calc(var(--card-width, 100px) * 1.1);
  border-radius: 6px;
  box-shadow: 0 2px 8px rgba(0, 0, 0, 0.6);
  cursor: zoom-in;
  transition: opacity 0.15s ease;
}

.stack-popover__card--passed {
  opacity: 0.4;
  filter: grayscale(0.4);
}

.stack-popover__card--current {
  outline: 2px solid rgba(255, 255, 255, 0.85);
  outline-offset: 2px;
  box-shadow: 0 0 12px rgba(255, 255, 255, 0.35), 0 2px 8px rgba(0, 0, 0, 0.6);
}

</style>

<style>
.v-popper--theme-stack-indicator-popover .v-popper__inner {
  background: rgba(15, 15, 20, 0.92);
  backdrop-filter: blur(8px);
  border: 1px solid rgba(255, 255, 255, 0.12);
  border-radius: 10px;
  color: #fff;
  box-shadow: 0 8px 30px rgba(0, 0, 0, 0.5);
}

.v-popper--theme-stack-indicator-popover .v-popper__arrow-outer {
  border-color: rgba(255, 255, 255, 0.12);
}

.v-popper--theme-stack-indicator-popover .v-popper__arrow-inner {
  border-color: rgba(15, 15, 20, 0.92);
}
</style>
