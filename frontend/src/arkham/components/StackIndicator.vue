<script lang="ts" setup>
import { computed } from 'vue'
import { Dropdown } from 'floating-vue'
import { type Card, cardImage } from '@/arkham/types/Card'
import { imgsrc } from '@/arkham/helpers'

type PipState = 'completed' | 'current' | 'remaining'

export type StackIndicatorGroup = {
  label: string
  state: PipState
  images: {
    src: string
    current?: boolean
    passed?: boolean
  }[]
}

const props = defineProps<{
  label: string
  current: number
  total: number
  completedCards: Card[]
  currentImage: string
  remainingCards: Card[]
  placement?: 'top' | 'bottom' | 'left' | 'right'
  groups?: StackIndicatorGroup[]
}>()

const pips = computed<PipState[]>(() => {
  if (props.groups) return props.groups.map((group) => group.state)

  return Array.from({ length: props.total }, (_, i) => {
    const position = i + 1
    if (position < props.current) return 'completed'
    if (position === props.current) return 'current'
    return 'remaining'
  })
})

const popoverGroups = computed<StackIndicatorGroup[]>(() => {
  if (props.groups) return props.groups

  return [
    ...props.completedCards.map((card, i) => ({
      label: `${props.label} ${i + 1}`,
      state: 'completed' as const,
      images: [{ src: imgsrc(cardImage(card)), passed: true }],
    })),
    {
      label: `${props.label} ${props.current}`,
      state: 'current' as const,
      images: [{ src: props.currentImage, current: true }],
    },
    ...props.remainingCards.map((card, i) => ({
      label: `${props.label} ${props.current + i + 1}`,
      state: 'remaining' as const,
      images: [{ src: imgsrc(cardImage(card)) }],
    })),
  ]
})

const hasCardGroups = computed(() =>
  popoverGroups.value.some((group) => group.images.length > 1)
)

const expandable = computed(() =>
  props.completedCards.length > 0 || props.remainingCards.length > 0 || hasCardGroups.value
)
const placement = computed(() => props.placement ?? 'bottom')

const currentPosition = computed(() => {
  const index = pips.value.findIndex((state) => state === 'current')
  return index === -1 ? props.current : index + 1
})

const tooltip = computed(() => {
  const base = `${props.label} ${currentPosition.value} / ${pips.value.length}`
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
      <div class="stack-popover" :class="{ 'stack-popover--grouped': hasCardGroups }">
        <div
          v-for="(group, groupIndex) in popoverGroups"
          :key="`${group.label}-${groupIndex}`"
          class="stack-popover__group"
          :class="{ 'stack-popover__group--multi': group.images.length > 1 }"
        >
          <div
            v-if="hasCardGroups"
            class="stack-popover__group-label"
            :class="{ 'stack-popover__group-label--hidden': group.images.length === 1 }"
          >
            {{ group.label }} ×{{ group.images.length }}
          </div>
          <div class="stack-popover__group-cards">
            <img
              v-for="(image, imageIndex) in group.images"
              :key="`${group.label}-${imageIndex}`"
              :src="image.src"
              class="card stack-popover__card"
              :class="{
                'stack-popover__card--passed': image.passed,
                'stack-popover__card--current': image.current,
              }"
            />
          </div>
        </div>
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
  align-items: center;
  gap: 8px;
  padding: 10px;
  max-width: 80vw;
  overflow-x: auto;
}

.stack-popover__group {
  display: flex;
  flex-direction: column;
  align-items: center;
  justify-content: center;
  gap: 6px;
  border: 1px solid transparent;
  border-radius: 8px;
}

.stack-popover--grouped .stack-popover__group {
  padding: 6px;
}

.stack-popover__group--multi {
  border-color: rgba(255, 255, 255, 0.18);
  background: rgba(255, 255, 255, 0.06);
}

.stack-popover__group-label {
  font-size: 0.75rem;
  font-weight: 700;
  line-height: 1;
  color: rgba(255, 255, 255, 0.82);
  text-transform: uppercase;
  letter-spacing: 0.04em;
}

.stack-popover__group-label--hidden {
  visibility: hidden;
}

.stack-popover__group-cards {
  display: flex;
  flex-direction: row;
  align-items: center;
  gap: 6px;
}

.stack-popover__card {
  width: calc(var(--card-width, 100px) * 1.1);
  border-radius: 6px;
  border: 1px solid rgba(255, 255, 255, 0.38);
  box-sizing: border-box;
  box-shadow: 0 2px 8px rgba(0, 0, 0, 0.6);
  cursor: zoom-in;
  transition: opacity 0.15s ease, border-color 0.15s ease;
}

.stack-popover__card--passed {
  opacity: 0.4;
  filter: grayscale(0.4);
}

.stack-popover__card--current {
  border-color: transparent;
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
