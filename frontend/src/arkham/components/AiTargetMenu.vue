<script setup lang="ts">
import { OnClickOutside } from '@vueuse/components'
import { ref, watch, computed, nextTick } from 'vue'
import type { Target } from '@/arkham/types/Target'
import type { AiFocus } from '@/arkham/types/NewGame'
import { useDebug } from '@/arkham/debug'
import { useAi } from '@/arkham/ai'

// Dev-only context menu for "AI targeting mode" (see useAi / AiControlPanel).
// Mirrors AbilitiesMenu's teleport + click-outside + frame-anchored positioning,
// but lists a fixed set of AI directives for the clicked entity instead of game
// abilities. Choosing one pushes the directive over the RAW debug channel and
// exits targeting mode.
type Kind = 'location' | 'enemy' | 'asset' | 'investigator'

const props = withDefaults(defineProps<{
  frame: HTMLElement | null
  kind: Kind
  target: Target
  seat: string | null
  gameId: string
  position?: 'top' | 'bottom' | 'left' | 'right'
}>(), { position: 'top' })

const open = defineModel<boolean>()
const debug = useDebug()
const ai = useAi()

interface Directive {
  label: string
  focus?: AiFocus
}

const directives = computed<Directive[]>(() => {
  switch (props.kind) {
    case 'location':
      return [
        { label: 'Move here', focus: 'mobility' },
        { label: 'Investigate here', focus: 'investigate' },
      ]
    case 'enemy':
      return [
        { label: 'Fight this enemy', focus: 'combat' },
        { label: 'Evade this enemy', focus: 'evade' },
      ]
    case 'asset':
    case 'investigator':
    default:
      return [{ label: 'Prioritize' }]
  }
})

interface Position {
  bottom?: string
  top?: string
  left?: string
  right?: string
}

const menuRef = ref<HTMLElement | null>(null)
const menuPosition = ref<Position>({ bottom: '0px', top: '0px', left: '0px' })
const positionClass = computed(() => props.position)

function calculatePosition() {
  if (!props.frame) return
  const rect = props.frame.getBoundingClientRect()
  const menuRect = menuRef.value?.getBoundingClientRect()
  const menuWidth = menuRect?.width ?? 160
  const margin = 8
  const maxLeft = Math.max(margin, window.innerWidth - menuWidth - margin)
  const clampedLeft = (left: number) => `${Math.min(Math.max(left, margin), maxLeft) + window.scrollX}px`
  const positionStyle: Position = {}

  switch (positionClass.value) {
    case 'bottom':
      positionStyle.top = `${rect.bottom + window.scrollY}px`
      positionStyle.left = clampedLeft(rect.left)
      break
    case 'left':
      positionStyle.top = `${rect.top + window.scrollY}px`
      if (rect.left - menuWidth - margin < 0) {
        positionStyle.left = clampedLeft(rect.right + margin)
      } else {
        positionStyle.right = `${window.innerWidth - rect.left + window.scrollX}px`
      }
      break
    case 'right':
      positionStyle.top = `${rect.top + window.scrollY}px`
      if (rect.right + menuWidth + margin > window.innerWidth) {
        positionStyle.right = `${window.innerWidth - rect.left + margin + window.scrollX}px`
      } else {
        positionStyle.left = clampedLeft(rect.right + margin)
      }
      break
    case 'top':
    default:
      positionStyle.bottom = `${window.innerHeight - rect.top - window.scrollY}px`
      positionStyle.left = clampedLeft(rect.left)
      break
  }

  menuPosition.value = positionStyle
}

function choose(directive: Directive) {
  const { seat } = props
  if (seat) {
    debug.send(props.gameId, { tag: 'AddAiPriority', contents: [seat, props.target] })
    if (directive.focus) {
      debug.send(props.gameId, { tag: 'SetAiFocusOverride', contents: [seat, directive.focus] })
    }
    // Pause the auto-driver after a manual directive so the AI doesn't immediately
    // act on it; resume from the panel's Running/Paused button when ready.
    ai.enabled = false
  }
  open.value = false
  ai.stopTargeting()
}

watch(open, (value) => {
  if (value) {
    nextTick(() => calculatePosition())
  }
})
</script>

<template>
  <Teleport to="body">
    <OnClickOutside @trigger="open = false" v-if="open" :options="{ ignore: [frame] }">
      <div class="ai-target-menu" :class="position" :style="menuPosition" ref="menuRef">
        <div class="ai-target-menu__header">AI directive</div>
        <button
          v-for="directive in directives"
          :key="directive.label"
          type="button"
          class="ai-target-menu__button"
          @click="choose(directive)"
        >
          {{ directive.label }}
        </button>
      </div>
    </OnClickOutside>
  </Teleport>
</template>

<style scoped>
.ai-target-menu {
  position: absolute;
  padding: min(4px, 1vw);
  background: rgba(10, 22, 14, 0.92);
  border: 1px solid var(--ai-target);
  border-radius: 8px;
  display: flex;
  flex-direction: column;
  gap: 4px;
  z-index: var(--z-index-1000);
  box-shadow: 0 8px 24px rgba(0, 0, 0, 0.5);
}

.ai-target-menu__header {
  font-size: 9px;
  letter-spacing: 0.08em;
  text-transform: uppercase;
  color: rgba(74, 222, 128, 0.85);
  padding: 2px 6px;
}

.ai-target-menu__button {
  border: 1px solid rgba(74, 222, 128, 0.5);
  background: rgba(74, 222, 128, 0.16);
  color: #d7f5dd;
  border-radius: 6px;
  padding: 5px 10px;
  cursor: pointer;
  font-size: 12px;
  text-align: left;
  white-space: nowrap;
}

.ai-target-menu__button:hover {
  background: rgba(74, 222, 128, 0.32);
}
</style>
