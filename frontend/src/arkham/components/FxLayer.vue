<script lang="ts" setup>
/* Shows what an update did to the board: damage and horror flying off an
 * investigator, an enemy lunging at them, doom landing on the agenda.
 *
 * It watches the game, diffs it against the previous state (gameDeltaFx.ts) and
 * draws floating numbers over whatever carries `data-fx-target="<kind>-<id>"`
 * (falling back to the older `data-investigator-mini` / enemy `data-id` hooks),
 * so it works the same in the card table, the map and the scene. An anchor that
 * is not on screen is skipped silently.
 *
 * While anything plays, `fxBusy` is true so the autopilot waits: the next
 * automatic answer must not run over an effect the player is still reading. */
import { onBeforeUnmount, ref, watch } from 'vue'
import type { Game } from '@/arkham/types/Game'
import { useSettings } from '@/stores/settings'
import { diffSnapshots, snapshotOf, FX_TIMINGS, type FxEvent, type FxKind, type FxSnapshot } from '@/arkham/gameDeltaFx'
import { holdFxBusy } from '@/arkham/composables/useFxBusy'

const props = defineProps<{ game: Game }>()
const settings = useSettings()

type Floater = { id: number; x: number; y: number; text: string; kind: FxKind }
const floaters = ref<Floater[]>([])
let nextId = 0
let previous: FxSnapshot | null = null
let previousStep = -1
let previousGameId = ''
const timers = new Set<ReturnType<typeof setTimeout>>()

const CLASS_FOR: Partial<Record<FxKind, string>> = {
  damage: 'fx-hit-damage',
  horror: 'fx-hit-horror',
  enemyDamage: 'fx-hit-damage',
  enemyDefeated: 'fx-defeated',
  doom: 'fx-doom',
  attack: 'fx-lunge',
}

function later(ms: number, fn: () => void) {
  const handle = setTimeout(() => {
    timers.delete(handle)
    fn()
  }, ms)
  timers.add(handle)
}

function findAnchor(anchor: string): HTMLElement | null {
  const exact = document.querySelector<HTMLElement>(`[data-fx-target="${anchor}"]`)
  if (exact) return exact
  const dash = anchor.indexOf('-')
  const entity = anchor.slice(0, dash)
  const id = anchor.slice(dash + 1)
  if (entity === 'investigator') return document.querySelector<HTMLElement>(`[data-investigator-mini="${id}"]`)
  if (entity === 'enemy') return document.querySelector<HTMLElement>(`img[data-id="${id}"]`)
  return null
}

function label(event: FxEvent): string {
  const n = Math.abs(event.amount)
  const sign = event.amount > 0 ? '+' : '-'
  switch (event.kind) {
    case 'damage': return `-${n} ♥`
    case 'horror': return `-${n} ◆`
    case 'enemyDamage': return `${event.amount > 0 ? '-' : '+'}${n}`
    case 'doom': return `${sign}${n} doom`
    case 'clue': return `${sign}${n} clue`
    case 'resource': return `${sign}${n} $`
    default: return ''
  }
}

function play(event: FxEvent) {
  const el = findAnchor(event.anchor)
  if (!el) return
  const rect = el.getBoundingClientRect()
  if (rect.width === 0 && rect.height === 0) return

  const cls = CLASS_FOR[event.kind]
  if (cls) {
    if (event.kind === 'attack' && event.toward) {
      const to = findAnchor(event.toward)?.getBoundingClientRect()
      if (to) {
        const dx = (to.left + to.width / 2 - (rect.left + rect.width / 2)) * 0.45
        const dy = (to.top + to.height / 2 - (rect.top + rect.height / 2)) * 0.45
        el.style.setProperty('--fx-dx', `${dx}px`)
        el.style.setProperty('--fx-dy', `${dy}px`)
      }
    }
    el.classList.remove(cls)
    void el.offsetWidth // restart the animation if it is already running
    el.classList.add(cls)
    later(FX_TIMINGS.floaterMs, () => el.classList.remove(cls))
  }

  const text = label(event)
  if (!text) return
  const floater: Floater = { id: nextId++, x: rect.left + rect.width / 2, y: rect.top + rect.height * 0.25, text, kind: event.kind }
  floaters.value = [...floaters.value, floater]
  later(FX_TIMINGS.floaterMs, () => {
    floaters.value = floaters.value.filter((f) => f.id !== floater.id)
  })
}

watch(
  () => props.game,
  (game) => {
    const next = snapshotOf(game)
    const step = game.scenarioSteps
    const fresh = previous === null || game.id !== previousGameId || step < previousStep
    const events = !fresh && settings.combatFx ? diffSnapshots(previous!, next) : []
    previous = next
    previousStep = step
    previousGameId = game.id
    if (events.length === 0) return

    const total = Math.max(...events.map((e) => e.delayMs)) + FX_TIMINGS.floaterMs
    holdFxBusy(total)
    for (const event of events) later(event.delayMs, () => play(event))
  },
  { immediate: true },
)

onBeforeUnmount(() => timers.forEach(clearTimeout))
</script>

<template>
  <div class="fx-layer" aria-hidden="true">
    <span v-for="f in floaters" :key="f.id" class="fx-floater" :class="`fx-floater--${f.kind}`" :style="{ left: f.x + 'px', top: f.y + 'px' }">
      {{ f.text }}
    </span>
  </div>
</template>

<style lang="scss">
.fx-layer {
  position: fixed;
  inset: 0;
  pointer-events: none;
  z-index: 9000;
}

.fx-floater {
  position: absolute;
  transform: translate(-50%, 0);
  font-weight: 800;
  font-size: 1.5rem;
  white-space: nowrap;
  text-shadow: 0 0 6px #000, 0 2px 2px #000;
  animation: fx-float 1.1s ease-out forwards;
}
.fx-floater--damage, .fx-floater--enemyDamage { color: #ff5a4d; }
.fx-floater--horror { color: #b58cff; }
.fx-floater--doom { color: #ff9a3c; }
.fx-floater--clue { color: #7fe3ff; }
.fx-floater--resource { color: #ffe27a; }

@keyframes fx-float {
  0% { opacity: 0; transform: translate(-50%, 10px) scale(0.7); }
  15% { opacity: 1; transform: translate(-50%, 0) scale(1.25); }
  100% { opacity: 0; transform: translate(-50%, -46px) scale(1); }
}

.fx-hit-damage { animation: fx-hit-red 0.6s ease-out; }
.fx-hit-horror { animation: fx-hit-purple 0.6s ease-out; }
.fx-doom { animation: fx-doom-pulse 0.9s ease-out; }
.fx-lunge { animation: fx-lunge 0.45s ease-in-out; }
.fx-defeated { animation: fx-defeated 0.8s ease-in forwards; }

@keyframes fx-hit-red {
  0%, 100% { filter: none; transform: none; }
  15% { filter: brightness(1.6) drop-shadow(0 0 8px #ff2a1c); transform: translateX(-4px); }
  35% { transform: translateX(4px); }
  55% { transform: translateX(-3px); }
  75% { transform: translateX(2px); }
}
@keyframes fx-hit-purple {
  0%, 100% { filter: none; transform: none; }
  20%, 60% { filter: brightness(0.7) saturate(1.5) drop-shadow(0 0 10px #9a5cff); transform: scale(1.04); }
}
@keyframes fx-doom-pulse {
  0%, 100% { filter: none; transform: none; }
  25% { filter: drop-shadow(0 0 12px #ff7a1a) brightness(1.4); transform: scale(1.18); }
}
@keyframes fx-lunge {
  0%, 100% { transform: translate(0, 0); }
  45% { transform: translate(var(--fx-dx, 0), var(--fx-dy, 0)) scale(1.1); }
}
@keyframes fx-defeated {
  to { opacity: 0; transform: translateY(14px) scale(0.9); filter: grayscale(1); }
}

@media (prefers-reduced-motion: reduce) {
  .fx-hit-damage, .fx-hit-horror, .fx-doom, .fx-lunge, .fx-defeated { animation: none; }
  .fx-floater { animation-duration: 0.01s; opacity: 1; }
}
</style>
