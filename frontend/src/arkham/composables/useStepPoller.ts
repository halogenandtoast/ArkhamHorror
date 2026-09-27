import { onUnmounted } from 'vue'
import { fetchGameStep } from '@/arkham/api'

/*
 * The deck screen has no reliable push for "another player finished", so it
 * probes the step column and only pays for the whole game when the step moved.
 * The probe is a few bytes, but a table parked on that screen still meant one
 * request per client per second for as long as it sat there.
 *
 * Two things keep that small. The interval grows while nothing changes, so an
 * idle screen settles at SLOW_MS instead of hammering; any change snaps it back
 * to FAST_MS, which is what matters -- once one player moves, the rest usually
 * follow immediately. And a backgrounded tab stops entirely, resuming with an
 * immediate probe when it is looked at again.
 *
 * Deliberately still cheap and self-limiting (see the long note in Game.vue
 * about the watchdog that took the site down): step only, jittered, one timer.
 */
const FAST_MS = 1000
const SLOW_MS = 5000
const GROWTH = 1.6

export interface StepPollerOptions {
  gameId: () => string
  /* Called when the probe reports a step we have not seen yet. */
  onChange: (step: number) => void | Promise<void>
  /* Checked after every tick; false ends the poll. */
  shouldContinue: () => boolean
}

export function useStepPoller(options: StepPollerOptions) {
  let timer: ReturnType<typeof setTimeout> | null = null
  let lastStep: number | null = null
  let delay = FAST_MS
  let pausedWhileHidden = false

  // So a table full of clients cannot line up on the same instant.
  const jitter = (ms: number) => Math.round(ms * (0.85 + Math.random() * 0.3))

  const schedule = (ms: number) => {
    if (timer !== null) clearTimeout(timer)
    timer = setTimeout(tick, ms)
  }

  const stop = () => {
    if (timer !== null) clearTimeout(timer)
    timer = null
    pausedWhileHidden = false
  }

  async function tick() {
    timer = null
    if (document.hidden) {
      pausedWhileHidden = true
      return
    }

    try {
      const step = await fetchGameStep(options.gameId())
      if (step !== lastStep) {
        lastStep = step
        delay = FAST_MS
        await options.onChange(step)
      } else {
        delay = Math.min(SLOW_MS, delay * GROWTH)
      }
    } catch {
      delay = Math.min(SLOW_MS, Math.max(delay, 2000))
    }

    if (options.shouldContinue()) schedule(jitter(delay))
  }

  /* Starting counts the first tick as a change, so it resyncs once. */
  const start = () => {
    if (timer !== null || pausedWhileHidden) return
    lastStep = null
    delay = FAST_MS
    schedule(500)
  }

  const onVisibilityChange = () => {
    if (document.hidden || !pausedWhileHidden) return
    pausedWhileHidden = false
    if (!options.shouldContinue()) return
    delay = FAST_MS
    schedule(0)
  }

  document.addEventListener('visibilitychange', onVisibilityChange)
  onUnmounted(() => {
    stop()
    document.removeEventListener('visibilitychange', onVisibilityChange)
  })

  return { start, stop }
}
