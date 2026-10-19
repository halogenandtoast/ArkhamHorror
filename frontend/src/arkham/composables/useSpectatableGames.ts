import { onUnmounted, ref, Ref } from 'vue'
import { fetchSpectatableGames, fetchSpectatableGameActivity } from '@/arkham/api'
import type { GameDetails } from '@/arkham/types/Game'

/*
 * The watch list is a lobby: somebody sits on it waiting for a table to open
 * up, so it has to notice one appearing without being reloaded. There is no
 * push for "a game opted in" -- allowSpectators is decided when the game is
 * created -- and a game leaves the list by ending, which nothing announces
 * either, so this polls.
 *
 * Cheaply, the way the deck screen does it (see useStepPoller): the probe asks
 * only for the ids, and the list -- which decodes a whole game per row to name
 * its scenario and investigators -- is fetched only when that set changes. The
 * interval grows while nothing opens or closes, so a page left open settles at
 * SLOW_MS, and a backgrounded tab stops entirely and resyncs when it is looked
 * at again.
 *
 * The probe also reports how long each game has been quiet, which is how the
 * list tells a game being played from one whose players wandered off -- both
 * are IsActive and otherwise look identical. Read off the probe rather than the
 * rows, so noticing a table go quiet costs no extra request, and refreshed on
 * every tick, so the mark appears within one interval of the game going idle.
 */
const FAST_MS = 5000
const SLOW_MS = 20000
const GROWTH = 1.5

/* How long without an action before a game is called inactive. Long enough
 * that a player thinking through a turn does not trip it. */
const INACTIVE_AFTER_SECONDS = 5 * 60

export function useSpectatableGames() {
  const games: Ref<GameDetails[]> = ref([])
  /* The games on the list that nobody has touched lately, by id. Keyed by id
   * rather than folded into the rows, because the two are fetched on different
   * schedules: the idle times refresh every tick, the rows only when the set of
   * games moves. */
  const inactiveIds: Ref<Set<string>> = ref(new Set())
  /* Only the first load, so the page says "looking" once rather than blinking
   * back to it on every refresh. */
  const loading = ref(true)

  let timer: ReturnType<typeof setTimeout> | null = null
  let lastIds: string | null = null
  let delay = FAST_MS
  let pausedWhileHidden = false
  let stopped = false

  // So everyone parked on the page cannot line up on the same instant.
  const jitter = (ms: number) => Math.round(ms * (0.85 + Math.random() * 0.3))

  const schedule = (ms: number) => {
    if (timer !== null) clearTimeout(timer)
    timer = setTimeout(tick, ms)
  }

  async function tick() {
    timer = null
    if (document.hidden) {
      pausedWhileHidden = true
      return
    }

    try {
      const activity = await fetchSpectatableGameActivity()
      inactiveIds.value = new Set(
        activity.filter((a) => a.idleSeconds >= INACTIVE_AFTER_SECONDS).map((a) => a.id),
      )
      // The probe is sorted by id, so this is membership and nothing else: a
      // game going quiet must not buy the whole list again.
      const ids = activity.map((a) => a.id).join(',')
      if (ids !== lastIds) {
        games.value = await fetchSpectatableGames()
        // Only once the list we fetched for it is actually in hand, so a
        // failure retries the set rather than writing it off as seen.
        lastIds = ids
        delay = FAST_MS
      } else {
        delay = Math.min(SLOW_MS, delay * GROWTH)
      }
    } catch {
      /* A quiet failure: the page keeps the list it has, which is what a failed
       * fetch looks like from the outside anyway. */
      delay = Math.min(SLOW_MS, Math.max(delay, FAST_MS))
    } finally {
      loading.value = false
    }

    if (!stopped) schedule(jitter(delay))
  }

  const onVisibilityChange = () => {
    if (document.hidden || !pausedWhileHidden || stopped) return
    pausedWhileHidden = false
    delay = FAST_MS
    schedule(0)
  }

  document.addEventListener('visibilitychange', onVisibilityChange)
  onUnmounted(() => {
    stopped = true
    if (timer !== null) clearTimeout(timer)
    timer = null
    document.removeEventListener('visibilitychange', onVisibilityChange)
  })

  /* The first probe counts as a change, so the initial load is the same code
   * path as every refresh and nothing can open up in the gap between them. */
  schedule(0)

  return { games, inactiveIds, loading }
}
