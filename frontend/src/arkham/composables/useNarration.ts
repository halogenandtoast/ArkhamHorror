import { computed, ref, watch, onBeforeUnmount, type Ref } from 'vue'
import type { Game } from '@/arkham/types/Game'
import { useSettings } from '@/stores/settings'
import { narrativeOf } from '@/arkham/composables/useNarrative'
import {
  narrationTracks,
  storyKeysInPlay,
  type NarrationManifest,
  type NarrationTrack,
} from '@/arkham/narration'

const ROOT = `${import.meta.env.BASE_URL.replace(/\/$/, '')}/audio/narration`

/* Module-level so the story panel's replay/stop buttons drive the same player
 * the game view owns. */
const playing = ref(false)
/* The browser refused to start audio before any click on the page. */
const blocked = ref(false)
const hasTracks = ref(false)
let replayCurrent: () => void = () => {}
let stopCurrent: () => void = () => {}

let manifestPromise: Promise<NarrationManifest | null> | null = null
function loadManifest(): Promise<NarrationManifest | null> {
  manifestPromise ??= fetch(`${ROOT}/manifest.json`, { cache: 'no-cache' })
    .then((r) => (r.ok ? (r.json() as Promise<NarrationManifest>) : null))
    .catch(() => null)
  return manifestPromise
}

export function useNarrationControls() {
  return {
    playing,
    blocked,
    hasTracks,
    replay: () => replayCurrent(),
    stop: () => stopCurrent(),
  }
}

/* Plays the pre-rendered narration of whatever story text any player is being
 * shown, so the whole table hears it at once (see arkham/narration.ts). Starts
 * when a Read question appears and stops the moment it is answered. */
export function useNarration(game: Ref<Game | null | undefined>) {
  const settings = useSettings()
  const manifest = ref<NarrationManifest | null>(null)
  void loadManifest().then((m) => (manifest.value = m))

  // A narrative variant rewrites the story text, which the recordings no longer match.
  const keys = computed(() => (narrativeOf(game.value) ? [] : storyKeysInPlay(game.value?.question)))
  const tracks = computed<NarrationTrack[]>(() =>
    narrationTracks(keys.value, manifest.value, settings.narrationLanguage, ROOT),
  )

  let audio: HTMLAudioElement | null = null
  let next: HTMLAudioElement | null = null
  let queue: NarrationTrack[] = []
  let index = 0

  const stop = () => {
    if (audio) {
      audio.onended = null
      audio.pause()
    }
    audio = null
    next = null
    playing.value = false
  }

  const preload = (track: NarrationTrack | undefined) => {
    if (!track) return null
    const a = new Audio(track.url)
    a.preload = 'auto'
    return a
  }

  const playAt = (i: number) => {
    stop()
    index = i
    const track = queue[i]
    if (!track) return
    audio = next && next.src.endsWith(track.url) ? next : preload(track)!
    audio.volume = settings.narrationVolume
    // Fetch the following track while this one plays, so there is no gap.
    next = preload(queue[i + 1])
    audio.onended = () => playAt(index + 1)
    playing.value = true
    audio.play().then(
      () => (blocked.value = false),
      (e: unknown) => {
        playing.value = false
        if (e instanceof DOMException && e.name === 'NotAllowedError') blocked.value = true
      },
    )
  }

  const start = () => {
    queue = settings.narration ? tracks.value : []
    hasTracks.value = tracks.value.length > 0
    if (queue.length > 0) playAt(0)
    else stop()
  }

  replayCurrent = () => {
    queue = tracks.value
    if (queue.length > 0) playAt(0)
  }
  stopCurrent = stop

  // Restart only when the story being shown changes, not on every game update.
  watch(
    () => tracks.value.map((t) => t.url).join('|'),
    () => start(),
  )
  watch(() => settings.narration, (on) => (on ? start() : stop()))
  watch(
    () => settings.narrationVolume,
    (v) => {
      if (audio) audio.volume = v
    },
  )

  onBeforeUnmount(() => {
    stop()
    hasTracks.value = false
    replayCurrent = () => {}
    stopCurrent = () => {}
  })
}
