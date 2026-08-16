import { defineStore } from "pinia"
import { computed, ref } from "vue"
import {
  getGameLocalStorageItem,
  removeGameLocalStorageItem,
  setGameLocalStorageItem,
} from '@/arkham/localStorage'
import { isDevBuild } from '@/arkham/displayRules'

const EPIC_MULTIPLAYER_KEY = 'epicMultiplayerEnabled'

// Decorative effects that are pure flourish — the WebGL fire on a burning
// location, the Cosmic Emissary laser beams. Never anything that carries game
// information. Deliberately NOT one of the `legacyGlobalGameSettingKeys` in
// arkham/localStorage.ts, which cullGameLocalStorage deletes on sight.
const EXTRA_ANIMATIONS_KEY = 'arkhamExtraAnimations'
// Same preference, scoped to one game, so a heavy scenario can be turned down
// without changing what every other game does.
const EXTRA_ANIMATIONS_SETTING = 'extraAnimations'

export const useSettings = defineStore("settings", () => {
  const gameId = ref<string | null>(null)
  const splitView = ref(false)

  // Dev-only feature flag for Epic Multiplayer. Stored in localStorage, but
  // exposed as `isDevBuild() && stored` so a stale value can never enable it in
  // production builds.
  const epicMultiplayerStored = ref(localStorage.getItem(EPIC_MULTIPLAYER_KEY) === 'true')
  const epicMultiplayerEnabled = computed(() => isDevBuild() && epicMultiplayerStored.value)

  function setEpicMultiplayerEnabled(enabled: boolean) {
    epicMultiplayerStored.value = enabled
    localStorage.setItem(EPIC_MULTIPLAYER_KEY, String(enabled))
  }

  function toggleEpicMultiplayer() {
    setEpicMultiplayerEnabled(!epicMultiplayerStored.value)
  }

  // Global player preference, on unless explicitly turned off.
  const extraAnimationsGlobal = ref(localStorage.getItem(EXTRA_ANIMATIONS_KEY) !== 'false')
  // Per-scenario override. null means "inherit the global preference" — which is
  // why this is a tri-state and not a boolean.
  const extraAnimationsOverride = ref<boolean | null>(null)

  // The OS accessibility preference is not a tie-breaker, it is an override:
  // someone who asked their system for less motion gets less motion regardless
  // of what either preference above says.
  const motionQuery =
    typeof window !== 'undefined' && typeof window.matchMedia === 'function'
      ? window.matchMedia('(prefers-reduced-motion: reduce)')
      : null
  const prefersReducedMotion = ref(motionQuery?.matches ?? false)
  motionQuery?.addEventListener('change', (event) => {
    prefersReducedMotion.value = event.matches
  })

  const extraAnimations = computed(
    () =>
      !prefersReducedMotion.value &&
      (extraAnimationsOverride.value ?? extraAnimationsGlobal.value),
  )

  function setExtraAnimationsGlobal(enabled: boolean) {
    extraAnimationsGlobal.value = enabled
    localStorage.setItem(EXTRA_ANIMATIONS_KEY, String(enabled))
  }

  function setExtraAnimationsOverride(value: boolean | null) {
    extraAnimationsOverride.value = value
    if (!gameId.value) return
    if (value === null) {
      removeGameLocalStorageItem(gameId.value, EXTRA_ANIMATIONS_SETTING)
    } else {
      setGameLocalStorageItem(gameId.value, EXTRA_ANIMATIONS_SETTING, String(value))
    }
  }

  function setGameId(id: string) {
    gameId.value = id
    const saved = getGameLocalStorageItem(id, 'splitView')
    splitView.value = saved === 'true'

    const override = getGameLocalStorageItem(id, EXTRA_ANIMATIONS_SETTING)
    extraAnimationsOverride.value = override === null ? null : override === 'true'
  }

  function toggleSplitView() {
    splitView.value = !splitView.value
    if (gameId.value) {
      setGameLocalStorageItem(gameId.value, 'splitView', String(splitView.value))
    }
  }

  const showBonded = ref(false)

  function toggleShowBonded() {
    showBonded.value = !showBonded.value
  }
  return {
    splitView,
    toggleSplitView,
    showBonded,
    toggleShowBonded,
    setGameId,
    epicMultiplayerStored,
    epicMultiplayerEnabled,
    setEpicMultiplayerEnabled,
    toggleEpicMultiplayer,
    extraAnimations,
    extraAnimationsGlobal,
    extraAnimationsOverride,
    prefersReducedMotion,
    setExtraAnimationsGlobal,
    setExtraAnimationsOverride,
  }
})
