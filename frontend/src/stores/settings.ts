import { defineStore } from "pinia"
import { computed, ref } from "vue"
import {
  getGameLocalStorageItem,
  removeGameLocalStorageItem,
  setGameLocalStorageItem,
} from '@/arkham/localStorage'
import { isDevBuild } from '@/arkham/displayRules'
import {
  defaultNarrationLanguage,
  readNarrationLanguage,
  type NarrationLanguage,
} from '@/arkham/narration'

// Spoken story narration (pre-rendered intro/resolution audio). A browser
// preference, so global: on unless explicitly turned off.
const NARRATION_KEY = 'arkhamNarration'
const NARRATION_LANGUAGE_KEY = 'arkhamNarrationLanguage'
const NARRATION_VOLUME_KEY = 'arkhamNarrationVolume'
// Synthesised horror ambience under a scenario (arkham/horrorAmbience.ts). Also
// silenced by the global Sounds switch.
const MUSIC_KEY = 'arkhamHorrorMusic'
const MUSIC_VOLUME_KEY = 'arkhamHorrorMusicVolume'

const LOCATION_SCALE_KEY = 'arkhamLocationScale'
const FIGURE_SCALE_KEY = 'arkhamFigureScale'
export const MIN_SCALE = 0.6
export const MAX_SCALE = 1.6

function readScale(value: string | null): number {
  const n = value === null ? NaN : Number(value)
  return Number.isFinite(n) ? Math.min(MAX_SCALE, Math.max(MIN_SCALE, n)) : 1
}

function readVolume(value: string | null, fallback = 0.8): number {
  const n = value === null ? NaN : Number(value)
  return Number.isFinite(n) ? Math.min(1, Math.max(0, n)) : fallback
}

const EPIC_MULTIPLAYER_KEY = 'epicMultiplayerEnabled'

// Decorative effects that are pure flourish — the WebGL fire on a burning
// location, the Cosmic Emissary laser beams. Never anything that carries game
// information. Deliberately NOT one of the `legacyGlobalGameSettingKeys` in
// arkham/localStorage.ts, which cullGameLocalStorage deletes on sight.
const EXTRA_ANIMATIONS_KEY = 'arkhamExtraAnimations'
// Same preference, scoped to one game, so a heavy scenario can be turned down
// without changing what every other game does.
const EXTRA_ANIMATIONS_SETTING = 'extraAnimations'

// Tuck permanents that do nothing during play (the `no-gameplay-effect` and
// `setup-only` card tags) into a stack beside the play area once setup is over.
// Off by default: they are still real cards, and some players want to see them.
const HIDE_INERT_CARDS_KEY = 'arkhamHideInertCards'

// Answer the engine's non-questions (a lone "draw encounter", "apply results",
// a forced single choice) automatically. On unless explicitly turned off.
const AUTOPILOT_KEY = 'arkhamAutopilot'
const COMBAT_FX_KEY = 'arkhamCombatFx'

// Ask before playing a card from hand, and warn before an action that may
// provoke an attack of opportunity. Both on unless explicitly turned off.
const CONFIRM_CARD_PLAYS_KEY = 'arkhamConfirmCardPlays'
const WARN_ATTACKS_OF_OPPORTUNITY_KEY = 'arkhamWarnAttacksOfOpportunity'

/* Show the cards you draw from your own deck, enlarged, the way an encounter
 * card is shown when it is drawn.
 *
 * Three-way rather than a boolean because the pacing complaint cuts both ways:
 * with it off the upkeep draw goes by with no beat at all, and with it on for
 * every draw the card that makes you draw six becomes a chore. 'upkeep' is the
 * middle the engine can actually distinguish -- draws the game forces, not draws
 * a card handed you.
 *
 * Deliberately NOT one of the `legacyGlobalGameSettingKeys` in
 * arkham/localStorage.ts, which cullGameLocalStorage deletes on sight.
 */
const DRAW_SPOTLIGHT_KEY = 'arkhamDrawSpotlight'
// Same preference, scoped to one game, so a deck that churns cards can be turned
// down without changing what every other game does.
const DRAW_SPOTLIGHT_SETTING = 'drawSpotlight'

// Cards you build yourself. Experimental: a custom card is only ever as correct
// as the def behind it, and the builder can express things the engine will
// happily run but no printed card would ever do.
const CUSTOM_CARDS_KEY = 'arkhamCustomCardsEnabled'
const INLINE_MODALS_KEY = 'arkhamInlineModals'

/* How a scenario draws its locations: 'scene' is the room you stand in with a small map beside it, 'map' the
 * full node map, 'cards' the original board of location cards. Anything stored that is not 'map' or 'cards'
 * means scene, so the scene is the default. */
const LOCATION_VIEW_KEY = 'arkhamLocationView'
export type LocationView = 'map' | 'cards'

/* Never trust what comes back out of localStorage: it survives a rename, and a
 * value this store does not understand would otherwise gate the overlay on a
 * string comparison that can never be true. */
export type DrawSpotlightMode = 'off' | 'upkeep' | 'every'
const DRAW_SPOTLIGHT_MODES: DrawSpotlightMode[] = ['off', 'upkeep', 'every']

function readDrawSpotlightMode(value: string | null): DrawSpotlightMode | null {
  return DRAW_SPOTLIGHT_MODES.find((m) => m === value) ?? null
}

function readLocationView(value: string | null): LocationView {
  return value === 'cards' ? 'cards' : 'map'
}

function loadVariants(): string[] {
  try {
    const value: unknown = JSON.parse(localStorage.getItem('arkhamUseVariants') ?? '[]')
    return Array.isArray(value) ? value.filter((v): v is string => typeof v === 'string') : []
  } catch {
    return []
  }
}

export const useSettings = defineStore("settings", () => {
  const gameId = ref<string | null>(null)
  const splitView = ref(false)
  const useVariants = ref<string[]>(loadVariants())

  function setUseVariants(variants: string[]) {
    useVariants.value = [...new Set(variants)]
    localStorage.setItem('arkhamUseVariants', JSON.stringify(useVariants.value))
  }

  // Dev-only feature flag for Epic Multiplayer. Stored in localStorage, but
  // exposed as `isDevBuild() && stored` so a stale value can never enable it in
  // production builds.
  const epicMultiplayerStored = ref(localStorage.getItem(EPIC_MULTIPLAYER_KEY) === 'true')
  const epicMultiplayerEnabled = computed(() => isDevBuild() && epicMultiplayerStored.value)

  const customCardsEnabled = ref(localStorage.getItem(CUSTOM_CARDS_KEY) === 'true')
  // Keep modal-heavy interactions in the document flow for small screens. This
  // is global because it is a browser/UI preference, not game state.
  const inlineModals = ref(localStorage.getItem(INLINE_MODALS_KEY) === 'true')

  const locationView = ref<LocationView>(readLocationView(localStorage.getItem(LOCATION_VIEW_KEY)))

  function setLocationView(view: LocationView) {
    locationView.value = view
    localStorage.setItem(LOCATION_VIEW_KEY, view)
  }

  function setInlineModals(enabled: boolean) {
    inlineModals.value = enabled
    localStorage.setItem(INLINE_MODALS_KEY, String(enabled))
  }

  function setCustomCardsEnabled(enabled: boolean) {
    customCardsEnabled.value = enabled
    localStorage.setItem(CUSTOM_CARDS_KEY, String(enabled))
  }

  function setEpicMultiplayerEnabled(enabled: boolean) {
    epicMultiplayerStored.value = enabled
    localStorage.setItem(EPIC_MULTIPLAYER_KEY, String(enabled))
  }

  function toggleEpicMultiplayer() {
    setEpicMultiplayerEnabled(!epicMultiplayerStored.value)
  }

  const narration = ref(localStorage.getItem(NARRATION_KEY) !== 'false')
  const narrationLanguage = ref<NarrationLanguage>(
    readNarrationLanguage(
      localStorage.getItem(NARRATION_LANGUAGE_KEY),
      defaultNarrationLanguage(typeof navigator === 'undefined' ? [] : navigator.languages ?? []),
    ),
  )
  const narrationVolume = ref(readVolume(localStorage.getItem(NARRATION_VOLUME_KEY)))

  function setNarration(enabled: boolean) {
    narration.value = enabled
    localStorage.setItem(NARRATION_KEY, String(enabled))
  }

  function setNarrationLanguage(language: NarrationLanguage) {
    narrationLanguage.value = language
    localStorage.setItem(NARRATION_LANGUAGE_KEY, language)
  }

  function setNarrationVolume(volume: number) {
    narrationVolume.value = readVolume(String(volume))
    localStorage.setItem(NARRATION_VOLUME_KEY, String(narrationVolume.value))
  }

  // Off unless explicitly turned on (narration/TTS stays on by default).
  const music = ref(localStorage.getItem(MUSIC_KEY) === 'true')
  const musicVolume = ref(readVolume(localStorage.getItem(MUSIC_VOLUME_KEY), 0.45))

  function setMusic(enabled: boolean) {
    music.value = enabled
    localStorage.setItem(MUSIC_KEY, String(enabled))
  }

  function setMusicVolume(volume: number) {
    musicVolume.value = readVolume(String(volume), 0.45)
    localStorage.setItem(MUSIC_VOLUME_KEY, String(musicVolume.value))
  }

  // Size of the location buttons/plates and of the character figures on the map and
  // in the scene. Multipliers published as CSS variables on <html> so any
  // stylesheet can use `var(--location-scale)` / `var(--figure-scale)`.
  const locationScale = ref(readScale(localStorage.getItem(LOCATION_SCALE_KEY)))
  const figureScale = ref(readScale(localStorage.getItem(FIGURE_SCALE_KEY)))

  function applyScales() {
    if (typeof document === 'undefined') return
    document.documentElement.style.setProperty('--location-scale', String(locationScale.value))
    document.documentElement.style.setProperty('--figure-scale', String(figureScale.value))
  }
  applyScales()

  function setLocationScale(scale: number) {
    locationScale.value = readScale(String(scale))
    localStorage.setItem(LOCATION_SCALE_KEY, String(locationScale.value))
    applyScales()
  }

  function setFigureScale(scale: number) {
    figureScale.value = readScale(String(scale))
    localStorage.setItem(FIGURE_SCALE_KEY, String(figureScale.value))
    applyScales()
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

    drawSpotlightOverride.value = readDrawSpotlightMode(
      getGameLocalStorageItem(id, DRAW_SPOTLIGHT_SETTING),
    )
  }

  function toggleSplitView() {
    splitView.value = !splitView.value
    if (gameId.value) {
      setGameLocalStorageItem(gameId.value, 'splitView', String(splitView.value))
    }
  }

  /* Global player preference, and a per-game override that can defer to it --
   * the same tri-state shape as extraAnimations, for the same reason: "off for
   * this scenario" and "off everywhere" are different wishes. */
  const drawSpotlightGlobal = ref<DrawSpotlightMode>(
    readDrawSpotlightMode(localStorage.getItem(DRAW_SPOTLIGHT_KEY)) ?? 'upkeep',
  )
  const drawSpotlightOverride = ref<DrawSpotlightMode | null>(null)

  const drawSpotlight = computed<DrawSpotlightMode>(
    () => drawSpotlightOverride.value ?? drawSpotlightGlobal.value,
  )

  function setDrawSpotlightGlobal(mode: DrawSpotlightMode) {
    drawSpotlightGlobal.value = mode
    localStorage.setItem(DRAW_SPOTLIGHT_KEY, mode)
  }

  function setDrawSpotlightOverride(mode: DrawSpotlightMode | null) {
    drawSpotlightOverride.value = mode
    if (!gameId.value) return
    if (mode === null) {
      removeGameLocalStorageItem(gameId.value, DRAW_SPOTLIGHT_SETTING)
    } else {
      setGameLocalStorageItem(gameId.value, DRAW_SPOTLIGHT_SETTING, mode)
    }
  }

  // Off unless explicitly turned on.
  const hideInertCards = ref(localStorage.getItem(HIDE_INERT_CARDS_KEY) === 'true')

  function setHideInertCards(enabled: boolean) {
    hideInertCards.value = enabled
    localStorage.setItem(HIDE_INERT_CARDS_KEY, String(enabled))
  }

  const combatFx = ref(localStorage.getItem(COMBAT_FX_KEY) !== 'false')

  function setCombatFx(enabled: boolean) {
    combatFx.value = enabled
    localStorage.setItem(COMBAT_FX_KEY, String(enabled))
  }

  const autopilot = ref(localStorage.getItem(AUTOPILOT_KEY) !== 'false')

  function setAutopilot(enabled: boolean) {
    autopilot.value = enabled
    localStorage.setItem(AUTOPILOT_KEY, String(enabled))
  }

  const confirmCardPlays = ref(localStorage.getItem(CONFIRM_CARD_PLAYS_KEY) !== 'false')

  function setConfirmCardPlays(enabled: boolean) {
    confirmCardPlays.value = enabled
    localStorage.setItem(CONFIRM_CARD_PLAYS_KEY, String(enabled))
  }

  const warnAttacksOfOpportunity = ref(localStorage.getItem(WARN_ATTACKS_OF_OPPORTUNITY_KEY) !== 'false')

  function setWarnAttacksOfOpportunity(enabled: boolean) {
    warnAttacksOfOpportunity.value = enabled
    localStorage.setItem(WARN_ATTACKS_OF_OPPORTUNITY_KEY, String(enabled))
  }

  const showBonded = ref(false)

  function toggleShowBonded() {
    showBonded.value = !showBonded.value
  }
  return {
    useVariants,
    setUseVariants,
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
    hideInertCards,
    setHideInertCards,
    autopilot,
    setAutopilot,
    combatFx,
    setCombatFx,
    confirmCardPlays,
    setConfirmCardPlays,
    warnAttacksOfOpportunity,
    setWarnAttacksOfOpportunity,
    customCardsEnabled,
    setCustomCardsEnabled,
    inlineModals,
    setInlineModals,
    locationView,
    setLocationView,
    drawSpotlight,
    drawSpotlightGlobal,
    drawSpotlightOverride,
    setDrawSpotlightGlobal,
    setDrawSpotlightOverride,
    narration,
    setNarration,
    narrationLanguage,
    setNarrationLanguage,
    narrationVolume,
    setNarrationVolume,
    music,
    setMusic,
    musicVolume,
    setMusicVolume,
    locationScale,
    setLocationScale,
    figureScale,
    setFigureScale,
  }
})
