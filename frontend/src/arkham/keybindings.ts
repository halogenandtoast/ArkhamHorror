import { computed, ref } from 'vue'

/* Every keyboard shortcut the game board owns, named instead of spelled.
 *
 * The literal keys used to be typed out in three places -- the `handleKeyPress`
 * comparisons, the `shortcut` on a menu entry, and the `<kbd>` rows in the
 * shortcuts modal -- so any remap had to be made three times and the modal
 * quietly lied whenever one was missed. Everything now resolves through a
 * profile here, which is also what makes a second profile possible at all.
 */
export type ShortcutAction =
  | 'continue'
  | 'endTurn'
  | 'draw'
  | 'takeResources'
  | 'undo'
  | 'undoChord'
  | 'toggleDebug'
  | 'showShortcuts'
  | 'viewBonded'
  | 'viewChaosBag'
  | 'viewSettings'
  | 'viewHistory'
  | 'rotateLayout'
  | 'rotateLayoutCcw'
  | 'showOutOfPlay'

/* Shortcuts that act on the card under the cursor instead of on the board.
 *
 * Separate from ShortcutAction because they are the only bindings allowed to be
 * absent: TTS spawns tokens onto whatever the pointer is over, this app has no
 * such player action at all (the engine owns every token), so outside the TTS
 * profile most of these are simply not bound.
 *
 * Whoever handles them must try them BEFORE the board-level ones and only when a
 * card is really under the cursor. That ordering is the whole trick behind SCE's
 * numpad 9, which adds a resource to the card you are pointing at and spawns a
 * loose one when you are pointing at nothing.
 */
export type HoverAction =
  | 'exhaustHovered'
  | 'placeDamage'
  | 'placeHorror'
  | 'placeDoom'
  | 'placeClue'
  | 'placeResource'

/* `code` matches a physical key (the only way to tell Numpad 9 from 9); `key`
 * matches the produced character and stays case-sensitive, because `S` and `s`
 * are different shortcuts here. `display` is what the modal and the menus show. */
export type Binding = {
  key?: string
  code?: string
  ctrl?: boolean
  display: string[]
}

export type KeybindingProfile = 'default' | 'tts'
export const KEYBINDING_PROFILES: KeybindingProfile[] = ['default', 'tts']

const DEFAULT_BINDINGS: Record<ShortcutAction, Binding> = {
  continue: { key: ' ', display: ['space'] },
  endTurn: { key: 'e', display: ['e'] },
  draw: { key: 'd', display: ['d'] },
  takeResources: { key: 'r', display: ['r'] },
  undo: { key: 'u', display: ['u'] },
  undoChord: { key: 'U', display: ['U'] },
  toggleDebug: { key: 'D', display: ['D'] },
  showShortcuts: { key: '?', display: ['?'] },
  viewBonded: { key: 'b', display: ['b'] },
  viewChaosBag: { key: 'c', display: ['c'] },
  viewSettings: { key: 'S', display: ['S'] },
  viewHistory: { key: 'H', display: ['H'] },
  rotateLayout: { key: '>', display: ['>'] },
  rotateLayoutCcw: { key: '<', display: ['<'] },
  showOutOfPlay: { key: 'o', display: ['o'] },
}

/* Tabletop Simulator + SCE.
 *
 * Only part of this is copied rather than chosen, and it is worth knowing which.
 * TTS's own object keys are fixed: Ctrl+Z undoes, Q and E rotate. SCE's scripting
 * buttons are fixed too -- numpad 1-9 spawn a token, 9 being a resource, which is
 * the one token spawn this app has an equivalent action for.
 *
 * SCE's named hotkeys (Upkeep, Take clue from location, View Playermat, ...) ship
 * with NO key attached: TTS makes the player bind them under Options > Game Keys,
 * so there is nothing to copy. `endTurn` on `u` is therefore a choice, not a
 * quote -- Upkeep is the SCE hotkey that ends a round, and `u` is free here only
 * because undo moved onto Ctrl+Z.
 */
const TTS_OVERRIDES: Partial<Record<ShortcutAction, Binding>> = {
  undo: { key: 'z', ctrl: true, display: ['Ctrl', 'Z'] },
  endTurn: { key: 'u', display: ['u'] },
  takeResources: { code: 'Numpad9', display: ['Num 9'] },
  rotateLayout: { key: 'e', display: ['e'] },
  rotateLayoutCcw: { key: 'q', display: ['q'] },
}

const TTS_BINDINGS: Record<ShortcutAction, Binding> = { ...DEFAULT_BINDINGS, ...TTS_OVERRIDES }

type HoverBindings = Partial<Record<HoverAction, Binding>>

/* Rotating a card 90 degrees is how an asset is exhausted on a real table, and in
 * TTS that is Q/E on the hovered object -- which is why `e` is already the key that
 * exhausts the asset under the cursor here. It keeps that job in both profiles;
 * `rotateLayout` picks the same press up when no card is under the cursor. */
const DEFAULT_HOVER: HoverBindings = {
  exhaustHovered: { key: 'e', display: ['e'] },
}

/* SCE's scripting buttons, which are fixed: numpad 4 damage, 6 horror, 7 doom,
 * 8 clue, 9 resource, all onto the object under the pointer. Numpad 1-3 and 5
 * (action tokens, resource counters, path) have no counterpart here. */
const TTS_HOVER: HoverBindings = {
  ...DEFAULT_HOVER,
  placeDamage: { code: 'Numpad4', display: ['Num 4'] },
  placeHorror: { code: 'Numpad6', display: ['Num 6'] },
  placeDoom: { code: 'Numpad7', display: ['Num 7'] },
  placeClue: { code: 'Numpad8', display: ['Num 8'] },
  placeResource: { code: 'Numpad9', display: ['Num 9'] },
}

const PROFILES: Record<KeybindingProfile, Record<ShortcutAction, Binding>> = {
  default: DEFAULT_BINDINGS,
  tts: TTS_BINDINGS,
}

const HOVER_PROFILES: Record<KeybindingProfile, HoverBindings> = {
  default: DEFAULT_HOVER,
  tts: TTS_HOVER,
}

export function matchesBinding(binding: Binding, event: KeyboardEvent): boolean {
  if (!!binding.ctrl !== (event.ctrlKey || event.metaKey)) return false
  if (binding.code) return event.code === binding.code
  return event.key === binding.key
}

/* Which profile the board answers to. A plain module ref rather than a field on
 * the settings store, because every binding lookup in this file would otherwise
 * import that store and the store imports this one back for the profile list.
 *
 * Never trust the stored string: an unknown profile would index PROFILES with
 * undefined and take every shortcut down with it.
 */
const PROFILE_KEY = 'arkhamKeybindingProfile'

// Read at module load, so unlike the stores it can run before a window exists.
const stored = typeof localStorage === 'undefined' ? null : localStorage.getItem(PROFILE_KEY)

export const keybindingProfile = ref<KeybindingProfile>(
  KEYBINDING_PROFILES.find((p) => p === stored) ?? 'default',
)

export function setKeybindingProfile(profile: KeybindingProfile) {
  keybindingProfile.value = profile
  if (typeof localStorage !== 'undefined') localStorage.setItem(PROFILE_KEY, profile)
}

export function useKeybindings() {
  const bindings = computed(() => PROFILES[keybindingProfile.value])
  const hoverBindings = computed(() => HOVER_PROFILES[keybindingProfile.value])

  const is = (action: ShortcutAction, event: KeyboardEvent) =>
    matchesBinding(bindings.value[action], event)

  const isHover = (action: HoverAction, event: KeyboardEvent) => {
    const binding = hoverBindings.value[action]
    return binding ? matchesBinding(binding, event) : false
  }

  /* Ctrl/Cmd chords belong to the browser unless a profile asked for one, so the
   * key handler needs to ask about the whole profile, not one action. */
  const anyBindingMatches = (event: KeyboardEvent) =>
    [...Object.values(bindings.value), ...Object.values(hoverBindings.value)].some((b) =>
      matchesBinding(b, event),
    )

  const keys = (action: ShortcutAction) => bindings.value[action].display

  /* The bound hover actions, in declaration order, so the shortcuts panel can list
   * exactly what the active profile answers to and nothing else. */
  const boundHoverActions = computed(
    () => Object.entries(hoverBindings.value) as [HoverAction, Binding][],
  )

  return {
    bindings,
    hoverBindings,
    boundHoverActions,
    is,
    isHover,
    anyBindingMatches,
    keys,
    profile: keybindingProfile,
  }
}
