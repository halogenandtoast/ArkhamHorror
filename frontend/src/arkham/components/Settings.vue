<script lang="ts" setup>
import { ref, computed, watch, onMounted, onBeforeUnmount, nextTick } from 'vue';
import { type Game } from '@/arkham/types/Game'
import { useDebug } from '@/arkham/debug'
import { useI18n } from 'vue-i18n';
import { updateGameRaw } from '@/arkham/api'
import { gameLocalStorageKey, getGameLocalStorageItem, removeGameLocalStorageItem, setGameLocalStorageItem } from '@/arkham/localStorage'
import campaignJSON from '@/arkham/data/campaigns.json'
import { BugAntIcon } from '@heroicons/vue/20/solid'
import { useSettingsFocus } from '@/composable/settingsFocus'
import { useSettings, type DrawSpotlightMode } from '@/stores/settings'
import { keybindingProfile, setKeybindingProfile, type KeybindingProfile } from '@/arkham/keybindings'
import CardOptionsSettings from '@/arkham/components/CardOptionsSettings.vue'
import ScopedSetting from '@/arkham/components/ScopedSetting.vue'

const props = defineProps<{
  game: Game
  playerId: string
  solo: boolean
  showOtherPlayersHands: boolean
  closeSettings: () => void
}>()

const emit = defineEmits<{
  (e: 'update:showOtherPlayersHands', value: boolean): void
}>()

const showOtherHands = computed({
  get: () => props.showOtherPlayersHands,
  set: (v: boolean) => emit('update:showOtherPlayersHands', v),
})

const settings = useSettings()

// Global player preference, and a per-scenario override that can defer to it.
// Both live in the settings store; prefers-reduced-motion is folded in there
// too, which is why the resolved value can be off while both of these read on.
const extraAnimationsGlobal = computed<string>({
  get: () => (settings.extraAnimationsGlobal ? 'on' : 'off'),
  set: (value) => settings.setExtraAnimationsGlobal(value === 'on'),
})

const extraAnimationsOverride = computed<string | null>({
  get: () => (settings.extraAnimationsOverride === null ? null : settings.extraAnimationsOverride ? 'on' : 'off'),
  set: (value) => settings.setExtraAnimationsOverride(value === null ? null : value === 'on'),
})

const hideInertCards = computed({
  get: () => settings.hideInertCards,
  set: (value: boolean) => settings.setHideInertCards(value),
})

// Same global / per-game pairing as extraAnimations above, tri-state for the
// same reason: "not in this scenario" and "not ever" are different wishes.
const drawSpotlightGlobal = computed<string>({
  get: () => settings.drawSpotlightGlobal,
  set: (value) => settings.setDrawSpotlightGlobal(value as DrawSpotlightMode),
})

const drawSpotlightOverrideValue = computed<string | null>({
  get: () => settings.drawSpotlightOverride,
  set: (value) => settings.setDrawSpotlightOverride(value as DrawSpotlightMode | null),
})

const drawSpotlightOptions = computed(() => [
  { value: 'off', label: t('gameBar.settings.drawSpotlightOff') },
  { value: 'upkeep', label: t('gameBar.settings.drawSpotlightUpkeep') },
  { value: 'every', label: t('gameBar.settings.drawSpotlightEvery') },
])

/* ScopedSetting speaks in option strings so one component can serve a tri-state
 * and a boolean alike; extraAnimations is stored as a boolean, so it is adapted
 * here rather than widening the store. */
const onOffOptions = computed(() => [
  { value: 'on', label: t('On') },
  { value: 'off', label: t('Off') },
])

const soundsDisabled = ref(localStorage.getItem('arkhamSoundsDisabled') === 'true')

const inlineModals = computed({
  get: () => settings.inlineModals,
  set: (value: boolean) => settings.setInlineModals(value),
})

const keybindings = computed<KeybindingProfile>({
  get: () => keybindingProfile.value,
  set: (value) => setKeybindingProfile(value),
})

watch(soundsDisabled, (value) => {
  localStorage.setItem('arkhamSoundsDisabled', value ? 'true' : 'false')
  window.dispatchEvent(new CustomEvent('arkham-setting-change', {
    detail: { key: 'arkhamSoundsDisabled', value: value ? 'true' : 'false' }
  }))
})

const canShowOtherHands = computed(() => !props.solo && props.game.playerCount > 1)

const headerStyle = computed(() => {
  const cls = investigator.value?.class?.toLowerCase()
  if (!cls) return {}
  return { '--class-color': `var(--${cls}-extra-dark, var(--${cls}, var(--background-dark)))` }
})

const { t, te } = useI18n()
const debug = useDebug()
const investigator = computed(() => {
  return Object.values(props.game.investigators).find(i => i.playerId === props.playerId)
})

const skipTriggers = ref(investigator.value?.settings.globalSettings.ignoreUnrelatedSkillTestTriggers ?? false)
const asIfRuling = ref(props.game.settings.settingsAsIfRuling)

watch(() => props.playerId, () => {
  skipTriggers.value = investigator.value?.settings.globalSettings.ignoreUnrelatedSkillTestTriggers ?? false
})
const ultimatumsAndBoonsEnabled = ref(props.game.settings.settingsUltimatumsAndBoonsEnabled)
const hasUltimatumsAndBoons = computed(() => props.game.settings.settingsUltimatumsAndBoons.length > 0)
const cosmicEmissaryAnimationKey = computed(() => gameLocalStorageKey(props.game.id, 'enableCosmicEmissaryAnimation'))
const showCosmicEmissaryAnimationSetting = computed(() => props.game.scenario?.id === 'c10651')
const enableCosmicEmissaryAnimation = ref(
  getGameLocalStorageItem(props.game.id, 'enableCosmicEmissaryAnimation') === null
    ? getGameLocalStorageItem(props.game.id, 'disableCosmicEmissaryAnimation') !== 'true'
    : getGameLocalStorageItem(props.game.id, 'enableCosmicEmissaryAnimation') !== 'false'
)

watch(() => skipTriggers.value, (value) => {
  const currentValue = investigator.value?.settings.globalSettings.ignoreUnrelatedSkillTestTriggers ?? false
  if (investigator.value && value !== currentValue) {
    debug.send(props.game.id,
      ({ tag: 'UpdateGlobalSetting'
       , contents: [investigator.value.id, {tag: "SetIgnoreUnrelatedSkillTestTriggers", contents: value}]
       }
      )
    )
  }
})

watch(() => props.game.settings.settingsAsIfRuling, (value) => {
  asIfRuling.value = value
})

watch(asIfRuling, async (value) => {
  if (value === props.game.settings.settingsAsIfRuling) return
  await updateGameRaw(props.game.id, { tag: 'SetAsIfRuling', contents: value })
})

watch(() => props.game.settings.settingsUltimatumsAndBoonsEnabled, (value) => {
  ultimatumsAndBoonsEnabled.value = value
})

watch(ultimatumsAndBoonsEnabled, async (value) => {
  if (value === props.game.settings.settingsUltimatumsAndBoonsEnabled) return
  await updateGameRaw(props.game.id, { tag: 'SetUltimatumsAndBoonsEnabled', contents: value })
})

watch(enableCosmicEmissaryAnimation, (value) => {
  setGameLocalStorageItem(props.game.id, 'enableCosmicEmissaryAnimation', value ? 'true' : 'false')
  removeGameLocalStorageItem(props.game.id, 'disableCosmicEmissaryAnimation')
  window.dispatchEvent(new CustomEvent('arkham-setting-change', {
    detail: { key: cosmicEmissaryAnimationKey.value, value: value ? 'true' : 'false' }
  }))
})

type RecommendedToggle = {
  type: 'toggle'
  default?: boolean
  icon?: 'bug-ant'
  option: { tag: string }
}

type CampaignEntry = { id: string, recommendedOptions?: RecommendedToggle[], returnTo?: { id: string } }

const campaignId = computed(() => props.game.campaign?.id ?? null)

const recommendedToggles = computed<RecommendedToggle[]>(() => {
  if (!campaignId.value) return []
  const entries = campaignJSON as CampaignEntry[]
  const c =
    entries.find((c) => c.id === campaignId.value) ??
    entries.find((c) => c.returnTo?.id === campaignId.value)
  const opts = c?.recommendedOptions ?? []
  return opts.filter((o) => o.type === 'toggle' && o.option?.tag)
})

const activeOptionTags = computed<string[]>(() =>
  props.game.campaign?.log?.options?.map((o) => o.tag) ?? []
)

const isOptionEnabled = (o: RecommendedToggle) => activeOptionTags.value.includes(o.option.tag)

const optionTitle = (tag: string) =>
  te(`create.recommendedOption.${tag}.title`) ? t(`create.recommendedOption.${tag}.title`) : tag

const optionDescription = (tag: string) =>
  te(`create.recommendedOption.${tag}.description`) ? t(`create.recommendedOption.${tag}.description`) : ''

const optionSaving = ref(false)

const setOptionEnabled = async (o: RecommendedToggle, enabled: boolean) => {
  if (optionSaving.value) return
  if (isOptionEnabled(o) === enabled) return
  optionSaving.value = true
  try {
    await updateGameRaw(props.game.id, {
      tag: enabled ? 'HandleOption' : 'RemoveOption',
      contents: { tag: o.option.tag },
    })
  } finally {
    optionSaving.value = false
  }
}

/* The panel was one flat column of a dozen rows spanning five different scopes
 * -- per-investigator, per-browser, per-game, per-scenario, shared with everyone
 * at the table -- all looking identical. Tabs group by what a setting affects;
 * the scope badge on each row says who it affects. The tablist wiring mirrors
 * components/SettingsForm.vue, the one place the pattern already existed.
 */
const TABS = ['table', 'pacing', 'soundMotion', 'cards', 'shared'] as const
type TabId = (typeof TABS)[number]
const activeTab = ref<TabId>('table')

/* Which tab each deep-linkable row lives on. `focusSetting` (SkillTest.vue asks
 * for 'skipTriggers') would otherwise scroll to a row on a panel that is not
 * showing and silently do nothing. */
const settingTabs: Record<string, TabId> = {
  skipTriggers: 'pacing',
  drawSpotlight: 'pacing',
}

function navigateTabs(event: KeyboardEvent) {
  const index = TABS.indexOf(activeTab.value)
  const next = {
    ArrowLeft: (index - 1 + TABS.length) % TABS.length,
    ArrowRight: (index + 1) % TABS.length,
    Home: 0,
    End: TABS.length - 1,
  }[event.key]
  if (next === undefined) return
  event.preventDefault()
  activeTab.value = TABS[next]
}

const settingRefs: Record<string, HTMLElement | null> = {}
/* A `ref` on a component yields its instance, not an element, and `focusOn`
 * needs something it can scroll to. */
const setSettingRef = (id: string) => (el: unknown) => {
  const node = el && typeof el === 'object' && '$el' in el ? (el as { $el: unknown }).$el : el
  settingRefs[id] = (node as HTMLElement | null) ?? null
}

const { focusedSettingId, clearFocus } = useSettingsFocus()
const highlightedSetting = ref<string | null>(null)
let highlightTimeout: ReturnType<typeof setTimeout> | null = null

const focusOn = async (id: string) => {
  // Show the row's tab first: a panel kept in the DOM by `v-show` still has no
  // layout while hidden, so scrollIntoView on it goes nowhere.
  const tab = settingTabs[id]
  if (tab) activeTab.value = tab
  await nextTick()
  const el = settingRefs[id]
  if (!el) return
  el.scrollIntoView({ behavior: 'smooth', block: 'center' })
  highlightedSetting.value = id
  if (highlightTimeout) clearTimeout(highlightTimeout)
  highlightTimeout = setTimeout(() => {
    highlightedSetting.value = null
  }, 2400)
}

watch(focusedSettingId, (id) => {
  if (id) {
    focusOn(id)
    clearFocus()
  }
})

onMounted(() => {
  if (focusedSettingId.value) {
    const id = focusedSettingId.value
    focusOn(id)
    clearFocus()
  }
})

onBeforeUnmount(() => {
  if (highlightTimeout) clearTimeout(highlightTimeout)
})
</script>
<template>
  <div class="settings">
    <div class="settings-header" :style="headerStyle">
      <h2 class="settings-title">{{$t('gameBar.viewSettingTitle', {investigator: investigator?.name.title ?? ''})}}</h2>
    </div>

    <div class="settings-tabs" role="tablist" @keydown="navigateTabs">
      <button
        v-for="tab in TABS"
        :key="tab"
        type="button"
        class="settings-tab"
        role="tab"
        :id="`settings-tab-${tab}`"
        :aria-controls="`settings-panel-${tab}`"
        :aria-selected="activeTab === tab"
        :tabindex="activeTab === tab ? 0 : -1"
        :class="{ 'settings-tab--active': activeTab === tab }"
        @click="activeTab = tab"
      >{{ $t(`gameBar.settings.tabs.${tab}`) }}</button>
    </div>

    <div class="settings-body">
      <!-- What is on the table and how much of it you can see. -->
      <section
        class="settings-section"
        role="tabpanel"
        id="settings-panel-table"
        aria-labelledby="settings-tab-table"
        v-show="activeTab === 'table'"
      >
        <div class="toggle-list">
          <div class="toggle-row" v-if="canShowOtherHands">
            <div class="toggle-text">
              <div class="toggle-name">{{$t('gameBar.viewSettingShowOtherPlayersHandsTitle')}}</div>
              <div class="toggle-desc">{{$t('gameBar.viewSettingShowOtherPlayersHands')}}</div>
              <div class="toggle-scope">{{$t('gameBar.settings.scope.game')}}</div>
            </div>
            <div class="segmented toggle-control">
              <input type="radio" id="opt-showHands-on" name="opt-showHands" :checked="showOtherHands" @change="showOtherHands = true" />
              <label for="opt-showHands-on">{{ $t('On') }}</label>
              <input type="radio" id="opt-showHands-off" name="opt-showHands" :checked="!showOtherHands" @change="showOtherHands = false" />
              <label for="opt-showHands-off">{{ $t('Off') }}</label>
            </div>
          </div>

          <div class="toggle-row">
            <div class="toggle-text">
              <div class="toggle-name">{{$t('gameBar.settings.hideInertCardsTitle')}}</div>
              <div class="toggle-desc">{{$t('gameBar.settings.hideInertCards')}}</div>
              <div class="toggle-scope">{{$t('gameBar.settings.scope.browser')}}</div>
            </div>
            <div class="segmented toggle-control">
              <input type="radio" id="opt-hideInertCards-on" name="opt-hideInertCards" :checked="hideInertCards" @change="hideInertCards = true" />
              <label for="opt-hideInertCards-on">{{ $t('On') }}</label>
              <input type="radio" id="opt-hideInertCards-off" name="opt-hideInertCards" :checked="!hideInertCards" @change="hideInertCards = false" />
              <label for="opt-hideInertCards-off">{{ $t('Off') }}</label>
            </div>
          </div>

          <div class="toggle-row">
            <div class="toggle-text">
              <div class="toggle-name">{{$t('gameBar.settings.keybindingsTitle')}}</div>
              <div class="toggle-desc">{{$t('gameBar.settings.keybindings')}}</div>
              <div class="toggle-scope">{{$t('gameBar.settings.scope.browser')}}</div>
            </div>
            <div class="segmented toggle-control">
              <input type="radio" id="opt-keybindings-default" name="opt-keybindings" :checked="keybindings === 'default'" @change="keybindings = 'default'" />
              <label for="opt-keybindings-default">{{ $t('gameBar.settings.keybindingProfile.default') }}</label>
              <input type="radio" id="opt-keybindings-tts" name="opt-keybindings" :checked="keybindings === 'tts'" @change="keybindings = 'tts'" />
              <label for="opt-keybindings-tts">{{ $t('gameBar.settings.keybindingProfile.tts') }}</label>
            </div>
          </div>

          <div class="toggle-row">
            <div class="toggle-text">
              <div class="toggle-name">{{$t('gameBar.settings.inlineModalsTitle')}}</div>
              <div class="toggle-desc">{{$t('gameBar.settings.inlineModals')}}</div>
              <div class="toggle-scope">{{$t('gameBar.settings.scope.browser')}}</div>
            </div>
            <div class="segmented toggle-control">
              <input type="radio" id="opt-inlineModals-on" name="opt-inlineModals" :checked="inlineModals" @change="inlineModals = true" />
              <label for="opt-inlineModals-on">{{ $t('On') }}</label>
              <input type="radio" id="opt-inlineModals-off" name="opt-inlineModals" :checked="!inlineModals" @change="inlineModals = false" />
              <label for="opt-inlineModals-off">{{ $t('Off') }}</label>
            </div>
          </div>
        </div>
      </section>

      <!-- When the game stops for you, and when it gets out of your way. -->
      <section
        class="settings-section"
        role="tabpanel"
        id="settings-panel-pacing"
        aria-labelledby="settings-tab-pacing"
        v-show="activeTab === 'pacing'"
      >
        <div class="toggle-list">
          <div class="toggle-row" :ref="setSettingRef('skipTriggers')" :class="{ 'toggle-row--highlighted': highlightedSetting === 'skipTriggers' }">
            <div class="toggle-text">
              <div class="toggle-name">{{$t('gameBar.viewSettingSkipTriggersTitle')}}</div>
              <div class="toggle-desc">{{$t('gameBar.viewSettingSkipTriggers')}}</div>
              <div class="toggle-scope">{{$t('gameBar.settings.scope.you')}}</div>
            </div>
            <div class="segmented toggle-control">
              <input type="radio" id="opt-skipTriggers-on" name="opt-skipTriggers" :checked="skipTriggers" @change="skipTriggers = true" />
              <label for="opt-skipTriggers-on">{{ $t('On') }}</label>
              <input type="radio" id="opt-skipTriggers-off" name="opt-skipTriggers" :checked="!skipTriggers" @change="skipTriggers = false" />
              <label for="opt-skipTriggers-off">{{ $t('Off') }}</label>
            </div>
          </div>

          <ScopedSetting
            :ref="setSettingRef('drawSpotlight')"
            settingKey="drawSpotlight"
            :name="$t('gameBar.settings.drawSpotlightTitle')"
            :description="$t('gameBar.settings.drawSpotlight')"
            :options="drawSpotlightOptions"
            :global="drawSpotlightGlobal"
            :override="drawSpotlightOverrideValue"
            :highlighted="highlightedSetting === 'drawSpotlight'"
            @update:global="drawSpotlightGlobal = $event"
            @update:override="drawSpotlightOverrideValue = $event"
          />
        </div>
      </section>

      <!-- Noise and movement. Nothing here carries information you need to play. -->
      <section
        class="settings-section"
        role="tabpanel"
        id="settings-panel-soundMotion"
        aria-labelledby="settings-tab-soundMotion"
        v-show="activeTab === 'soundMotion'"
      >
        <div class="toggle-list">
          <div class="toggle-row">
            <div class="toggle-text">
              <div class="toggle-name">{{$t('gameBar.settings.soundsTitle')}}</div>
              <div class="toggle-desc">{{$t('gameBar.settings.sounds')}}</div>
              <div class="toggle-scope">{{$t('gameBar.settings.scope.browser')}}</div>
            </div>
            <div class="segmented toggle-control">
              <input type="radio" id="opt-sounds-on" name="opt-sounds" :checked="!soundsDisabled" @change="soundsDisabled = false" />
              <label for="opt-sounds-on">{{ $t('On') }}</label>
              <input type="radio" id="opt-sounds-off" name="opt-sounds" :checked="soundsDisabled" @change="soundsDisabled = true" />
              <label for="opt-sounds-off">{{ $t('Off') }}</label>
            </div>
          </div>

          <ScopedSetting
            settingKey="extraAnimations"
            :name="$t('gameBar.settings.extraAnimationsTitle')"
            :description="$t('gameBar.settings.extraAnimations')"
            :note="settings.prefersReducedMotion ? $t('gameBar.settings.extraAnimationsReducedMotion') : undefined"
            :options="onOffOptions"
            :global="extraAnimationsGlobal"
            :override="extraAnimationsOverride"
            @update:global="extraAnimationsGlobal = $event"
            @update:override="extraAnimationsOverride = $event"
          />

          <div class="toggle-row" v-if="showCosmicEmissaryAnimationSetting">
            <div class="toggle-text">
              <div class="toggle-name">{{$t('gameBar.settings.cosmicEmissaryTitle')}}</div>
              <div class="toggle-desc">{{$t('gameBar.settings.cosmicEmissary')}}</div>
              <div class="toggle-scope">{{$t('gameBar.settings.scope.game')}}</div>
            </div>
            <div class="segmented toggle-control">
              <input type="radio" id="opt-cosmicEmissaryAnimation-on" name="opt-cosmicEmissaryAnimation" :checked="enableCosmicEmissaryAnimation" @change="enableCosmicEmissaryAnimation = true" />
              <label for="opt-cosmicEmissaryAnimation-on">{{ $t('On') }}</label>
              <input type="radio" id="opt-cosmicEmissaryAnimation-off" name="opt-cosmicEmissaryAnimation" :checked="!enableCosmicEmissaryAnimation" @change="enableCosmicEmissaryAnimation = false" />
              <label for="opt-cosmicEmissaryAnimation-off">{{ $t('Off') }}</label>
            </div>
          </div>
        </div>
      </section>

      <div
        role="tabpanel"
        id="settings-panel-cards"
        aria-labelledby="settings-tab-cards"
        v-show="activeTab === 'cards'"
      >
        <CardOptionsSettings :game="game" :playerId="playerId" />
      </div>

      <!-- Changing any of these changes the game for everyone at the table. -->
      <section
        class="settings-section"
        role="tabpanel"
        id="settings-panel-shared"
        aria-labelledby="settings-tab-shared"
        v-show="activeTab === 'shared'"
      >
        <div class="toggle-list">
          <div class="toggle-row">
            <div class="toggle-text">
              <div class="toggle-name">{{$t('gameBar.settings.asIfRulingTitle')}}</div>
              <div class="toggle-desc">{{$t('gameBar.settings.asIfRuling')}}</div>
              <div class="toggle-scope">{{$t('gameBar.settings.scope.everyone')}}</div>
            </div>
            <div class="segmented toggle-control">
              <input type="radio" id="opt-asIfRuling-chapter1" name="opt-asIfRuling" :checked="asIfRuling === 'chapter1'" @change="asIfRuling = 'chapter1'" />
              <label for="opt-asIfRuling-chapter1">Chapter 1</label>
              <input type="radio" id="opt-asIfRuling-chapter2" name="opt-asIfRuling" :checked="asIfRuling === 'chapter2'" @change="asIfRuling = 'chapter2'" />
              <label for="opt-asIfRuling-chapter2">Chapter 2</label>
            </div>
          </div>

          <div class="toggle-row" v-if="hasUltimatumsAndBoons">
            <div class="toggle-text">
              <div class="toggle-name">{{ $t('ultimatumsAndBoons.settingsToggleTitle') }}</div>
              <div class="toggle-desc">{{ $t('ultimatumsAndBoons.settingsToggleDescription') }}</div>
              <div class="toggle-scope">{{$t('gameBar.settings.scope.everyone')}}</div>
            </div>
            <div class="segmented toggle-control">
              <input type="radio" id="opt-ultimatumsAndBoons-on" name="opt-ultimatumsAndBoons" :checked="ultimatumsAndBoonsEnabled" @change="ultimatumsAndBoonsEnabled = true" />
              <label for="opt-ultimatumsAndBoons-on">{{ $t('On') }}</label>
              <input type="radio" id="opt-ultimatumsAndBoons-off" name="opt-ultimatumsAndBoons" :checked="!ultimatumsAndBoonsEnabled" @change="ultimatumsAndBoonsEnabled = false" />
              <label for="opt-ultimatumsAndBoons-off">{{ $t('Off') }}</label>
            </div>
          </div>

          <div class="toggle-row" v-for="o in recommendedToggles" :key="o.option.tag">
            <div class="toggle-text">
              <div class="toggle-name">
                <BugAntIcon v-if="o.icon === 'bug-ant'" class="toggle-icon" aria-hidden="true" />
                {{ optionTitle(o.option.tag) }}
              </div>
              <div class="toggle-desc" v-if="optionDescription(o.option.tag)">
                {{ optionDescription(o.option.tag) }}
              </div>
              <div class="toggle-scope">{{$t('gameBar.settings.scope.everyone')}}</div>
            </div>
            <div class="segmented toggle-control">
              <input
                type="radio"
                :id="`opt-${o.option.tag}-on`"
                :name="`opt-${o.option.tag}`"
                :checked="isOptionEnabled(o)"
                :disabled="optionSaving"
                @change="setOptionEnabled(o, true)"
              />
              <label :for="`opt-${o.option.tag}-on`">{{ $t('On') ?? 'On' }}</label>

              <input
                type="radio"
                :id="`opt-${o.option.tag}-off`"
                :name="`opt-${o.option.tag}`"
                :checked="!isOptionEnabled(o)"
                :disabled="optionSaving"
                @change="setOptionEnabled(o, false)"
              />
              <label :for="`opt-${o.option.tag}-off`">{{ $t('Off') ?? 'Off' }}</label>
            </div>
          </div>
        </div>
      </section>
    </div>

    <button class="settings-footer" @click="closeSettings">{{$t('close')}}</button>
  </div>
</template>

<style scoped>
.settings {
  display: flex;
  flex-direction: column;
  width: 100%;
  max-height: 75vh;
  background: var(--background);
  color: var(--text);
}

.settings-header {
  flex-shrink: 0;
  padding: 8px 16px;
  background: var(--class-color, var(--background-dark));
  border-bottom: 1px solid var(--box-border);
}

.settings-title {
  margin: 0;
  font-family: Teutonic, serif;
  font-size: 20px;
  color: var(--text);
  text-transform: none;
}

.settings-tabs {
  flex-shrink: 0;
  display: flex;
  gap: 2px;
  padding: 0 12px;
  background: var(--background-dark);
  border-bottom: 1px solid var(--box-border);
  overflow-x: auto;
}

.settings-tab {
  flex: 0 0 auto;
  border: 0;
  border-bottom: 2px solid transparent;
  /* The global button radius would curl the active underline up at both ends. */
  border-radius: 0;
  background: none;
  color: var(--background-light);
  font-family: Teutonic, serif;
  font-size: 13px;
  letter-spacing: 0.08em;
  text-transform: uppercase;
  padding: 10px 12px;
  cursor: pointer;
}

.settings-tab:hover {
  color: var(--text);
}

.settings-tab--active {
  color: var(--text);
  border-bottom-color: var(--button-1);
}

/* The row's scope: who a change reaches. Muted and small -- it answers a
   question you only ask occasionally, and must never compete with the setting's
   own name. */
.toggle-scope {
  margin-top: 6px;
  font-size: 10px;
  letter-spacing: 0.08em;
  text-transform: uppercase;
  color: var(--background-light);
  opacity: 0.75;
}

.toggle-scope::before {
  content: '·';
  margin-right: 4px;
}

/* Three- and four-way controls need more room than the On/Off pair. */
.segmented-wide {
  min-width: 260px;
}

.settings-body {
  flex: 1 1 auto;
  min-height: 0;
  overflow-y: auto;
  padding: 18px 24px;
  display: flex;
  flex-direction: column;
  gap: 20px;
}

.settings-section {
  display: flex;
  flex-direction: column;
}

.toggle-list {
  display: flex;
  flex-direction: column;
  gap: 6px;
}

.toggle-row {
  display: grid;
  grid-template-columns: 1fr auto;
  gap: 16px;
  align-items: center;
  padding: 10px 14px;
  background: var(--box-background);
  border: 1px solid var(--box-border);
  border-radius: 5px;
}

.toggle-row:hover {
  background: var(--background-mid);
}

.toggle-row--highlighted {
  border-color: var(--select);
  box-shadow: 0 0 0 1px var(--select), 0 0 12px rgba(255, 0, 255, 0.6);
  animation: settingsFocusPulse 1.2s ease-out 2;
}

@keyframes settingsFocusPulse {
  0% { box-shadow: 0 0 0 1px var(--select), 0 0 4px rgba(255, 0, 255, 0.3); }
  50% { box-shadow: 0 0 0 1px var(--select), 0 0 18px rgba(255, 0, 255, 0.85); }
  100% { box-shadow: 0 0 0 1px var(--select), 0 0 4px rgba(255, 0, 255, 0.3); }
}

.toggle-text {
  min-width: 0;
}

.toggle-name {
  font-size: 14px;
  font-weight: 500;
  color: var(--text);
  display: flex;
  align-items: center;
}

.toggle-desc {
  margin-top: 4px;
  font-size: 12px;
  line-height: 1.4;
  color: var(--background-light);
}

/* Grow past 150px when the labels need it ("Default", "Chapter 1", …) rather
   than letting the segments clip or wrap. */
.toggle-control {
  min-width: 150px;
  flex-shrink: 0;
  justify-self: end;
}

.toggle-icon {
  width: 1em;
  height: 1em;
  vertical-align: -0.15em;
  margin-right: 0.45em;
}

.segmented {
  display: grid;
  grid-auto-flow: column;
  grid-auto-columns: 1fr;
  border-radius: 5px;
  background: var(--background-dark);
  border: 1px solid var(--box-border);
  padding: 2px;
  gap: 2px;
}

.segmented input[type='radio'] {
  display: none;
}

.segmented label {
  display: flex;
  align-items: center;
  justify-content: center;
  padding: 6px 8px;
  text-transform: uppercase;
  letter-spacing: 0.06em;
  font-size: 11px;
  font-weight: 600;
  white-space: nowrap;
  user-select: none;
  cursor: pointer;
  border-radius: 3px;
  color: var(--background-light);
  margin: 0;
}

.segmented label:hover {
  color: var(--text);
}

.segmented input[type='radio']:checked + label {
  background: var(--button-1);
  color: var(--text);
}

.segmented input[type='radio']:checked + label:hover {
  background: var(--button-1-highlight);
}

.segmented input[type='radio']:disabled + label {
  cursor: not-allowed;
  opacity: 0.5;
}

.settings-footer {
  flex-shrink: 0;
  width: 100%;
  padding: 8px 16px;
  border: none;
  border-top: 1px solid var(--box-border);
  background: var(--button-2);
  color: var(--text);
  font-family: Teutonic, serif;
  font-size: 14px;
  letter-spacing: 0.08em;
  text-transform: uppercase;
  cursor: pointer;
  text-align: center;
}

.settings-footer:hover {
  background: var(--button-2-highlight);
}

@media (max-width: 700px) {
  .settings-header,
  .settings-footer {
    padding-left: 16px;
    padding-right: 16px;
  }
  .settings-body {
    padding: 14px 16px;
  }
  .toggle-row {
    grid-template-columns: 1fr;
    gap: 10px;
  }
  .toggle-control {
    width: 100%;
  }
}
</style>
