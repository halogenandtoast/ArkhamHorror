<script lang="ts" setup>
import { ref, computed, watch } from 'vue';
import { type Game } from '@/arkham/types/Game'
import { useDebug } from '@/arkham/debug'
import { useI18n } from 'vue-i18n';
import { updateGameRaw } from '@/arkham/api'
import campaignJSON from '@/arkham/data/campaigns.json'
import { BugAntIcon } from '@heroicons/vue/20/solid'

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

const skipTriggers = ref(investigator.value.settings.globalSettings.ignoreUnrelatedSkillTestTriggers)

watch(() => skipTriggers.value, (value) => {
  if (investigator.value) {
    debug.send(props.game.id,
      ({ tag: 'UpdateGlobalSetting'
       , contents: [investigator.value.id, {tag: "SetIgnoreUnrelatedSkillTestTriggers", contents: value}]
       }
      )
    )
  }
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
</script>
<template>
  <div class="settings">
    <div class="settings-header" :style="headerStyle">
      <h2 class="settings-title">{{$t('gameBar.viewSettingTitle', {investigator: investigator.name.title})}}</h2>
    </div>

    <div class="settings-body">
      <section class="settings-section">
        <h3 class="section-title">{{ $t('gameBar.playerSettings') }}</h3>

        <div class="toggle-list">
          <div class="toggle-row">
            <div class="toggle-text">
              <div class="toggle-name">{{$t('gameBar.viewSettingSkipTriggersTitle')}}</div>
              <div class="toggle-desc">{{$t('gameBar.viewSettingSkipTriggers')}}</div>
            </div>
            <div class="segmented segmented-2 toggle-control">
              <input type="radio" id="opt-skipTriggers-on" name="opt-skipTriggers" :checked="skipTriggers" @change="skipTriggers = true" />
              <label for="opt-skipTriggers-on">{{ $t('On') }}</label>
              <input type="radio" id="opt-skipTriggers-off" name="opt-skipTriggers" :checked="!skipTriggers" @change="skipTriggers = false" />
              <label for="opt-skipTriggers-off">{{ $t('Off') }}</label>
            </div>
          </div>

          <div class="toggle-row" v-if="canShowOtherHands">
            <div class="toggle-text">
              <div class="toggle-name">{{$t('gameBar.viewSettingShowOtherPlayersHandsTitle')}}</div>
              <div class="toggle-desc">{{$t('gameBar.viewSettingShowOtherPlayersHands')}}</div>
            </div>
            <div class="segmented segmented-2 toggle-control">
              <input type="radio" id="opt-showHands-on" name="opt-showHands" :checked="showOtherHands" @change="showOtherHands = true" />
              <label for="opt-showHands-on">{{ $t('On') }}</label>
              <input type="radio" id="opt-showHands-off" name="opt-showHands" :checked="!showOtherHands" @change="showOtherHands = false" />
              <label for="opt-showHands-off">{{ $t('Off') }}</label>
            </div>
          </div>
        </div>
      </section>

      <section v-if="recommendedToggles.length > 0" class="settings-section">
        <h3 class="section-title">{{ $t('create.recommendedOptions') ?? 'Campaign Options' }}</h3>
        <div class="toggle-list">
          <div class="toggle-row" v-for="o in recommendedToggles" :key="o.option.tag">
            <div class="toggle-text">
              <div class="toggle-name">
                <BugAntIcon v-if="o.icon === 'bug-ant'" class="toggle-icon" aria-hidden="true" />
                {{ optionTitle(o.option.tag) }}
              </div>
              <div class="toggle-desc" v-if="optionDescription(o.option.tag)">
                {{ optionDescription(o.option.tag) }}
              </div>
            </div>
            <div class="segmented segmented-2 toggle-control">
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

.section-title {
  margin: 0 0 10px;
  padding-bottom: 6px;
  font-family: Teutonic, serif;
  font-size: 13px;
  letter-spacing: 0.12em;
  text-transform: uppercase;
  color: var(--title);
  border-bottom: 1px solid var(--box-border);
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

.toggle-control {
  width: 150px;
  flex-shrink: 0;
}

.toggle-icon {
  width: 1em;
  height: 1em;
  vertical-align: -0.15em;
  margin-right: 0.45em;
}

.segmented {
  display: grid;
  border-radius: 5px;
  background: var(--background-dark);
  border: 1px solid var(--box-border);
  padding: 2px;
  gap: 2px;
}

.segmented-2 {
  grid-template-columns: repeat(2, 1fr);
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
