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
  closeSettings: () => void
}>()

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
    <div class="options box">
      <h2 class="title">{{$t('gameBar.viewSettingTitle', {investigator: investigator.name.title})}}</h2>
      <label>{{$t('gameBar.viewSettingSkipTriggers')}}</label>
      <input type="checkbox" v-model="skipTriggers" />
    </div>

    <div v-if="recommendedToggles.length > 0" class="options box">
      <h2 class="title">{{ $t('create.recommendedOptions') ?? 'Campaign Options' }}</h2>
      <div class="recommended-list">
        <div class="recommended-row" v-for="o in recommendedToggles" :key="o.option.tag">
          <div class="recommended-text">
            <div class="recommended-name">
              <BugAntIcon v-if="o.icon === 'bug-ant'" class="recommended-icon" aria-hidden="true" />
              {{ optionTitle(o.option.tag) }}
            </div>
            <div class="recommended-desc" v-if="optionDescription(o.option.tag)">
              {{ optionDescription(o.option.tag) }}
            </div>
          </div>
          <div class="segmented segmented-2 recommended-toggle">
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
    </div>

    <div>
      <button @click="closeSettings">{{$t('close')}}</button>
    </div>
  </div>
</template>

<style scoped>
.box {
  margin: 10px;
}

label {
  margin-right: 10px;
}

button {
  width: 100%;
}

.recommended-list {
  display: grid;
  gap: 10px;
}

.recommended-row {
  display: grid;
  grid-template-columns: 1fr auto;
  gap: 12px;
  align-items: center;
  padding-top: 8px;
  border-top: 1px solid rgba(255, 255, 255, 0.08);
}

.recommended-row:first-child {
  border-top: none;
  padding-top: 0;
}

.recommended-name {
  font-size: 14px;
  color: rgba(255, 255, 255, 0.9);
}

.recommended-desc {
  margin-top: 4px;
  font-size: 12px;
  line-height: 1.35;
  color: rgba(255, 255, 255, 0.65);
}

.recommended-toggle {
  width: 180px;
}

.recommended-icon {
  width: 1em;
  height: 1em;
  vertical-align: -0.15em;
  margin-right: 0.4em;
  opacity: 0.75;
}

.segmented input[type='radio'] {
  display: none;
}

.segmented {
  display: grid;
  border-radius: 12px;
  overflow: hidden;
  border: 1px solid rgba(255, 255, 255, 0.08);
  background: rgba(0, 0, 0, 0.12);
}

.segmented-2 {
  grid-template-columns: repeat(2, 1fr);
}

.segmented label {
  display: flex;
  align-items: center;
  justify-content: center;
  padding: 10px 8px;
  text-transform: uppercase;
  letter-spacing: 0.06em;
  font-size: 12px;
  user-select: none;
  cursor: pointer;
  background: rgba(255, 255, 255, 0.06);
  border-right: 1px solid rgba(255, 255, 255, 0.08);
  margin-right: 0;
}

.segmented label:last-of-type {
  border-right: none;
}

.segmented label:hover {
  background: rgba(255, 255, 255, 0.10);
}

.segmented input[type='radio']:checked + label {
  background: rgba(110, 134, 64, 0.95);
  box-shadow: inset 0 0 0 1px rgba(255, 255, 255, 0.10);
}

.segmented input[type='radio']:disabled + label {
  cursor: not-allowed;
  opacity: 0.6;
}

@media (max-width: 700px) {
  .recommended-row {
    grid-template-columns: 1fr;
  }
  .recommended-toggle {
    width: 100%;
  }
}
</style>
