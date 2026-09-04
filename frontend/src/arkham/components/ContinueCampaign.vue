<script setup lang="ts">
import { inject, computed, ref, onMounted, watch, type Ref } from 'vue'
import { toCamelCase } from '@/arkham/helpers'
import { imgsrc } from '@/arkham/helpers'
import { Game } from '@/arkham/types/Game'
import { scenarioIdToI18n } from '@/arkham/types/Scenario'
import type { Campaign } from '@/arkham/types/Campaign'
import type { Scenario } from '@/arkham/types/Scenario'
import type { Investigator } from '@/arkham/types/Investigator'
import { type CampaignStep, campaignStepName, extendWithOptions } from '@/arkham/types/CampaignStep'
import { useI18n } from 'vue-i18n'
import InvestigatorRow from '@/arkham/components/InvestigatorRow.vue'
import LogIcons from '@/arkham/components/LogIcons.vue'
import sideStories from '@/arkham/data/side-stories.json'
import { useRoute, useRouter } from 'vue-router'
import { useClipboard } from '@vueuse/core'
import { buildShareableUrl } from '@/arkham/helpers'
import { joinCampaign, rejoinInvestigator, retireInvestigator } from '@/arkham/api'
import { useUserStore } from '@/stores/user'
import { storeToRefs } from 'pinia'
import { filterDisplayable, isDevBuild } from '@/arkham/displayRules'
import { hasParallelContent } from '@/arkham/deckRestrictions'

const props = defineProps<{
  game: Game
  campaign?: Campaign
  scenario?: Scenario
  playerId?: string
  canUpgradeDecks: boolean
  chooseSideStory: boolean
  canChooseSideStory: boolean
  step: CampaignStep
}>();

const { t, te } = useI18n()
const route = useRoute()
const router = useRouter()
const store = useUserStore()
const { currentUser } = storeToRefs(store)
const send = inject<(msg: string) => void>('send', () => {})
const addSideStory = ref(false)
const hasSent = ref(false)
const alpha = ref(false)
const dev = isDevBuild()
const isBetaUser = computed(() => !!currentUser.value?.beta)
const displayRuleOptions = computed(() => ({ alpha: alpha.value, beta: isBetaUser.value, dev }))
const sendOnce = (payload: unknown) => {
  if (hasSent.value) return
  hasSent.value = true
  send(JSON.stringify(payload))
}
const normalizedContents = (step: CampaignStep): string => {
  switch (step.tag) {
    case 'ScenarioStep':
    case 'InterludeStepPart':
    case 'CheckpointStep':
      return String(step.contents)
    case 'ScenarioStepWithOptions':
    case 'StandaloneScenarioStep':
    case 'StandaloneScenarioStepWithOptions':
    case 'InterludeStep':
    case 'CampaignSpecificStep':
      return JSON.stringify(step.contents)
    case 'ContinueCampaignStep':
      return normalizedContents(step.contents.nextStep)
    default:
      return step.tag
  }
}
// reset the lock on a "fresh update" of the step (new step name/kind)
onMounted(() => {
  hasSent.value = false
  alpha.value = route.query.alpha !== undefined || localStorage.getItem('alpha') === 'true'
  if (route.query.alpha !== undefined) localStorage.setItem('alpha', 'true')
})
const stepKey = computed(() => `${props.step.tag}:${JSON.stringify(normalizedContents(props.step))}`)
watch(stepKey, () => { hasSent.value = false })
const bonusXp = computed(() => props.campaign?.meta?.bonusXp ?? null)
const scenario = computed(() => {
  if (props.step.tag === 'ScenarioStep') {
    return props.step.contents.slice(1)
  }
  if (props.step.tag === 'ScenarioStepWithOptions') {
    return props.step.contents[0].slice(1)
  }
  if (props.step.tag === 'StandaloneScenarioStep') {
    return props.step.contents[0].slice(1)
  }
  return null
})

const name = computed(() => campaignStepName(props.game, props.step, props.scenario))

const numToRomanNumeral = (num: number): string => {
  const romanNumerals: { [key: number]: string } = {
    1: 'I',
    2: 'II',
    3: 'III',
    4: 'IV',
    5: 'V',
    6: 'VI',
    7: 'VII',
    8: 'VIII',
    9: 'IX',
    10: 'X'
  };
  return romanNumerals[num] || num.toString();
}
const kind = computed(() => {
  if (props.scenario) {
    const scenarioId = props.scenario.id
    const prefix = scenarioIdToI18n(scenarioId)
    let key = `${prefix}.heading`
    if (props.step.tag === 'CheckpointStep') {
      key = `${prefix}.headings.checkpoint${props.step.contents}`
    }
    if (te(key)) return t(key)
    return t('headings.scenario')
  }

  if (props.step.tag === 'StandaloneScenarioStep') {
    const scenarioId = props.step.contents[0]
    const prefix = scenarioIdToI18n(scenarioId)
    let key = `${prefix}.heading`
    if (props.step.contents[1]?.tag === 'CheckpointStep') {
      key = `${prefix}.headings.checkpoint${props.step.contents[1].contents}`
    }
    if (te(key)) return t(key)
    return t('headings.scenario')
  }

  if (props.step.tag === 'ScenarioStep') {
    const scenarioId = props.step.contents
    const prefix = scenarioIdToI18n(scenarioId)
    const key = `${prefix}.heading`
    if (te(key)) return t(key)
    return t('headings.scenario')
  }

  if (props.step.tag === 'ScenarioStepWithOptions') {
    const scenarioId = props.step.contents[0]
    const prefix = scenarioIdToI18n(scenarioId)
    const key = `${prefix}.heading`
    if (te(key)) return t(key)
    return t('headings.scenario')
  }

  if (props.step.tag === 'InterludeStep') {
    if (props.game.campaign) {
      const key = `${toCamelCase(props.game.campaign.name)}.headings.interludes.${props.step.contents[0]}`
      if (te(key)) {
        return t(key)
      }
    }
    return t('headings.interlude', { number: numToRomanNumeral(props.step.contents[0]) })
  }

  if (props.step.tag === 'CampaignSpecificStep') {
    if (props.game.campaign) {
      const [a, _] = props.step.contents
      const key = `${toCamelCase(props.game.campaign.name)}.headings.kind.${a}`
      if (te(key)) return t(key)
    }
  }

  return ''
})

const investigators = computed(() => {
  return Object.values(props.game.investigators)
})

const usesTime = computed<boolean>(() => {
  if(!props.campaign) return false
  return props.campaign.log.recordedCounts.some(([c, _v]: [any, number]) => c.tag === 'TheScarletKeysKey' && c.contents === 'Time')
})

const minXp = computed<number>(() => {
  if(!props.campaign) return 0
  const time = props.campaign.log.recordedCounts.find(([c, v]) => c.tag === 'TheScarletKeysKey' && c.contents === 'Time')
  if (time) return (35 - time[1])
  return investigators.value.reduce((acc: number, investigator: Investigator) => {
    const currentXp = investigator.xp
    return Math.min(acc, currentXp)
  }, Infinity)
})

const canUpgrade = computed(() => {
  if (!props.campaign) return false
  if (!props.canUpgradeDecks) return false
  if (props.step.tag === "CampaignSpecificStep" && props.canUpgradeDecks) return true
  if (!["ScenarioStep", "ScenarioStepWithOptions", "StandaloneScenarioStep"].includes(props.step.tag)) return false
  return (props.campaign.completedSteps ?? []).some((step: CampaignStep) => ['ScenarioStep', 'ScenarioStepWithOptions', 'StandaloneScenarioStep'].includes(step.tag))
})

const isScenario = computed(() =>  {
  // We do not yet handle the standalone step
  // return ["ScenarioStep", "StandaloneScenarioStep", "ScenarioStepWithOptions"].includes(props.step.tag)
  if (!(Object.values(props.game.investigators).length > 1)) return false
  return ["ScenarioStep", "StandaloneScenarioStep", "ScenarioStepWithOptions", "StandaloneScenarioStepWithOptions"].includes(props.step.tag)
})

const standalones = computed(() => {
  if (!props.campaign) return []
  if (!props.canChooseSideStory) return []
  const completed = (props.campaign.completedSteps ?? []).reduce((acc: string[], step: CampaignStep) => {
    if (step.tag === 'StandaloneScenarioStep') {
      acc.push(step.contents[0].replace(/^c/, ''))
    }
    return acc
  }, [] as string[])

  return filterDisplayable(sideStories, displayRuleOptions.value).flatMap((s: { xp: number, id: string, name: string, requiredInvestigator?: string, deckRequirements?: string[], scenarios?: { id: string, name: string, notAfter?: string[] }[] }) => {
    if (!s.xp) return []
    if (s.id === '90094' && !investigators.value.some((i) => hasParallelContent(i.cardCode))) return []
    if (s.requiredInvestigator) {
      // challenge scenarios require their investigator; they pay the full
      // cost while each other investigator only pays 1 xp
      const signature = investigators.value.find((i) => i.name.title === s.requiredInvestigator)
      if (!signature) return []
      if (usesTime.value) {
        if (s.xp > minXp.value) return []
      } else if (signature.xp < s.xp || investigators.value.some((i) => i.id !== signature.id && i.xp < 1)) {
        return []
      }
    } else if (s.xp > minXp.value) return []
    const parts = s.scenarios ?? [{ id: s.id, name: s.name }]
    return parts
      .filter((p) => !completed.includes(p.id))
      .filter((p) => !(p.notAfter ?? []).some((id) => completed.includes(id)))
      .map((p) => ({ ...s, id: p.id, name: p.name }))
  })
})

async function loadSideStory(sideStoryId: string) {
  addSideStory.value = false
  sendOnce({
    tag: 'CampaignStepAnswer',
    contents: {
      tag: 'ContinueCampaignStep',
      contents: {
        canUpgradeDecks: true,
        canChooseSideStory: false,
        nextStep: {
          tag: 'StandaloneScenarioStep',
          contents:
            [
              sideStoryId,
              {
                tag: 'ContinueCampaignStep',
                contents: {
                  nextStep: props.step,
                  canUpgradeDecks: props.canUpgradeDecks,
                  canChooseSideStory: false
                }
              }
            ]
        }
      }
    }
  })
}

async function upgradeDecks() {
  sendOnce({
    tag: 'CampaignStepAnswer',
    contents: {
      tag: 'UpgradeDeckStep',
      contents: {
        tag: 'ContinueCampaignStep',
        contents: {
          canUpgradeDecks: true,
          nextStep: props.step
        }
      }
    }
  })
}

async function startStep() {
  if (['ScenarioStep', 'StandaloneScenarioStep', 'ScenarioStepWithOptions'].includes(props.step.tag)) {
    if (isScenario.value && leadInvestigatorId.value !== null) {
      // inform the server of the lead investigator
      sendOnce({
        tag: 'CampaignStepAnswer',
        contents: extendWithOptions(props.step as Parameters<typeof extendWithOptions>[0], { scenarioOptionsLeadInvestigator: leadInvestigatorId.value })
      })
      return
    }
  }
  sendOnce({ tag: 'CampaignStepAnswer', contents: props.step })
}

const leadInvestigatorId = ref<string | null>(null)

const expeditionLeader = computed(() => {
  if (!props.campaign) return null
  if (!scenario.value) return null
  if (!['53016', '04043', '04054', '53017'].includes(scenario.value)) return null
  return props.campaign.meta?.expeditionLeader
})

// --- Joining and leaving a campaign (between scenarios) -------------------
// Only the seat holding the continuation ask can change the roster; the server
// rejects everyone else, so the controls are hidden rather than left to fail.
const solo = inject<Ref<boolean>>('solo', ref(false))
const rosterBusy = ref(false)
const rosterError = ref<string | null>(null)
const confirmingRetire = ref<string | null>(null)

const holdsContinuation = computed(() => {
  if (!props.playerId) return false
  const question = props.game.question[props.playerId]
  if (!question) return false
  const inner = question.tag === 'QuestionLabel' ? question.question : question
  return inner.tag === 'ContinueCampaign'
})

const canManageRoster = computed(() => !!props.campaign && holdsContinuation.value)
const retired = computed(() => Object.values(props.game.retiredInvestigators ?? {}))
const canRetire = computed(() => investigators.value.length > 1)
// A scenario is played by one to four investigators.
const canAdd = computed(() => investigators.value.length < 4)

// A one-player game is created WithFriends, so `solo` is false even though its
// single seat belongs to one user. Adding a second hand there turns it into
// multihanded solo, which the server does when it seats the new investigator.
const isSingleSeat = computed(() => props.game.playerCount === 1 && retired.value.length === 0)
const canAddOwnInvestigator = computed(() => solo.value || isSingleSeat.value)
const becomesMultihandedSolo = computed(() => !solo.value && isSingleSeat.value)

// The add control starts as one button. It only opens a panel when there is a
// choice to make: taking a second hand yourself versus inviting someone.
const addOpen = ref(false)
const addOptionCount = computed(() => (canAddOwnInvestigator.value ? 1 : 0) + (solo.value ? 0 : 1))

function startAdd() {
  if (addOptionCount.value === 1 && canAddOwnInvestigator.value) {
    addInvestigator()
    return
  }
  addOpen.value = true
}

const joinUrl = computed(() =>
  buildShareableUrl(router.resolve({ name: 'JoinGame', params: { gameId: props.game.id } }).href)
)
const { copy: copyInvite, copied: inviteCopied } = useClipboard({ source: joinUrl })

async function withRoster(action: () => Promise<void>) {
  if (rosterBusy.value) return
  rosterBusy.value = true
  rosterError.value = null
  try {
    await action()
  } catch (e) {
    rosterError.value = (e as { response?: { data?: string } }).response?.data ?? t('campaign.roster.failed')
  } finally {
    rosterBusy.value = false
  }
}

function retire(investigatorId: string) {
  confirmingRetire.value = null
  withRoster(() => retireInvestigator(props.game.id, investigatorId))
}

function rejoin(investigatorId: string) {
  withRoster(() => rejoinInvestigator(props.game.id, investigatorId))
}

function addInvestigator() {
  addOpen.value = false
  withRoster(() => joinCampaign(props.game.id))
}

const setIcon = computed(() => {
  if (!scenario.value) return null
  if (scenario.value.startsWith(":")) {
    const match = scenario.value.match(/^:(.+):(.+)$/)
    if (!match) return null
    const [, homebrew, scenarioId] = match
    return imgsrc(`homebrew/${homebrew}/sets/${scenarioId}.png`)
  }
  return imgsrc(`sets/${scenario.value}.png`)
})

</script>

<template>
  <LogIcons />
  <div class="continue-campaign scroll-container">
    <div v-if="chooseSideStory || (addSideStory && standalones.length > 0)" class="side-story-selection">
      <h2>{{ $t('sideStory.selectSideScenario') }}</h2>
      <div v-for="sideStory in standalones" :key="sideStory.id" class="side-story-option">
        <div class="scenario-icon">
          <img :src="imgsrc(`sets/${sideStory.id}.png`)" />
        </div>
        <div class="scenario-info">
          <h2>{{ sideStory.name }}</h2>
          <h3 v-if="sideStory.requiredInvestigator">{{ $t('sideStory.xpAsymmetric', { signatureXp: sideStory.xp, name: sideStory.requiredInvestigator, otherXp: 1 }) }}</h3>
          <template v-else>
            <h3>({{ sideStory.xp }} XP)</h3>
            <h3 v-for="requirement in sideStory.deckRequirements" :key="requirement">{{ requirement }}</h3>
          </template>
        </div>

        <button class="add" @click="loadSideStory(sideStory.id)" :disabled="hasSent">+</button>
      </div>
      <button v-if="!chooseSideStory" @click="addSideStory = false">{{t('cancel')}}</button>
    </div>
    <div v-else class="next-scenario">
      <div class="next-scenario-info">
        <div class='scenario-info'>
          <h3>{{kind}}</h3>
          <h2>{{name}}</h2>
        </div>
        <div class="actions">
          <button @click="startStep" :disable="hasSent">{{t('continue')}}</button>
          <button v-if="canUpgrade" @click="upgradeDecks" :disable="hasSent">{{t('upgradeDecks')}}</button>
          <button v-if="canChooseSideStory && standalones.length > 0" @click="addSideStory = true" :disable="hasSent">+ {{t('addSideScenario')}}</button>
        </div>
      </div>
      <div v-if="setIcon" class="next-step-icon"><img :src="setIcon" /></div>
    </div>

    <template v-if="!addSideStory && !chooseSideStory">
      <div v-if="investigators.length > 0" id="investigators">
        <section v-if="isScenario" id="investigators-header"><i class="secret"></i> {{t('lead')}}</section>
        <InvestigatorRow v-for="investigator in investigators" :key="investigator.id" :investigator="investigator" :game="game" :bonus-xp="bonusXp && bonusXp[investigator.id]">
          <template v-if="canManageRoster && canRetire" #actions="{ investigator }">
            <template v-if="confirmingRetire === investigator.id">
              <button class="roster-btn roster-btn--danger" :disabled="rosterBusy" @click="retire(investigator.id)">{{ t('campaign.roster.confirmLeave') }}</button>
              <button class="roster-btn" @click="confirmingRetire = null">{{ t('cancel') }}</button>
            </template>
            <button
              v-else
              class="roster-btn"
              :disabled="rosterBusy"
              v-tooltip="t('campaign.roster.leaveTooltip')"
              @click="confirmingRetire = investigator.id"
            >{{ t('campaign.roster.leave') }}</button>
          </template>
          <template v-if="isScenario" #back="{ investigator }">
            <label class="secret-radio">
              <input
                type="radio"
                class="secret-radio__input"
                name="lead-player"
                :disabled="expeditionLeader && investigator.id !== expeditionLeader"
                :checked="investigator.id === expeditionLeader"
                :value="investigator.id"
                v-model="leadInvestigatorId"
              />
              <span class="secret-radio__visual">
                <span class="secret-radio__circle"></span>
                <i class="secret"></i>
              </span>
            </label>
          </template>
        </InvestigatorRow>

        <template v-if="canManageRoster">
          <InvestigatorRow
            v-for="investigator in retired"
            :key="investigator.id"
            :investigator="investigator"
            :game="game"
            dimmed
          >
            <template #actions>
              <button class="roster-btn" :disabled="rosterBusy || !canAdd" @click="rejoin(investigator.id)">
                {{ t('campaign.roster.rejoin') }}
              </button>
            </template>
          </InvestigatorRow>

          <button
            v-if="!addOpen"
            class="roster-btn roster-add"
            :disabled="rosterBusy || !canAdd"
            @click="startAdd"
          >+ {{ t('campaign.roster.add') }}</button>

          <div v-else class="roster-add-panel">
            <button v-if="canAddOwnInvestigator" class="roster-btn" :disabled="rosterBusy" @click="addInvestigator">
              {{ becomesMultihandedSolo ? t('campaign.roster.addSolo') : t('campaign.roster.add') }}
            </button>
            <p v-if="becomesMultihandedSolo" class="roster-hint">{{ t('campaign.roster.addSoloHint') }}</p>
            <template v-if="!solo">
              <p class="roster-hint">{{ t('campaign.roster.inviteHint') }}</p>
              <div class="roster-invite">
                <input type="text" :value="joinUrl" readonly />
                <button class="roster-btn" type="button" @click="copyInvite()">{{ inviteCopied ? t('lobby.copied') : t('lobby.copy') }}</button>
              </div>
            </template>
            <button class="roster-btn" @click="addOpen = false">{{ t('cancel') }}</button>
          </div>

          <p v-if="rosterError" class="roster-error">{{ rosterError }}</p>
        </template>
      </div>

    </template>
  </div>
</template>

<style scoped lang="scss">
.next-scenario {
  display: flex;
  justify-content: space-between;
  gap: 10px;
  @media (max-width: 800px) {
    flex-direction: column;
    align-items: center;
    text-align: center;
  }
  padding: 16px;
  color: #bebebe;
  border: 2px solid var(--line);
  border-radius: 8px;
  background: rgba(255, 255, 255, 0.1);
  img {
    max-height: 150px;
  }

}


.scenario-info {
  h2 {
    color: white;
    font-family: "Teutonic", sans-serif;
    font-size: 1.8em;
  }
}

.next-scenario-info {
  display: flex;
  flex-direction: column;
  gap: 10px;
  flex: 1;
  .actions {
    flex: 1;
    display: flex;
    flex-direction: column;
    gap: 10px;
    justify-content: flex-end;
    button {
      border: 0;
      background: rgba(0, 0, 0, 0.3);
      color: white;
      font-size: 1.2em;
      padding: 10px 20px;
      border-radius: 8px;
      width: 100%;
      justify-self: end;
      &:hover {
        background: rgba(0, 0, 0, 0.5);
        cursor: pointer;
      }
    }
  }
}

.next-step-icon {
  width: 150px;
  filter: invert(100%) brightness(60%);
  justify-content: center;
  align-items: center;
  display: flex;
  /* when too small to fit, hide */
  @media (max-width: 600px) {
    display: none;
  }
}

.continue-campaign {
  margin-top: 5vh;
  margin-inline: auto;
  display: flex;
  flex-direction: column;
  gap: 20px;
  width: 40vw;
}

.investigator {
  width: 100%;
}

.side-story-selection {
  > h2 {
    color: white;
    font-family: "Teutonic", sans-serif;
    font-size: 1.5em;
    margin-bottom: 10px;
  }
  display: flex;
  flex-direction: column;
  gap: 10px;
  .side-story-option {
    border: 1px solid var(--line);
    border-radius: 8px;
    padding: 10px;
    background: rgba(255, 255, 255, 0.1);
    display: flex;
    gap: 10px;
    h3 {
      margin: 0;
      color: white;
    }
    img {
      max-height: 60px;
      filter: invert(100%) brightness(60%);
    }
    .scenario-icon {
      margin-right: 10px;
      width: 60px;
      justify-content: center;
      display: flex;
    }
  }
}

button {
  border: 0;
  background: rgba(0, 0, 0, 0.3);
  color: white;
  font-size: 1em;
  padding: 8px 16px;
  border-radius: 8px;
  &:hover {
    background: rgba(0, 0, 0, 0.5);
    cursor: pointer;
  }
}

.add {
  font-size: 1.5em;
  width: 40px;
  height: 40px;
  align-self: center;
  margin-left: auto;
  display: flex;
  align-items: center;
  justify-content: center;
}

.investigators {
  display: flex;
  flex-direction: column;
  gap: 10px;
}

.secret-radio {
  position: relative;
  display: inline-flex;
  align-items: center;
  justify-content: center;
  width: 2em;
  height: 2em;
  cursor: pointer;

  &__input {
    position: absolute;
    inset: 0;
    opacity: 0;
    cursor: pointer;
    margin: 0;
  }

  &__visual {
    position: relative;
    width: 100%;
    height: 100%;
    border-radius: 9999px;
    border: 1px solid rgba(255, 255, 255, 0.5);
    overflow: hidden;
    box-sizing: border-box;
    background: rgba(0, 0, 0, 0.4);
  }

  &__circle {
    position: absolute;
    inset: 0;
    border-radius: inherit;
    background: var(--neutral-dark);
    transform: scale(0);
    opacity: 0;
    transition:
      transform 0.18s ease-out,
      opacity 0.18s ease-out;
    transform-origin: center;
  }

  i.secret {
    position: relative;
    width: 100%;
    height: 100%;
    display: flex;
    align-items: center;
    justify-content: center;

    &:before {
      font-family: "Arkham";
      content: "\0048";
    }

    font-size: 1.5em;
    color: var(--title);

    opacity: 0;
    transform: scale(0.8);
    transition:
      opacity 0.12s ease-out 0.08s,
      transform 0.12s ease-out 0.08s;
  }

  &__input:checked + .secret-radio__visual .secret-radio__circle {
    transform: scale(1);
    opacity: 1;
  }

  &__input:checked + .secret-radio__visual i.secret {
    opacity: 1;
    transform: scale(1);
  }
}

#investigators {
  padding: 10px;
  background: rgba(255, 255, 255, 0.05);
  display: flex;
  gap: 10px;
  flex-direction: column;
  border-radius: 8px;
}


.roster-add {
  width: 100%;
  padding: 10px;
  font-size: 1em;
}

.roster-add-panel {
  display: flex;
  flex-direction: column;
  gap: 8px;
  padding: 10px;
  border: 1px solid var(--line);
  border-radius: 8px;
  background: rgba(0, 0, 0, 0.2);
}

.roster-hint {
  margin: 0;
  color: #bebebe;
  font-size: 0.85em;
}

.roster-invite {
  display: flex;
  gap: 8px;
  input {
    flex: 1;
    min-width: 0;
    background: rgba(0, 0, 0, 0.3);
    border: 1px solid var(--line);
    border-radius: 8px;
    color: #e0e0e0;
    padding: 8px;
  }
}

.roster-error {
  margin: 0;
  color: var(--survivor);
}

.roster-btn {
  font-size: 0.85em;
  padding: 6px 12px;
  white-space: nowrap;
  &:disabled {
    opacity: 0.5;
    cursor: default;
  }
}

.roster-btn--danger {
  background: oklch(from var(--survivor) calc(l - 0.35) c h);
  &:hover {
    background: oklch(from var(--survivor) calc(l - 0.25) c h);
  }
}

#investigators-header {
  color: var(--title);
  text-align: end;
  border-bottom: 1px solid oklch(from var(--title) calc(l - 0.4) c h);
  padding-bottom: 4px;
  i.secret {
    &:before {
      font-family: "Arkham";
      content: "\0048";
    }

    font-size: 1.5em;
  }
}
</style>
