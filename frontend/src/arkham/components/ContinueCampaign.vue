<script setup lang="ts">
import { inject, computed, ref } from 'vue'
import { toCamelCase } from '@/arkham/helpers'
import { imgsrc } from '@/arkham/helpers'
import { Game } from '@/arkham/types/Game'
import { scenarioIdToI18n } from '@/arkham/types/Scenario'
import type { Campaign } from '@/arkham/types/Campaign'
import type { Investigator } from '@/arkham/types/Investigator'
import { type CampaignStep, campaignStepName } from '@/arkham/types/CampaignStep'
import { useI18n } from 'vue-i18n'
import InvestigatorRow from '@/arkham/components/InvestigatorRow.vue'
import LogIcons from '@/arkham/components/LogIcons.vue'
import sideStories from '@/arkham/data/side-stories.json'

const props = defineProps<{
  game: Game
  campaign: Campaign
  step: CampaignStep
}>();

const { t, te } = useI18n()
const send = inject<(msg: string) => void>('send', () => {})
const addSideStory = ref(false)


const bonusXp = computed(() => props.campaign.meta?.bonusXp ?? null)
const scenario = computed(() => {
  if (props.step.tag === 'ScenarioStep') {
    return props.step.contents.slice(1)
  }
  if (props.step.tag === 'StandaloneScenarioStep') {
    return props.step.contents[0].slice(1)
  }
  return null
})

const name = computed(() => campaignStepName(props.game, props.step))
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
  if (props.step.tag === 'ScenarioStep' || props.step.tag === 'StandaloneScenarioStep') {
    const scenarioId = props.step.tag === 'ScenarioStep' ? props.step.contents : props.step.contents[0]
    const prefix = scenarioIdToI18n(scenarioId)
    const key = `${prefix}.heading`
    if (te(key)) return t(key)
    return t('headings.scenario')
  }

  if (props.step.tag === 'InterludeStep') {
    return t('headings.interlude', { number: numToRomanNumeral(parseInt(props.step.contents[0])) })
  }
  return ''
})

const investigators = computed(() => {
  return Object.values(props.game.investigators)
})

const minXp = computed<number>(() => {
  return investigators.value.reduce((acc: number | null, investigator: Investigator) => {
    const currentXp = investigator.xp
    if (acc === null) {
      return currentXp
    }
    return Math.min(acc, currentXp)
  }, null)  
})

const canUpgrade = computed(() => {
  if (props.step.tag !== "ScenarioStep" && props.step.tag !== "StandaloneScenarioStep") return false
  return props.campaign.completedSteps.some((step: CampaignStep) => step.tag === 'ScenarioStep' || step.tag === 'StandaloneScenarioStep')
})

const standalones = computed(() => {
  if (props.step.tag !== "ScenarioStep") return []
  const completed = props.campaign.completedSteps.reduce((acc: string[], step: CampaignStep) => {
    if (step.tag === 'StandaloneScenarioStep') {
      acc.push(step.contents[0].replace(/^c/, ''))
    }
    return acc
  }, [] as string[])

  return sideStories.filter((s: { xp: number, id: string }) => s.xp && s.xp <= minXp.value && !completed.includes(s.id))
})

async function chooseSideStory(sideStoryId: string) {
  addSideStory.value = false
  send(JSON.stringify({ tag: 'CampaignStepAnswer', contents: { tag: 'ContinueCampaignStep', contents: { tag: 'StandaloneScenarioStep', contents: [sideStoryId, {tag: 'ContinueCampaignStep', contents: props.step}]}}}))
}

async function upgradeDecks() {
  send(JSON.stringify({ tag: 'CampaignStepAnswer', contents: { tag: 'UpgradeDeckStep', contents: { tag: 'ContinueCampaignStep', contents: props.step }}}))
}

async function startStep() {
  send(JSON.stringify({ tag: 'CampaignStepAnswer', contents: props.step }))
}

</script>

<template>
  <LogIcons />
  <div class="continue-campaign scroll-container">
    <div v-if="addSideStory && standalones.length > 0" class="side-story-selection">
      <h2>Select a side scenario to add</h2>
      <div v-for="sideStory in standalones" :key="sideStory.id" class="side-story-option">
        <div class="scenario-icon">
          <img :src="imgsrc(`sets/${sideStory.id}.png`)" />
        </div>
        <div class="scenario-info">
          <h2>{{ sideStory.name }}</h2>
          <h3>({{ sideStory.xp }} XP)</h3>
        </div>

        <button class="add" @click="chooseSideStory(sideStory.id)">+</button>
      </div>
      <button @click="addSideStory = false">{{t('cancel')}}</button>
    </div>
    <div v-else class="next-scenario">
      <div class="next-scenario-info">
        <div class='scenario-info'>
          <h3>{{kind}}</h3>
          <h2>{{name}}</h2>
        </div>
        <div class="actions">
          <button @click="startStep">{{t('continue')}}</button>
          <button v-if="canUpgrade" @click="upgradeDecks">{{t('upgradeDecks')}}</button>
          <button v-if="standalones.length > 0" @click="addSideStory = true">+ {{t('addSideScenario')}}</button>
        </div>
      </div>
      <div v-if="scenario" class="next-step-icon"><img :src="imgsrc(`sets/${scenario}.png`)" /></div>
    </div>

    <template v-if="!addSideStory">
      <InvestigatorRow v-for="investigator in investigators" :key="investigator.id" :investigator="investigator" :game="game" :bonus-xp="bonusXp && bonusXp[investigator.id]" />
    </template>
  </div>
</template>

<style scoped>
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
</style>
