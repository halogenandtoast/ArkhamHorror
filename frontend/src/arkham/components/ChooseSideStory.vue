<script setup lang="ts">
import { inject, computed } from 'vue'
import { useI18n } from 'vue-i18n'
import { imgsrc } from '@/arkham/helpers'
import { Game } from '@/arkham/types/Game'
import type { Campaign } from '@/arkham/types/Campaign'
import { type CampaignStep } from '@/arkham/types/CampaignStep'
import type { Investigator } from '@/arkham/types/Investigator'
import sideStories from '@/arkham/data/side-stories.json'

const props = defineProps<{
  game: Game
  campaign: Campaign
  canUpgradeDecks: boolean
  step: CampaignStep
}>();

const send = inject<(msg: string) => void>('send', () => {})
const { t } = useI18n()

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

const standalones = computed(() => {
  if (props.step.tag !== "ScenarioStep" && props.step.tag !== "ScenarioStepWithOptions") return []
  const completed = props.campaign.completedSteps.reduce((acc: string[], step: CampaignStep) => {
    if (step.tag === 'StandaloneScenarioStep') {
      acc.push(step.contents[0].replace(/^c/, ''))
    }
    return acc
  }, [] as string[])

  return sideStories.filter((s: { xp: number, id: string }) => s.xp && s.xp <= minXp.value && !completed.includes(s.id))
})

async function chooseSideStory(sideStoryId: string) {
  send(JSON.stringify({
    tag: 'CampaignStepAnswer',
    contents: {
      tag: 'ContinueCampaignStep',
      contents: {
        canUpgradeDecks: true,
        nextStep: {
          tag: 'StandaloneScenarioStep',
          contents:
            [
              sideStoryId,
              {
                tag: 'ContinueCampaignStep',
                contents: {
                  nextStep: props.step,
                  canUpgradeDecks: props.canUpgradeDecks
                }
              }
            ]
        }
      }
    }
  })).then(() => {
    close()
  })
}

const emit = defineEmits<{
  close: []
}>()

async function close() {
  emit('close')
}
</script>

<template>
  <div v-if="standalones.length > 0" class="side-story-selection">
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
    <button @click="close">{{t('cancel')}}</button>
  </div>
</template>

<style scoped>
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

.scenario-info {
  h2 {
    color: white;
    font-family: "Teutonic", sans-serif;
    font-size: 1.8em;
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
