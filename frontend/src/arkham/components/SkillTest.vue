<script lang="ts" setup>
import { computed } from 'vue';
import { ChaosBag } from '@/arkham/types/ChaosBag';
import { Game } from '@/arkham/types/Game';
import { SkillTest } from '@/arkham/types/SkillTest';
import Draggable from '@/components/Draggable.vue';
import Card from '@/arkham/components/Card.vue'
import CommittedSkills from '@/arkham/components/CommittedSkills.vue';
import { MessageType, StartSkillTestButton } from '@/arkham/types/Message';
import * as ArkhamGame from '@/arkham/types/Game';
import { imgsrc } from '@/arkham/helpers';
import ChaosBagView from '@/arkham/components/ChaosBag.vue';
// has a slot for content

const props = defineProps<{
  game: Game
  skillTest: SkillTest
  chaosBag: ChaosBag
  playerId: string
}>()

const skillTestResults = computed(() => props.game.skillTestResults)

const emit = defineEmits(['choose'])

const committedCards = computed(() => props.skillTest.committedCards)

const choices = computed(() => ArkhamGame.choices(props.game, props.playerId))
const investigatorPortrait = computed(() => {
  const choice = choices.value.find((c): c is StartSkillTestButton => c.tag === MessageType.START_SKILL_TEST_BUTTON)
  if (choice) {
    const player = props.game.investigators[choice.investigatorId]

    if (player.isYithian) {
      return imgsrc(`portraits/${choice.investigatorId.replace('c', '')}.jpg`)
    }

    return imgsrc(`portraits/${player.cardCode.replace('c', '')}.jpg`)
  }

  if (props.skillTest) {
    const player = props.game.investigators[props.skillTest.investigator]

    if (player.isYithian) {
      return imgsrc(`portraits/${props.skillTest.investigator.replace('c', '')}.jpg`)
    }

    return imgsrc(`portraits/${player.cardCode.replace('c', '')}.jpg`)
  }

  return null;
})


async function choose(idx: number) {
  emit('choose', idx)
}

const card = computed(() => {

  if (!props.skillTest.card) {
    return null
  }

  return props.game.cards[props.skillTest.card]
})

const tokenOperator = computed(() => (skillTestResults.value?.skillTestResultsChaosTokensValue || 0) < 0 ? '-' : '+')

const applyResultsAction = computed(() => {
  return choices.value.findIndex((c) => c.tag === "SkillTestApplyResultsButton");
})


const testResult = computed(() => {
  const result = skillTestResults.value
  if (result !== null) {
    const {skillTestResultsSkillValue, skillTestResultsIconValue, skillTestResultsChaosTokensValue, skillTestResultsDifficulty} = result
    return skillTestResultsSkillValue + skillTestResultsIconValue + skillTestResultsChaosTokensValue - skillTestResultsDifficulty
  } else {
    return null
  }
})

</script>

<template>
  <Teleport to="body">
    <Draggable class="skill-test">
      <template #handle>
        <h2>Skill Test</h2>
      </template>
      <div class="skill-test-contents">
        <Card v-if="card" :game="game" :card="card" :revealed="true" playerId="" />
        <div class="test-status">
          <div class="test-difficulty">
            <span class="difficulty">{{skillTest.difficulty}}</span>
            <span>Test Difficulty</span>
          </div>
          <span>VS</span>
          <div class="modified-skill">
            <span class="skill">{{skillTest.modifiedSkillValue}}</span>
            <span>Modified Skill</span>
          </div>
        </div>
        <img
          v-if="investigatorPortrait"
          class="portrait"
          :src="investigatorPortrait"
        />
      </div>
      <ChaosBagView
        :game="game"
        :chaosBag="chaosBag"
        :skillTest="skillTest"
        :playerId="playerId"
        @choose="choose"
      />
      <div v-if="committedCards.length > 0" class="committed-skills" key="committed-skills">
        <div class="skills-container">
          <CommittedSkills
            :game="game"
            :cards="committedCards"
            :playerId="playerId"
            @choose="$emit('choose', $event)"
          />
        </div>
        <h2>Committed Skills</h2>
      </div>

      <div v-if="skillTestResults" class="skill-test-results" :class="{ success: skillTestResults.skillTestResultsSuccess, failure: !skillTestResults.skillTestResultsSuccess}">
        <span v-if="skillTestResults.skillTestResultsSuccess">
          Succeeded by {{testResult}}
        </span>
        <span v-else-if="testResult">
          Failed by {{testResult - (skillTestResults.skillTestResultsResultModifiers || 0)}}
        </span>
      </div>

      <div v-if="skillTestResults" class="skill-test-results-break"></div>
      <button
        class="apply-results"
        v-if="applyResultsAction !== -1"
        @click="choose(applyResultsAction)"
      >Apply Results</button>
    </Draggable>
  </Teleport>
</template>

<style scoped lang="scss">
.skill-test {
  background: #759686;
  width: fit-content;
  text-align: center;
  z-index: 10;
  overflow: hidden;
}

.skill-test-contents {
  padding: 10px;
  display: flex;
}

.test-status {
  flex: 1;
  display: flex;
  justify-content: center;
  align-items: center;
  gap: 30px;
  padding: 0 30px;
  text-transform: uppercase;
}

.test-difficulty {
  display: flex;
  flex-direction: column;
  align-items: center;
  gap: 5px;
}

.difficulty {
  background-color: darkred;
  color: white;
  font-weight: bold;
  display: inline-flex;
  align-items: center;
  justify-content: center;
  width: 30px;
  height: 30px;
  border-radius: 50%;
}

.skill {
  background-color: darkgreen;
  color: white;
  font-weight: bold;
  display: inline-flex;
  align-items: center;
  justify-content: center;
  width: 30px;
  height: 30px;
  border-radius: 50%;
}

.modified-skill {
  display: flex;
  flex-direction: column;
  align-items: center;
  gap: 5px;
}

.portrait {
  width: $card-width;
  height: fit-content;
}

.committed-skills {
  background: #333;

  h2 {
    background: #111;
    color: #666;
    text-transform: uppercase;
    margin: 0
  }
}

.skills-container {
  padding: 10px;
}

.skill-test-results {
  padding: 10px;
  text-align: left;
}

.skill-test-results-break {
  flex-basis: 100%;
  height: 0;
}

.apply-results {
  width: 100%;
  border: 0;
  text-align: center;
  text-transform: uppercase;
  transition: all 0.3s ease-in;
  border: 0;
  padding: 10px;
  background-color: #532e61;
  color: #EEE;
  font: Arial, sans-serif;
}

.success {
  background-color: darkgreen;
  text-transform: uppercase;
  text-align: center;
  color: white;
}

.failure {
  background-color: darkred;
  text-transform: uppercase;
  text-align: center;
  color: white;
}
</style>
