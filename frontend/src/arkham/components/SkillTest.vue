<script lang="ts" setup>
import AbilityButton from '@/arkham/components/AbilityButton.vue'
import Question from '@/arkham/components/Question.vue';
import { computed } from 'vue';
import { ChaosBag } from '@/arkham/types/ChaosBag';
import { Game } from '@/arkham/types/Game';
import { SkillTest } from '@/arkham/types/SkillTest';
import { AbilityLabel, AbilityMessage, Message } from '@/arkham/types/Message'
import Draggable from '@/components/Draggable.vue';
import Card from '@/arkham/components/Card.vue'
import CommittedSkills from '@/arkham/components/CommittedSkills.vue';
import { MessageType, StartSkillTestButton } from '@/arkham/types/Message';
import * as ArkhamGame from '@/arkham/types/Game';
import { imgsrc, replaceIcons } from '@/arkham/helpers';
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
const skipTriggersAction = computed(() => choices.value.findIndex((c) => c.tag === MessageType.SKIP_TRIGGERS_BUTTON))
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

function isAbility(v: Message): v is AbilityLabel {
  if ("ability" in v) {
    const ability = v.ability
    if ("source" in ability) {
      const { source } = ability

      if (source.sourceTag === 'ProxySource') {
        return source.source.tag === 'SkillTestSource'
      }
    }
  }

  return false
}

const abilities = computed<AbilityMessage[]>(() => {
  return choices.value
    .reduce<AbilityMessage[]>((acc, v, i) =>
      isAbility(v) ? [...acc, { contents: v, displayAsAction: false, index: i}] : acc
    , [])
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

const applyResultsAction = computed(() => {
  return choices.value.findIndex((c) => c.tag === "SkillTestApplyResultsButton");
})

const skillValue = computed(() => {
  const result = skillTestResults.value
  if (result !== null) {
    const {skillTestResultsSkillValue, skillTestResultsIconValue, skillTestResultsChaosTokensValue } = result
    return skillTestResultsSkillValue + skillTestResultsIconValue + skillTestResultsChaosTokensValue
  } else {
    return props.skillTest.modifiedSkillValue
  }
})

const testResult = computed(() => {
  const result = skillTestResults.value
  if (result !== null) {
    const {skillTestResultsDifficulty} = result
    return skillValue.value - skillTestResultsDifficulty
  } else {
    return null
  }
})

const label = function(body: string) {
  if (body.startsWith("$")) {
    return t(body.slice(1))
  }
  return replaceIcons(body).replace(/_([^_]*)_/g, '<b>$1</b>').replace(/\*([^*]*)\*/g, '<i>$1</i>')
}

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
            <span class="difficulty">{{skillTest.modifiedDifficulty}}</span>
            <span>Test Difficulty</span>
          </div>
          <span>VS</span>
          <div class="modified-skill">
            <span class="skill">{{skillValue}}</span>
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

      <AbilityButton
        v-for="ability in abilities"
        :key="ability.index"
        :ability="ability.contents"
        :tooltipIsButtonText="true"
        @click="choose(ability.index)"
        />

      <div v-if="skillTestResults" class="skill-test-results" :class="{ success: skillTestResults.skillTestResultsSuccess, failure: !skillTestResults.skillTestResultsSuccess}">
        <span v-if="skillTestResults.skillTestResultsSuccess">
          Succeeded by {{testResult}}
        </span>
        <span v-else-if="testResult !== null">
          Failed by {{testResult - (skillTestResults.skillTestResultsResultModifiers || 0)}}
        </span>
      </div>

      <div v-if="skillTestResults" class="skill-test-results-break"></div>
      <button
        v-if="skipTriggersAction !== -1"
        @click="$emit('choose', skipTriggersAction)"
        class="skip-triggers-button"
      >Skip Triggers</button>
      <Question :game="game" :playerId="playerId" @choose="choose" :isSkillTest="true" />
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
  height: auto;
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

button {
  width: 100%;
  border: 0;
  text-align: center;
  text-transform: uppercase;
  transition: all 0.2s ease-in;
  border: 0;
  padding: 10px;
  background-color: darken($select, 30%);
  &:hover {
    background-color: darken($select, 20%);
  }
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

i {
  font-family: 'Arkham';
  font-style: normal;
  font-weight: normal;
  font-variant: normal;
  text-transform: none;
  line-height: 1;
  -webkit-font-smoothing: antialiased;
  position: relative;
}

i.iconSkull {
  &:before {
    font-family: "Arkham";
    content: "\004E";
  }
}

i.iconCultist {
  &:before {
    font-family: "Arkham";
    content: "\0042";
  }
}

i.iconTablet {
  &:before {
    font-family: "Arkham";
    content: "\0056";
  }
}

i.iconElderThing {
  &:before {
    font-family: "Arkham";
    content: "\0043";
  }
}

i.iconSkillWillpower {
  &:before {
    font-family: "Arkham";
    content: "\0041";
  }
}

i.iconSkillIntellect {
  &:before {
    font-family: "Arkham";
    content: "\0046";
  }
}

i.iconSkillCombat {
  &:before {
    font-family: "Arkham";
    content: "\0044";
  }
}

i.iconSkillAgility {
  &:before {
    font-family: "Arkham";
    content: "\0053";
  }
}

.button {
  display: inline-block;
  padding: 5px 10px;
  margin: 2px;
  background-color: #333;
  color: white;
  border: 1px solid #666;
  cursor: pointer;

  &:hover {
    background-color: #111;
  }

  &:active {
    background-color: #666;
    border-color: #111;
  }

  flex: 1;
}

.skill-test :deep(.choices) {
  display: flex;
  width: 100%;
  padding: 0;
  margin: 0;
  box-sizing: border-box;
  gap: 0;
  font-size: 0.7em;

  .message-label {
    flex: 1;
    margin: 0;
  }

  button {
    display: block;
    border: 0;
    text-align: left;
    text-transform: uppercase;
    transition: all 0.2s ease-in;
    border: 0;
    padding: 10px;
    margin: 0 !important;
    box-sizing: border-box;
    border-radius: 0;
    background-color: darken($select, 30%);
    &:hover {
      background-color: darken($select, 20%);
    }
    color: #EEE;
    font: Arial, sans-serif;
  }
}


.message-label {
  flex: 1;
}
</style>
