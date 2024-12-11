<script lang="ts" setup>
import AbilityButton from '@/arkham/components/AbilityButton.vue'
import Question from '@/arkham/components/Question.vue';
import { useDebug } from '@/arkham/debug'
import { computed, h } from 'vue';
import { ChaosBag } from '@/arkham/types/ChaosBag';
import * as Cards from '@/arkham/types/Card';
import { chaosTokenImage } from '@/arkham/types/ChaosToken';
import { scenarioToI18n } from '@/arkham/types/Scenario';
import { Game } from '@/arkham/types/Game';
import { Enemy } from '@/arkham/types/Enemy';
import { ModifierType, Modifier, cannotCommitCardsToWords } from '@/arkham/types/Modifier';
import { SkillTest } from '@/arkham/types/SkillTest';
import { AbilityLabel, AbilityMessage, Message } from '@/arkham/types/Message'
import Draggable from '@/components/Draggable.vue';
import Card from '@/arkham/components/Card.vue'
import CommittedSkills from '@/arkham/components/CommittedSkills.vue';
import { MessageType, StartSkillTestButton } from '@/arkham/types/Message';
import * as ArkhamGame from '@/arkham/types/Game';
import { imgsrc, formatContent } from '@/arkham/helpers';
import ChaosBagView from '@/arkham/components/ChaosBag.vue';
import { useI18n } from 'vue-i18n';

const debug = useDebug()
const { t } = useI18n()
const props = defineProps<{
  game: Game
  skillTest: SkillTest
  chaosBag: ChaosBag
  playerId: string
}>()

const normalizeSkill = (skill: string) => {
  switch (skill) {
    case 'SkillWillpower': return 'willpower'
    case 'SkillIntellect': return 'intellect'
    case 'SkillCombat': return 'combat'
    case 'SkillAgility': return 'agility'
    default: return skill
  }
}

const skills = computed(() => {
  const { skillTest } = props
  return skillTest.skills.map(normalizeSkill)
})
const skillTestResults = computed(() => props.game.skillTestResults)
const emit = defineEmits(['choose'])
const shouldRender = (mod: Modifier) => {
  const { type } = mod
  if (!('tag' in type)) return false
  if (type.tag === 'DiscoveredClues') return true
  if (type.tag === 'CancelEffects') return true
  if (type.tag === 'DamageDealt') return true
  if (type.tag === 'AnySkillValue') return true
  if (type.tag === 'SkillModifier') return true
  if (type.tag === 'ActionSkillModifier') return true
  if (type.tag === 'AddSkillValue') return true
  if (type.tag === 'RevealAnotherChaosToken') return true
  if (type.tag === 'CannotCommitCards')
    return props.playerId == props.game.investigators[props.skillTest.investigator].playerId
  if (type.tag === 'OtherModifier' && type.contents === 'MayIgnoreLocationEffectsAndKeywords') return true
  return false
}

const shouldRenderYourModifiers = (mod: Modifier) => {
  const { type } = mod
  if (!('tag' in type)) return false
  if (props.game.investigators[props.skillTest.investigator].playerId === props.playerId) return false
  if (type.tag === 'CannotCommitCards') return true
  return false
}

const yourModifiers = computed(() => {
  const investigator = Object.values(props.game.investigators).find((i) => i.playerId === props.playerId)
  if (!investigator) return []
  return (investigator.modifiers ?? []).filter(shouldRenderYourModifiers)
})

const modifiers = computed(() =>
  [...(props.game.investigators[props.skillTest.investigator]?.modifiers ?? []).
    filter(shouldRender), ...yourModifiers.value, ...(props.skillTest.modifiers ?? [])]) 
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

function sourceCardCode(source: Source) {
  if (source.tag === 'LocationSource') {
    const location = props.game.locations[source.contents]
    if (!location) return null
    const { cardCode, revealed } = location
    const suffix = revealed ? '' : 'b'

    return `${cardCode.replace('c', '')}${suffix}`
  }

  if (source.tag === 'AssetSource') {
    const asset = props.game.assets[source.contents]
    if (!asset) return null

    const mutated = asset.mutated ? `_${asset.mutated}` : ''
    if (asset.flipped) {
      if (asset.cardCode === "c90052") return "90052b"
      return null
    }
    return `${asset.cardCode.replace('c', '')}${mutated}`
  }

  if (source.tag === 'TreacherySource') {
    const treachery = props.game.treacheries[source.contents]
    if (!treachery) return null
    return `${treachery.cardCode.replace('c', '')}`
  }

  if (source.tag === 'EnemySource') {
    const enemy = props.game.enemies[source.contents]
    if (!enemy) return null

    const { cardCode, flipped } = enemy
    const suffix = flipped ? 'b' : ''
    return `${cardCode.replace('c', '')}${suffix}`
  }

  if (source.tag === 'AbilitySource') {
    const [inner,] = source.contents
    return sourceCardCode(inner)
  }

  console.log(source)
  if (source.tag === 'EventSource') {
    const event = props.game.events[source.contents]
    if (!event) return null

    const mutated = event.mutated ? `_${event.mutated}` : ''
    return `${event.cardCode.replace('c', '')}${mutated}`
  }

  return null
}

function modifierSource(mod: Modifier) {
  if(mod.card) {
    return mod.card.contents.cardCode.replace(/^c/, '')
  }
  return sourceCardCode(mod.source)
}

const targetCard = computed(() => {
  if (!props.skillTest.targetCard) return null
  return props.game.cards[props.skillTest.targetCard]
})

type SwarmEnemy = Omit<Enemy, "placement"> & {
  placement: { tag: "AsSwarm"; swarmHost: string; swarmCard: Cards.Card };
};

const swarmEnemy = computed<SwarmEnemy | null>(() => {
  let enemy = Object.values(props.game.enemies).find((e): e is SwarmEnemy => {
    if (e.placement.tag !== 'AsSwarm') return false
    return e.placement.swarmCard.contents.id === props.skillTest.targetCard
  })
  if (!enemy) return null
  return {...enemy}
})

const swarmHost = computed(() => {
  if (!swarmEnemy.value) return null
  return props.game.cards[props.game.enemies[swarmEnemy.value.placement.swarmHost].cardId]
})

const sourceCard = computed(() => {
  if (!props.skillTest.sourceCard) return null
  if (props.skillTest.sourceCard === props.skillTest.targetCard) return null
  return props.game.cards[props.skillTest.sourceCard]
})

const applyResultsAction = computed(() => {
  return choices.value.findIndex((c) => c.tag === "SkillTestApplyResultsButton");
})

const skillValue = computed(() => {
  const result = skillTestResults.value
  const cap = (a: number) => Math.max(0, a)
  if (result !== null) {
    const {skillTestResultsSkillValue, skillTestResultsIconValue, skillTestResultsChaosTokensValue } = result
    return cap(skillTestResultsSkillValue + skillTestResultsIconValue + skillTestResultsChaosTokensValue)
  } else {
    return cap(props.skillTest.modifiedSkillValue)
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

const tokenEffects = computed(() => {
  const scenario = props.game.scenario
  if(!scenario) return []
  const tokens = props.skillTest.resolvedChaosTokens.length > 0 ? props.skillTest.resolvedChaosTokens : props.skillTest.revealedChaosTokens
  const faces = tokens.map((t) => t.face)

  const difficulty = ['Easy', 'Standard'].includes(scenario.difficulty) ? 'easyStandard' : 'hardExpert'

  // lowercase the first letter
  const lowerFirst = (str: string) => str.charAt(0).toLowerCase() + str.slice(1)


  return ["Skull", "Cultist", "Tablet", "ElderThing"].filter((face) => faces.includes(face)).map((face) => 
    `<img src='${chaosTokenImage(face)}' width=23 /><span>`
          + formatContent(t(`${scenarioToI18n(scenario)}.tokens.${difficulty}.${lowerFirst(face)}`)) + `</span>`
          )
})
</script>

<template>
  <Draggable>
    <template #handle>
      <h2>{{ $t('skillTestTitle') }}</h2>
    </template>
    <div class="skill-test">
      <div class="steps">
        <div v-tooltip="$t('skillTest.determineSkillOfTestStep')" class="step" :class="{ active: skillTest.step === 'DetermineSkillOfTestStep' }">ST.1</div>
        <div v-tooltip="{content: formatContent($t('skillTest.fastPlayerWindow')), html: true }" class="step" :class="{ active: skillTest.step === 'SkillTestFastWindow1' }" v-html="formatContent('{fast}')" />
        <div v-tooltip="$t('skillTest.commitCardsFromHandToSkillTestStep')" class="step" :class="{ active: skillTest.step === 'CommitCardsFromHandToSkillTestStep' }">ST.2</div>
        <div v-tooltip="{content: formatContent($t('skillTest.fastPlayerWindow')), html: true}" class="step" :class="{ active: skillTest.step === 'SkillTestFastWindow2' }" v-html="formatContent('{fast}')" />
        <div v-tooltip="$t('skillTest.revealChaosTokenStep')" class="step" :class="{ active: skillTest.step === 'RevealChaosTokenStep' }">ST.3</div>
        <div v-tooltip="$t('skillTest.resolveChaosSymbolEffectsStep')" class="step" :class="{ active: skillTest.step === 'ResolveChaosSymbolEffectsStep' }">ST.4</div>
        <div v-tooltip="$t('skillTest.determineInvestigatorsModifiedSkillValueStep')" class="step" :class="{ active: skillTest.step === 'DetermineInvestigatorsModifiedSkillValueStep' }">ST.5</div>
        <div v-tooltip="$t('skillTest.determineSuccessOrFailureOfSkillTestStep')" class="step" :class="{ active: skillTest.step === 'DetermineSuccessOrFailureOfSkillTestStep' }">ST.6</div>
        <div v-tooltip="$t('skillTest.applySkillTestResultsStep')" class="step" :class="{ active: skillTest.step === 'ApplySkillTestResultsStep' }">ST.7</div>
        <div v-tooltip="$t('skillTest.skillTestEndsStep')" class="step" :class="{ active: skillTest.step === 'SkillTestEndsStep' }">ST.8</div>
      </div>
      <div class="skill-test-contents">
        <div v-if="swarmEnemy" class="target-card swarming">
          <div class="swarm">
            <img :src="imgsrc('player_back.jpg')" class="card" />
          </div>
          <div class="host">
            <Card :game="game" :card="swarmHost" :revealed="true" playerId="" />
          </div>
        </div>
        <Card v-else-if="targetCard" :game="game" :card="targetCard" class="target-card" :revealed="true" playerId="" />
        <div class="test-status">
          <div class="test-difficulty">
            <span class="difficulty">{{skillTest.modifiedDifficulty}}</span>
          </div>
          <div class="vs">
            <div v-if="skills.length > 0" class="skills">
              <div v-for="(skill, idx) in skills" :key="idx" :class="`${skill}-icon ${skill}-skill`">
                <span>{{skill}}</span>
              </div>
            </div>
            <div v-else-if="skillTest.baseValue.tag === 'HalfResourcesOf'" class="half-resources">
              <img :src="imgsrc(`resource.png`)" /> / 2
            </div>
            <span>VS</span>
          </div>
          <div class="modified-skill">
            <span class="skill">{{skillValue}}</span>
          </div>
        </div>
        <div class="test-source">
          <img
            v-if="investigatorPortrait"
            class="portrait"
            :src="investigatorPortrait"
          />
          <Card v-if="sourceCard" :game="game" :card="sourceCard" :revealed="true" playerId="" />
        </div>
      </div>
      <ChaosBagView
        :game="game"
        :chaosBag="chaosBag"
        :skillTest="skillTest"
        :playerId="playerId"
        @choose="choose"
      />
      <div v-if="modifiers.length > 0" class="modifiers">
        <div v-for="(modifier, idx) in modifiers" :key="idx" class="modifier" :data-image-id="modifierSource(modifier)">
          <template v-if="modifier.type.tag === 'CannotCommitCards'">
            <span>{{cannotCommitCardsToWords(modifier.type)}}</span>
          </template>
          <template v-if="modifier.type.tag === 'Difficulty'">
            <span>+{{modifier.type.contents}}</span>
            Difficulty
          </template>
          <template v-if="modifier.type.tag === 'CancelEffects'">
            <span class="text">Cancel Effects</span>
          </template>
          <template v-if="modifier.type.tag === 'CannotPerformSkillTest'">
            <span class="text">Cannot Perform Skill Test</span>
          </template>
          <template v-if="modifier.type.tag === 'DiscoveredClues'">
            <span>+{{modifier.type.contents}}</span>
            <img :src="imgsrc(`clue.png`)" />
          </template>
          <template v-if="modifier.type.tag === 'SkillTestResultValueModifier'">
            <span class="text">Result</span> <span>{{modifier.type.contents > 0 ? '+' : ''}}{{modifier.type.contents}}</span>
          </template>
          <template v-if="modifier.type.tag === 'DamageDealt'">
            <span>+{{modifier.type.contents}}</span>
            <img :src="imgsrc(`damage.png`)" />
          </template>
          <template v-if="modifier.type.tag === 'AddSkillValue'">
            <span>+</span>
            <i
               :class="`${normalizeSkill(modifier.type.contents)}-icon`"
               :style="{ color: `var(--${normalizeSkill(modifier.type.contents)})` }"
            ></i>
          </template>
          <template v-if="modifier.type.tag === 'SkillModifier'">
            <span>+ {{modifier.type.value}}</span>
            <i
               :class="`${normalizeSkill(modifier.type.skillType)}-icon`"
               :style="{ color: `var(--${normalizeSkill(modifier.type.skillType)})` }"
            ></i>
          </template>
          <template v-if="modifier.type.tag === 'ActionSkillModifier'">
            <span>+ {{modifier.type.value}}</span>
            <i
               :class="`${normalizeSkill(modifier.type.skillType)}-icon`"
               :style="{ color: `var(--${normalizeSkill(modifier.type.skillType)})` }"
            ></i>
          </template>
          <template v-if="modifier.type.tag === 'AnySkillValue'">
            <span>+ {{modifier.type.contents}}</span>
            <span class="text">Skill Value</span>
          </template>
          <template v-if="modifier.type.tag === 'OtherModifier' && modifier.type.contents === 'MayIgnoreLocationEffectsAndKeywords'">
            <span class="text">May Ignore Location Effects</span>
          </template>
          <template v-if="modifier.type.tag === 'OtherModifier' && modifier.type.contents === 'SkillTestAutomaticallySucceeds'">
            <span class="text">Skill test automatically succeeds</span>
          </template>
          <template v-if="modifier.type.tag === 'OtherModifier' && modifier.type.contents === 'RevealAnotherChaosToken'">
            <span class="text">Reveal another chaos token</span>
          </template>
        </div>
      </div>
      <div v-if="tokenEffects.length > 0" class="token-effects">
        <div class="token-effect" v-for="effect in tokenEffects" :key="effect" v-html="effect"></div>
      </div>
      <div v-if="debug.active && skillTest.result.tag == 'Unrun' && !['SkillTestFastWindow1', 'SkillTestFastWindow2'].includes(skillTest.step)">
        <button @click="debug.send(game.id, {tag: 'PassSkillTest'})">Pass Skill Test</button>
        <button @click="debug.send(game.id, {tag: 'FailSkillTest'})">Fail Skill Test</button>
      </div>
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
          Succeeded by {{(testResult ?? 0) + (skillTestResults.skillTestResultsResultModifiers || 0)}}
        </span>
        <span v-else-if="testResult !== null">
          Failed by {{testResult + (skillTestResults.skillTestResultsResultModifiers || 0)}}
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
    </div>
  </Draggable>
</template>

<style scoped lang="scss">
.skill-test {
  backdrop-filter: blur(0px);
  -webkit-backdrop-filter: blur(0px); /* Safari support */
  background: #75968600;
  width: fit-content;
  text-align: center;
  z-index: 10;
  overflow: hidden;
}

.skill-test-contents {
  padding: 10px;
  display: grid;
  grid-template-columns: 1fr 1fr 1fr;
  align-items: center;
  justify-items: center;
  gap: 5px;
  color: white;
  background-color: rgb(0, 0, 0, 0.6);
  backdrop-filter: blur(80px);
  -webkit-backdrop-filter: blur(80px); /* Safari support */
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
  width: var(--card-width);
  height: auto;
  border-radius: 5px;
  box-shadow: 1px 1px 6px rgba(0, 0, 0, 0.45);
  margin-right: calc(var(--card-width) + 5px);

  &:has(+ .card-container) {
    margin-right: 0;
  }
}

.committed-skills {
  background: rgba(0, 0, 0, 0.6);

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
}

button {
  width: 100%;
  border: 0;
  text-align: center;
  text-transform: uppercase;
  transition: all 0.2s ease-in;
  border: 0;
  padding: 10px;
  background-color: var(--select-dark-30);
  &:hover {
    background-color: var(--select-dark-20);
  }
  color: #EEE;
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
    border-radius: 0;
    background-color: var(--select-dark-30);
    &:hover {
      background-color: var(--select-dark-20);
    }
    color: #EEE;
  }
}


.message-label {
  flex: 1;
}

.vs {
  display: flex;
  flex-direction: column;
  align-items: center;
  gap: 5px;
}

.steps {
  display: flex;
  flex-direction: row;
  width: 100%;

  .step {
    flex: 1;
    text-align: center;
  }

  .step:nth-child(odd) {
    background: rgba(0, 0, 0, 0.4);
    color: white;
  }

  .step:nth-child(even) {
    background: rgba(0, 0, 0, 0.2);
    color: white;
  }

  .step.active {
    background: rgba(255, 0, 255, 0.5);
  }

}

.willpower-skill {
  color: var(--willpower);
  background: var(--willpower-light);
}

.intellect-skill {
  color: var(--intellect);
  background: var(--intellect-light);
}

.combat-skill {
  color: var(--combat);
  background: var(--combat-light);
}

.agility-skill {
  color: var(--agility);
  background: var(--agility-light);
}

.willpower-skill, .intellect-skill, .combat-skill, .agility-skill {
  font-size: 1.5em;
  width:1.2em;
  height:1.2em;
  line-height: 1.2em;
  border-radius: 50%;
}

.willpower-skill span, .intellect-skill span, .combat-skill span, .agility-skill span {
  display: none;
}

.target-card {
  margin-left: calc(var(--card-width) + 5px);
}

.swarming {
  display: flex;
  flex-direction: row;
  gap: 5px;

  .swarm img {
    width: var(--card-width);
    min-width: var(--card-width);
    border-radius: 7px;
    box-shadow: 0 3px 6px rgba(0,0,0,0.23), 0 3px 6px rgba(0,0,0,0.53);
    border-radius: 6px;
  }

  .host :deep(img) {
    box-sizing: border-box;
    border: 2px dashed var(--title);
    border-radius: 7px;
    box-shadow: 0 3px 6px rgba(0,0,0,0.23), 0 3px 6px rgba(0,0,0,0.53);
    border-radius: 6px;
  }
}


.skills {
  display: flex;
  flex-direction: row;
  gap: 5px;
}

.half-resources {
  img {
    width: 30px;
  }
  align-items: center;
  display: flex;
  gap: 5px;
}

.modifier {
  align-items: center;
  background: #000;
  border-radius: 100px;
  border: none;
  color: var(--title);
  display: flex;
  gap: 4px;
  margin-bottom: 10px;
  padding: 2px 10px;
  text-align: center;
  user-select: none;
  text-decoration: none;
  text-transform: uppercase;
  font-family: sans-serif;
  > * {
    pointer-events: none;
  }

  img {
    height: 15px;
  }

  i {
    align-self: flex-end;
  }

  .text {
    font-size: 0.8em;
  }
}

.modifiers {
  align-self: flex-start;
  display: flex;
  background: rgba(0, 0, 0, 0.5);
  padding-inline: 10px;
  gap: 5px;
  font-size: 1em;
}

.token-effects {
  background: rgba(0, 0, 0, 0.5);
}

.token-effect {
  background: transparent;
  display: flex;
  gap: 10px;
  padding: 10px;
  align-items: center;
  color: var(--title);
  justify-content: center;
}

.test-source {
  display: flex;
  flex-direction: row;
  gap: 5px;
}
</style>
