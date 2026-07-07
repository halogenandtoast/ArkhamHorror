<script lang="ts" setup>
import AbilityButton from '@/arkham/components/AbilityButton.vue'
import FormattedEntry from '@/arkham/components/FormattedEntry.vue'
import { MessageType, Message } from '@/arkham/types/Message';
import { formatContent } from '@/arkham/helpers';
import { handleEmbeddedI18n } from '@/arkham/i18n';
import { formatCost } from '@/arkham/cost';
import { useI18n } from 'vue-i18n';
import type { Game } from '@/arkham/types/Game';

defineProps<{
  game: Game
  choices: [Message, number][]
}>()

const emit = defineEmits<{
  (e: 'choose', value: number): void
}>()

const choose = (idx: number) => emit('choose', idx)

const { t } = useI18n()
const label = function(body: string) {
  return formatContent(handleEmbeddedI18n(body, t))
}

const drownedCityTaskCards: Record<string, string> = {
  noPlaceLikeHome: '11753a',
  walkInFaith: '11754a',
  toeTheLine: '11755a',
  goodMoney: '11756a',
  proveYourWorth: '11757a',
  doNoHarm: '11758a',
  dreamsOfDestruction: '11759a',
  plumbTheDepths: '11760a',
}

const drownedCityTaskNames: Record<string, string> = {
  'No Place Like Home': 'noPlaceLikeHome',
  'Walk in Faith': 'walkInFaith',
  'Toe the Line': 'toeTheLine',
  'Good Money': 'goodMoney',
  'Prove Your Worth': 'proveYourWorth',
  'Do No Harm': 'doNoHarm',
  'Dreams of Destruction': 'dreamsOfDestruction',
  'Plumb the Depths': 'plumbTheDepths',
}

const drownedCityTaskKey = (body: string) => {
  const raw = body.trim()
  const i18nKey = raw.replace(/^\$/, '')
  const prefix = 'theDrownedCity.anOfferYouCantRefuse.label.'
  if (i18nKey.startsWith(prefix)) {
    const key = i18nKey.slice(prefix.length)
    return key in drownedCityTaskCards ? key : null
  }
  if (raw in drownedCityTaskCards) return raw

  const localized = handleEmbeddedI18n(body, t)
  return drownedCityTaskNames[localized] ?? null
}

const drownedCityTaskCardCode = (body: string) => {
  const key = drownedCityTaskKey(body)
  return key ? drownedCityTaskCards[key] : undefined
}

const drownedCityTaskRecommendation = (body: string) => {
  const key = drownedCityTaskKey(body)
  return key ? label(`$theDrownedCity.anOfferYouCantRefuse.recommended.${key}`) : null
}
</script>
<template>
  <div class='question-choices'>
    <template v-for="[choice, index] in choices" :key="index">
      <template v-if="choice.tag === MessageType.ABILITY_LABEL">
        <AbilityButton
          :ability="choice"
          :game="game"
          @click="choose(index)"
          />
      </template>
      <template v-else-if="choice.tag === MessageType.TARGET_LABEL">
        <button @click="choose(index)">{{ t('continue') }}</button>
      </template>
      <template v-else-if="choice.tag === MessageType.TOOLTIP_LABEL">
        <button @click="choose(index)" v-tooltip="choice.tooltip">{{ t(choice.label) }}</button>
      </template>
      <div v-else-if="choice.tag === MessageType.LABEL" class="message-label">
        <button v-if="choice.label == 'Choose {skull}'" @click="choose(index)">
          Choose <i class="iconSkull"></i>
        </button>
        <button v-else-if="choice.label == 'Choose {cultist}'" @click="choose(index)">
          Choose <i class="iconCultist"></i>
        </button>
        <button v-else-if="choice.label == 'Choose {tablet}'" @click="choose(index)">
          Choose <i class="iconTablet"></i>
        </button>
        <button v-else-if="choice.label == 'Choose {elderThing}'" @click="choose(index)">
          Choose <i class="iconElderThing"></i>
        </button>
        <button
          v-else-if="drownedCityTaskCardCode(choice.label)"
          @click="choose(index)"
          class="task-choice"
        >
          <span class="choice-content">
            <span class="choice-label" v-html="label(choice.label)"></span>
            <span
              v-if="drownedCityTaskRecommendation(choice.label)"
              class="choice-subtext"
              v-html="drownedCityTaskRecommendation(choice.label)"
            ></span>
          </span>
        </button>
        <button v-else @click="choose(index)" v-html="label(choice.label)"></button>
      </div>
      <div v-else-if="choice.tag === MessageType.COST_LABEL" class="message-label">
        <button @click="choose(index)" v-html="label(formatCost(choice.cost, t))"></button>
      </div>
      <div
        v-else-if="choice.tag === MessageType.INVALID_LABEL"
        class="message-label"
      >
        <button
          v-if="drownedCityTaskCardCode(choice.label)"
          class="task-choice"
          disabled
        >
          <span class="choice-content">
            <span class="choice-label" v-html="label(choice.label)"></span>
            <span
              v-if="drownedCityTaskRecommendation(choice.label)"
              class="choice-subtext"
              v-html="drownedCityTaskRecommendation(choice.label)"
            ></span>
          </span>
        </button>
        <button v-else v-html="label(choice.label)" disabled></button>
      </div>
      <div v-else-if="choice.tag === MessageType.INFO" class="message-label">
        <FormattedEntry v-for="(entry, entryIndex) in choice.flavor.body" :key="entryIndex" :entry="entry" />
      </div>
      <div v-else-if="choice.tag === MessageType.DONE" class="message-label">
        <button @click="choose(index)" v-html="label(choice.label)"></button>
      </div>

      <a
        v-if="choice.tag === MessageType.SKILL_LABEL"
        class="button"
        @click="choose(index)"
      >
        Use <i :class="`icon${choice.skillType}`"></i>
      </a>

      <a
        v-if="choice.tag === MessageType.SKILL_LABEL_WITH_LABEL"
        class="button"
        @click="choose(index)"
      >
        Use <i :class="`icon${choice.skillType}`"></i>: {{ t(choice.label) }}
      </a>
    </template>
  </div>
</template>

<style scoped>

a.button {
  display: block;
  background-color: var(--neutral-dark);
  color: white;
  border: 1px solid #666;
  cursor: pointer;
  align-content: center;
  align-items: center;
  line-height: 1.2em;

  &:has(i.iconSkillCombat) {
    background-color: #8b0000;
    &:hover {
      background-color: #5a0000;
    }
  }
  &:has(i.iconSkillAgility) {
    background-color: #004d00;
    &:hover {
      background-color: #003300;
    }
  }
  &:has(i.iconSkillWillpower) {
    background-color: #00008b;
    &:hover {
      background-color: #00005a;
    }
  }
}

a.button:hover {
  background-color: #111;
}

a.button:active {
  background-color: #666;
  border-color: #111;
}

button, a.button {
  background-color: var(--button-2);
  border-radius: 0.6em;
  border: 0;
  color: #DDD;
  font-size: 1.2em;
  font-weight: bold;
  font-family: Arial, sans-serif;
  padding: 10px;
  text-align: justify;
  transition: all 0.3s ease-in;
  white-space: break-spaces;
  width: 100%;
  text-transform: uppercase;
  display: flex;

  &:deep(strong) {
    display: contents;
    color: #FFF;
  }

  &[disabled] {
    cursor: not-allowed;
    background-color: #999 !important;
  }

  &::before {
    font-family: "ArkhamIcons";
    content: "\E91A";
    margin-right: 10px;
    flex: 0 0 auto;
  }

  &.task-choice {
    align-items: flex-start;
    text-align: left;
  }
}

.choice-content,
.choice-label {
  display: block;
}

.choice-content {
  flex: 1 1 auto;
}

.choice-subtext {
  color: #cfc6d8;
  display: block;
  font-size: 0.72em;
  font-weight: 600;
  line-height: 1.25;
  margin-top: 4px;
  text-transform: none;
}

.choice-subtext :deep(.guardian-icon)::before,
.choice-subtext :deep(.seeker-icon)::before,
.choice-subtext :deep(.rogue-icon)::before,
.choice-subtext :deep(.mystic-icon)::before,
.choice-subtext :deep(.survivor-icon)::before {
  display: inline-block;
  font-family: "Arkham";
  font-size: 1.1em;
  font-weight: normal;
  text-transform: none;
}

.choice-subtext :deep(.guardian-icon)::before { content: "\0051"; }
.choice-subtext :deep(.seeker-icon)::before { content: "\0045"; }
.choice-subtext :deep(.rogue-icon)::before { content: "\0054"; }
.choice-subtext :deep(.mystic-icon)::before { content: "\0057"; }
.choice-subtext :deep(.survivor-icon)::before { content: "\0052"; }

  button:hover, a.button:hover {
  background-color: #311b3e;

  &[disabled] {
    background-color: #999 !important;
  }
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

i.iconSkull:before {
  content: "\004E";
}

i.iconCultist:before {
  content: "\0042";
}

i.iconTablet:before {
  content: "\0056";
}

i.iconElderThing:before {
  content: "\0043";
}

i.iconSkillWillpower:before {
  content: "\0041";
  font-size: 1.3em;
}

i.iconSkillIntellect:before {
  content: "\0046";
  font-size: 1.3em;
}

i.iconSkillCombat:before {
  content: "\0044";
  font-size: 1.3em;
}

i.iconSkillAgility:before {
  content: "\0053";
  font-size: 1.3em;
}

.question-choices {
  display: flex;
  gap: 10px;
  flex-direction: column;
  &:has(.question-label) {
    padding: 10px;
  }
}

:deep(.message-label) {
  .agility-icon, .combat-icon, .intellect-icon, .willpower-icon {
    display: contents;
    &::before {
      display: contents;
    }
  }
}
</style>
