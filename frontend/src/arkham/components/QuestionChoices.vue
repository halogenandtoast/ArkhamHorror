<script lang="ts" setup>
import AbilityButton from '@/arkham/components/AbilityButton.vue'
import FormattedEntry from '@/arkham/components/FormattedEntry.vue'
import { MessageType, Message } from '@/arkham/types/Message';
import { formatContent } from '@/arkham/helpers';
import { handleI18n } from '@/arkham/i18n';
import { useI18n } from 'vue-i18n';
import type { Game } from '@/arkham/types/Game';

defineProps<{
  game: Game
  choices: Message[]
}>()

const emit = defineEmits<{
  choose: (value: number) => void
}>()

const choose = (idx: number) => emit('choose', idx)

const { t } = useI18n()
const label = function(body: string) {
  if (body.startsWith("$")) {
    return formatContent(handleI18n(body.slice(1), t))
  }
  return formatContent(body)
}
</script>
<template>
  <div class='question-choices'>
    <template v-for="(choice, index) in choices" :key="index">
      <template v-if="choice.tag === 'AbilityLabel' && ['DisplayAsCard'].includes(choice.ability.displayAs)">
        <AbilityButton
          :ability="choice"
          :game="game"
          @click="choose(index)"
          />
      </template>
      <template v-if="choice.tag === MessageType.TOOLTIP_LABEL">
        <button @click="choose(index)" v-tooltip="choice.tooltip">{{choice.label}}</button>
      </template>
      <template v-if="choice.tag === MessageType.ABILITY_LABEL && choice.ability.type.tag === 'ConstantReaction'">
        <button @click="choose(index)">{{choice.ability.type.label}}</button>
      </template>
      <div v-if="choice.tag === MessageType.LABEL" class="message-label">
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
        <button v-else @click="choose(index)" v-html="label(choice.label)"></button>
      </div>
      <div v-else-if="choice.tag === MessageType.INVALID_LABEL" class="message-label">
        <button v-html="label(choice.label)" disabled></button>
      </div>
      <div v-else-if="choice.tag === MessageType.INFO" class="message-label">
        <FormattedEntry :entry="choice.flavor" />
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
        Use <i :class="`icon${choice.skillType}`"></i>: {{choice.label}}
      </a>
    </template>
  </div>
</template>

<style scoped>

a.button {
  display: block;
  background-color: #333;
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
  background-color: #532e61;
  border-radius: 0.6em;
  border: 0;
  color: #EEE;
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

  &[disabled] {
    cursor: not-allowed;
    background-color: #999 !important;
  }

  &::before {
    font-family: "ArkhamIcons";
    content: "\E91A";
    margin-right: 10px;
  }
}

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
</style>
