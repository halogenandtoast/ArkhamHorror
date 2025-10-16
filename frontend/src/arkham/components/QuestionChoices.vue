<script lang="ts" setup>
import AbilityButton from '@/arkham/components/AbilityButton.vue'
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
</template>

<style scoped>

.button {
  display: inline-block;
  padding: 5px 10px;
  margin: 2px;
  background-color: #333;
  color: white;
  border: 1px solid #666;
  cursor: pointer;
}

.button:hover {
  background-color: #111;
}

.button:active {
  background-color: #666;
  border-color: #111;
}

button {
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

button:hover {
  background-color: #311b3e;

  &[disabled] {
    background-color: #999 !important;
  }
}
</style>
