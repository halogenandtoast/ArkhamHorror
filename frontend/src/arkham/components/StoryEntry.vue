<script lang="ts" setup>
import { computed } from 'vue';
import { imgsrc, formatContent } from '@/arkham/helpers';
import { Game } from '@/arkham/types/Game';
import type { Read, FlavorTextEntry } from '@/arkham/types/Question';
import Token from '@/arkham/components/Token.vue';
import { useI18n } from 'vue-i18n';
import FormattedEntry from '@/arkham/components/FormattedEntry.vue';

export interface Props {
  game: Game
  question: Read
  playerId: string
}

const props = defineProps<Props>()
const emit = defineEmits(['choose'])
const choose = (idx: number) => emit('choose', idx)
const { t } = useI18n()

const format = function(body: string) {
  return formatContent(body)
}

const maybeFormat = function(body: string) {
  return body.startsWith("$") ? t(tformat(body.split(' ')[0])) : body
}

const readCards = computed(() => props.question.readCards || [])

const readChoices = computed(() => {
  switch (props.question.readChoices.tag) {
    case "BasicReadChoices":
      return props.question.readChoices.contents.reduce<{ label: string, index: number}[]>((acc, v, i) => {
        if ("label" in v) {
          return [...acc, { label: v.label, index: i }]
        }
        return acc
      }, [] as { label: string, index: number }[])

    case "LeadInvestigatorMustDecide":
      return props.question.readChoices.contents.reduce<{ label: string, index: number}[]>((acc, v, i) => {
        if ("label" in v) {
          return [...acc, { label: v.label, index: i }]
        }
        return acc
      }, [] as { label: string, index: number }[])
  }
})

const focusedChaosTokens = computed(() => props.game.focusedChaosTokens)

function formatEntry(entry: FlavorTextEntry): string {
  switch (entry.tag) {
    case 'BasicEntry': return formatContent(entry.text.startsWith('$') ? t(entry.text.slice(1)) : entry.text)
    case 'I18nEntry': return formatContent(t(entry.key, entry.variables))
    case 'ModifyEntry': return formatEntry(entry.entry)
    case 'CompositeEntry': return entry.entries.map(formatEntry).join(' ')
    default: return "Unknown entry type"
  }
}

const tformat = (t:string) => t.startsWith("$") ? t.slice(1) : t

</script>
<template>
  <div class="intro-text">
    <div class="entry">
      <h1 v-if="question.flavorText.title">{{maybeFormat(question.flavorText.title)}}</h1>
      <Token v-for="(focusedToken, index) in focusedChaosTokens" :key="index" :token="focusedToken" :playerId="playerId" :game="game" @choose="() => {}" />
      <div class="entry-body">
        <img :src="imgsrc(`cards/${cardCode.replace('c', '')}.avif`)" v-for="cardCode in readCards" class="card no-overlay" />
        <FormattedEntry v-for="(paragraph, index) in question.flavorText.body" :key="index" :entry="paragraph" />
      </div>
    </div>
    <div class="options">
      <button
        v-for="readChoice in readChoices"
        @click="choose(readChoice.index)"
        :key="readChoice.index"
        ><i class="option"></i><span v-html="formatContent(tformat(readChoice.label))"></span></button>
    </div>
  </div>
</template>

<style lang="scss" scoped>
.entry {
  background: #DCD6D0;
  padding: 20px;
  box-shadow: inset 0 0 170px rgba(0,0,0,0.5), 1px 1px 3px rgba(0,0,0,0.6);
}

.entry {
  &:has(.iceAndDeath) {
    h1 {
      color: #19214F;
      border-bottom: 1px solid #19214F;
      &::after {
        border-bottom: 1px solid #19214F;
      }
    }

    :deep(li){
      &::marker {
        color: #19214F;
      }
    }
  }
  &:has(.checkpoint), &:has(.interlude) {
    background-color: #AFA9A9;
    box-shadow: unset;
    overflow: hidden;
    &::after {
      border: 20px solid #D4CCC3;
      border-left-width: 10px;
      border-right-width: 10px;
      position: absolute;
      inset: 0px;
      box-sizing: border-box;
      content: "";
      filter: blur(0.25em);
      z-index: 1;
    }
    h1 {
      color: #19214F;
      border-bottom: 1px solid #19214F;
      &::after {
        border-bottom: 1px solid #19214F;
      }
      font-size: 1.3em;
      font-weight: 500;
    }
    padding: 50px;
    position: relative;
    &::before {
      z-index: 2;
      pointer-events: none;
      position: absolute;
      inset: 10px;
      border-image: url(/img/arkham/checkpoint_fleur.png) 49.9%;
      border-image-repeat: no-repeat;
      border-image-width: 50px;
      content: "";
    }
  }
}

.intro-text {
  color: #333;
  font-size: 1.5em;
  overflow: auto;
  height: 100%;
  padding: 20px;
  margin-bottom: 20px;
  :deep(h1) {
    font-family: "Teutonic";
    font-weight: 500;
    color: #38615F;
    margin: 0;
    padding-bottom: 2px;
    margin-bottom: 10px;
    border-bottom: 1px solid #38615f;
    &::after {
      display: block;
      content: " ";
      margin-top: 2px;
      border-bottom: 1px solid #38615f;
    }
  }

  :deep(p) {
    margin: 10px;
    font-style: italic;
  }
}

p {
  font-family: "ArkhamFlavor";
  :deep(i) {
    font-family: "ArkhamCursive";
    display: block;
    font-size: 1.3em;
    line-height: 0.3em;
    &:last-child {
      padding-bottom: 1.3em;
    }
  }
}

button, a.button {
  width: 100%;
  border: 0;
  text-align: left;
  padding: 10px;
  text-transform: uppercase;
  background-color: #532e61;
  font-weight: bold;
  color: #CFCFCF;
  font: Arial, sans-serif;
  &:hover {
    background-color: #311b3e;
    &:deep(strong) {
      color: white;
    }
  }

  i {
    font-style: normal;
  }

  &:deep(strong) {
    color: white;
  }
}

a.button {
  display: block;
  font-size: 0.7em;
}


.option {
  &:before {
    font-family: "ArkhamIcons";
    content: "\E91A";
    margin-right: 10px;
  }
}

.card {
  flex-basis: 30%;
  flex-shrink: 0;
  height: fit-content;
  border-radius: 15px;
}

.entry-text {
  flex: 1;
  &:deep(.right) {
    text-align: right;
  }
  &:deep(.basic) {
    font-style: normal;
    font-family: auto;
  }

  &:deep(i) {
    font-style: italic;
  }
}

.entry-body {
  width: 100%;
  display: flex;
  flex-direction: column;
  p {
    flex: 1;
  }

  &:has(.card) {
    flex-direction: row;
    gap: 20px;
  }
}

</style>
