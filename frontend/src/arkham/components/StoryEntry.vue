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

const checkpoint_fleur = `url(${imgsrc('checkpoint_fleur.png')})`
const props = defineProps<Props>()
const emit = defineEmits(['choose'])
const choose = (idx: number) => emit('choose', idx)
const { t } = useI18n()

const maybeFormat = function(body: string) {
  return body.startsWith("$") ? t(tformat(body.split(' ')[0])) : body
}

const tformat = (t:string) => t.startsWith("$") ? t.slice(1) : t

const readCards = computed(() => props.question.readCards ?? [])

const pickCards = computed(() => props.question.readChoices.contents.reduce((acc, v, i) => {
  if ("cardCode" in v) {
    return [...acc, { cardCode: v.cardCode, index: i }]
  }
  return acc
}, [] as { cardCode: string, index: number }[]))

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
      <div class="pick-cards" v-if="pickCards.length > 0">
        <img :src="imgsrc(`cards/${card.cardCode.replace('c', '')}.avif`)" v-for="card in pickCards" class="card no-overlay pick" :key="card.index" @click="choose(card.index)" />
      </div>
    </div>
    <div class="options">
      <button
        v-for="readChoice in readChoices"
        @click="choose(readChoice.index)"
        :key="readChoice.index"
        ><i class="option"></i><span v-html="formatContent(maybeFormat(readChoice.label))"></span></button>
    </div>
  </div>
</template>

<style lang="scss" scoped>
.entry {
  border-radius: 5px;
  background: #DCD6D0;
  padding: 20px;
  box-shadow: inset 0 0 170px rgba(0,0,0,0.5), 1px 1px 3px rgba(0,0,0,0.6);
}

.entry {
  h1 { font-size: 1.3em; }
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
      font-weight: 500;
    }
    padding: 50px;
    position: relative;
    &::before {
      z-index: 2;
      pointer-events: none;
      position: absolute;
      inset: 10px;
      border-image-source: v-bind(checkpoint_fleur);
      border-image-slice: 49.9%;
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
  :deep(div.wolgast) {
    font-family: "Wolgast";
    text-align: center;
    margin-block: 30px;
    p {
      margin-block: -20px;
      font-family: "Wolgast";
    }
  }
  :deep(div.anke) {
    font-family: "Anke";
    text-align: center;
    margin-block: 30px;
    p {
      margin-block: -20px;
      font-family: "Anke";
    }
  }
  :deep(p.anke) {
    font-family: "Anke";
  }
}

.pick-cards {
  display: flex;
  margin-block: 20px;
  gap: 10px;
  justify-content: center;

  img {
    transition: box-shadow 0.1s ease-in-out, transform 0.1s ease-in-out;
  }

  img:hover {
    box-shadow: 0 0 10px rgba(0,0,0,0.9);
    transform: scale(1.05);
  }
}

.pick {
  cursor: pointer;
  flex-basis: 20%;
}

</style>
