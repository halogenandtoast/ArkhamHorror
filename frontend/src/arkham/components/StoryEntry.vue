<script lang="ts" setup>
import { computed } from 'vue';
import { imgsrc, formatContent } from '@/arkham/helpers';
import { Game } from '@/arkham/types/Game';
import type { Read } from '@/arkham/types/Question';
import type { FlavorText } from '@/arkham/types/FlavorText';
import { MessageType } from '@/arkham/types/Message';
import Token from '@/arkham/components/Token.vue';
import { useI18n } from 'vue-i18n';
import FormattedEntry from '@/arkham/components/FormattedEntry.vue';
import CardImage from '@/arkham/components/CardImage.vue';

export interface Props {
  game: Game
  question: Read
  playerId: string
}

const grunge = `url(${imgsrc('grunge.png')})`
const checkpoint_fleur = `url(${imgsrc('checkpoint_fleur.png')})`
const resolution_fleur = `url(${imgsrc('resolution_fleur.png')})`
const black_fleur = `url(${imgsrc('black_fleur.png')})`
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
    return [...acc, { cardCode: v.cardCode, flippable: "flippable" in v ? v.flippable : false, index: i }]
  }
  return acc
}, [] as { cardCode: string, index: number }[]))

type ReadChoice =
  | { tag: "Label", label: string, index: number }
  | { tag: "InvalidLabel", label: string, index: number }
  | { tag: "Info", flavor: FlavorText}

const readChoices = computed(() => {
  switch (props.question.readChoices.tag) {
    case "BasicReadChoices":
      return props.question.readChoices.contents.reduce<ReadChoice[]>((acc, v, i) => {
        if ("label" in v) {
          if (v.tag === MessageType.LABEL) {
            return [...acc, { tag: "Label", label: v.label, index: i }]
          }
          if (v.tag === MessageType.INVALID_LABEL) {
            return [...acc, { tag: "InvalidLabel", label: v.label, index: i }]
          }
        }
        if ("flavorText" in v) {
          return [...acc, { tag: "Info", flavor: v.flavorText }]
        }
        return acc
      }, [] as ReadChoice[])

    case "LeadInvestigatorMustDecide":
      return props.question.readChoices.contents.reduce<ReadChoice[]>((acc, v, i) => {
        if ("label" in v) {
          if (v.tag === MessageType.LABEL) {
            return [...acc, { tag: "Label", label: v.label, index: i }]
          }
          if (v.tag === MessageType.INVALID_LABEL) {
            return [...acc, { tag: "InvalidLabel", label: v.label, index: i }]
          }
        }
        if ("flavorText" in v) {
          return [...acc, { tag: "Info", flavor: v.flavorText }]
        }
        return acc
      }, [] as ReadChoice[])
  }
})

const focusedChaosTokens = computed(() => props.game.focusedChaosTokens)

const flippableCard = (cardCode: string) => {
  return {
    cardCode,
    doubleSided: true,
    classSymbols: [],
    cardType: 'UnknownType',
    art: cardCode.replace('c', ''),
    level: 0,
    traits: [],
    name: "",
    skills: [],
    cost: null,
    otherSide: `${cardCode}b`
  }
}
</script>
<template>
  <div class="intro-text">
    <div class="entry">
      <h1 v-if="question.flavorText.title">{{maybeFormat(question.flavorText.title)}}</h1>
      <section v-if="focusedChaosTokens.length > 0" class="focused-tokens">
        <Token v-for="(focusedToken, index) in focusedChaosTokens" :key="index" :token="focusedToken" :playerId="playerId" :game="game" @choose="() => {}" />
      </section>
      <div class="entry-body">
        <img :src="imgsrc(`cards/${cardCode.replace('c', '')}.avif`)" v-for="cardCode in readCards" class="card no-overlay" />
        <FormattedEntry v-for="(paragraph, index) in question.flavorText.body" :key="index" :entry="paragraph" />
      </div>
      <div class="pick-cards" v-if="pickCards.length > 0">
        <template v-for="card in pickCards" :key="card.index">
          <CardImage v-if="card.flippable" :card="flippableCard(card.cardCode)" class="no-overlay pick" @click="choose(card.index)" />
          <img v-else :src="imgsrc(`cards/${card.cardCode.replace('c', '')}.avif`)" class="card no-overlay pick" @click="choose(card.index)" />
        </template>
      </div>
    </div>
    <div class="options">
      <template v-for="readChoice in readChoices" :key="readChoice.index">
        <button
          v-if="readChoice.tag === 'InvalidLabel'"
          disabled
          ><i class="option"></i><span v-html="formatContent(maybeFormat(readChoice.label))"></span></button>
        <button
          v-else-if="readChoice.tag === 'Label'"
          @click="choose(readChoice.index)"
          ><i class="option"></i><span v-html="formatContent(maybeFormat(readChoice.label))"></span></button>
      </template>
    </div>
  </div>
</template>

<style scoped>
.entry {
  border-radius: 5px;
  background: #DCD6D0;
  padding: 20px;
  box-shadow: inset 0 0 170px rgba(0,0,0,0.5), 1px 1px 3px rgba(0,0,0,0.6);


  &:has(.resolution) {
    box-shadow: unset;
    background-color: #BAA597;
  }
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
  &:has(.black) {
    background-image: v-bind(grunge);
    background-size: cover;
    max-height: min-content;
  }
}

.hunted, :deep(.hunted) {
  display: grid;
  &:has(> div:first-child > img) {
    grid-template-columns: 1fr 2fr;
  }
  &:has(> div:last-child > img) {
    grid-template-columns: 2fr 1fr;
  }
  max-height: min-content;
  --image-top: 60px;

  div:first-child:has(img) {
    width: 100%;
    margin-top: var(--image-top);
    max-height: min-content;
    display: flex;
    flex-direction: column;
    img {
      object-fit: cover;
      height: calc(80vh - var(--image-top));
      align-self: flex-end;
    }
  }

  div:has(~ div:last-child > img) {
    margin-left: 20px;
  }

  div:last-child:has(img) {
    width: 100%;
    margin-top: var(--image-top);
    max-height: min-content;
    display: flex;
    flex-direction: column;
    img {
      object-fit: cover;
      height: calc(80vh - var(--image-top));
      align-self: flex-end;
    }
  }
}

.black, :deep(.black) {
  background-color: rgba(0,0,0,0.1);
  box-shadow: unset;
  margin-right: 20px;
  border: 5px solid #000;
  border-radius: 60px;
  &::after {
    border: 5px solid rgba(0,0,0,0.3);
    border-radius: 25px;
    position: absolute;
    inset: 0px;
    box-sizing: border-box;
    content: "";
    filter: blur(0.25em);
    z-index: 1;
  }
  h1 {
    color: #000 !important;
    font-family: "Unquiet Spirits", sans-serif !important;
    font-weight: 500;
    border: 0 !important;
    text-transform: uppercase;
    text-align: center;
    &::after {
      border: 0 !important;
    }
  }
  padding: 50px;
  position: relative;
  &::before {
    z-index: 2;
    pointer-events: none;
    position: absolute;
    inset: 0px;
    inset-block: -5px;
    inset-inline: -25px;
    border-image-source: v-bind(black_fleur);
    border-image-slice: 49.9%;
    border-image-repeat: no-repeat;
    border-image-width: 90px;
    content: "";
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

  &[disabled] {
    cursor: not-allowed;
    background-color: #999 !important;
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
    flex-wrap: wrap;
  }

  @media (max-width: 600px) {
    &:has(.card) {
      flex-direction: column;
      align-items: center;
      gap: 20px;
    }
  }

  :deep(span.wolgast) {
    font-family: "Wolgast";
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

  :deep(div:has(> img)) {
    width: 25%;
    min-width: min(300px, 25vw);
    flex: 0;
    img {
      border-radius: 4%;
    }
  }

  &:deep(.resolution) {
    padding: 40px;
  }
  &:has(.resolution) {
    background: #BAA898;
  }

  &:has(.resolution) {
    background-color: #BAA597;
    box-shadow: unset;
    overflow: auto;
    isolation: isolate;
    position: relative;
    &::after {
      border: 20px solid #D4CCC3;
      border-left-width: 10px;
      border-right-width: 10px;
      position: absolute;
      inset: 0px;
      box-sizing: border-box;
      content: "";
      filter: blur(0.25em);
      z-index: -2;
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
    &::before {
      z-index: -1;
      pointer-events: none;
      position: absolute;
      inset: 10px;
      border-image-source: v-bind(resolution_fleur);
      border-image-slice: 49.9%;
      border-image-repeat: no-repeat;
      border-image-width: 50px;
      content: "";
    }
    @media (max-width: 800px) and (orientation: portrait)  {
      padding: 10px;
      &::before {
        border-image-width: 20px;
      }
    }
  }

}

.pick-cards {
  display: flex;
  margin-block: 20px;
  gap: 10px;
  justify-content: center;

  img, .card-container {
    transition: filter 0.1s ease-in-out, transform 0.1s ease-in-out;
    border-radius: 4%;
    flex: 1;
    max-width: min(500px, 30vw);
  }

  img:hover, .card-container:hover {
    filter: drop-shadow(0 0 10px rgba(0,0,0,0.9));
    transform: scale(1.05);
  }

  .card-container {
    width: clamp(200px, 25vw, 400px);
    max-width: fit-content;
  }
}

.pick, :deep(.pick) {
  cursor: pointer;
  flex-basis: 20%;
}

.focused-tokens {
  display: flex;
  justify-content: center;
  gap: 10px;
  margin-bottom: 20px;
  background-color: rgba(0, 0, 0, 0.6);
  border: 1px solid rgba(0, 0, 0, 0.9);
  padding: 10px;
  border-radius: 10px;
}

:deep(.card) {
  border-radius: 10px;
  width: clamp(200px, 25vw, 400px);
}

:deep(div) > :has(.card) {
  flex-shrink: 0;
}
</style>
