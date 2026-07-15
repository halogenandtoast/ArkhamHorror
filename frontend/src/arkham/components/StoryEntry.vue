<script lang="ts" setup>
import { computed, ref, watch } from 'vue';
import { imgsrc, formatContent } from '@/arkham/helpers';
import { cardArt, cardImage } from '@/arkham/cardImages';
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
const checkpoint_fleur = `url(${imgsrc('fleurs/checkpoint_fleur.png')})`
const resolution_fleur = `url(${imgsrc('fleurs/resolution_fleur.png')})`
const black_fleur = `url(${imgsrc('fleurs/black_fleur.png')})`
const props = defineProps<Props>()
const emit = defineEmits(['choose'])
const choose = (idx: number) => emit('choose', idx)
const { t } = useI18n()

const maybeFormat = function(body: string) {
  return body.startsWith("$") ? t(tformat(body.split(' ')[0])) : body
}

const tformat = (t:string) => t.startsWith("$") ? t.slice(1) : t

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
  const localized = maybeFormat(body)
  return drownedCityTaskNames[localized] ?? null
}

const drownedCityTaskCardCode = (body: string) => {
  const key = drownedCityTaskKey(body)
  return key ? drownedCityTaskCards[key] : undefined
}

const drownedCityTaskRecommendation = (body: string) => {
  const key = drownedCityTaskKey(body)
  return key ? t(`theDrownedCity.anOfferYouCantRefuse.recommended.${key}`) : null
}

const selectedTaskChoice = ref<{ index: number; label: string; cardCode: string; canConfirm: boolean } | null>(null)

const selectDrownedCityTask = (choice: ReadChoice) => {
  if (!('label' in choice)) return false
  const cardCode = drownedCityTaskCardCode(choice.label)
  if (!cardCode) return false
  selectedTaskChoice.value = {
    index: choice.index,
    label: choice.label,
    cardCode,
    canConfirm: choice.tag === 'Label',
  }
  return true
}

const handleChoice = (choice: ReadChoice) => {
  if (selectDrownedCityTask(choice)) return
  if (choice.tag === 'Label') choose(choice.index)
}

const confirmDrownedCityTask = () => {
  if (selectedTaskChoice.value?.canConfirm) choose(selectedTaskChoice.value.index)
}

watch(() => props.question, () => {
  selectedTaskChoice.value = null
})

const readCards = computed(() => props.question.readCards ?? [])

type PickCardChoice = { cardCode: string; flippable: boolean; index: number }
const pickCards = computed(() => props.question.readChoices.contents.reduce<PickCardChoice[]>((acc, v, i) => {
  if ("cardCode" in v && typeof v.cardCode === 'string') {
    return [...acc, { cardCode: v.cardCode, flippable: "flippable" in v ? Boolean(v.flippable) : false, index: i }]
  }
  return acc
}, []))

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
        if (v.tag === MessageType.INFO) {
          return [...acc, { tag: "Info", flavor: v.flavor }]
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
        if (v.tag === MessageType.INFO) {
          return [...acc, { tag: "Info", flavor: v.flavor }]
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
    art: cardArt(cardCode),
    level: 0,
    cardTraits: [],
    name: { title: '', subtitle: null },
    skills: [],
    cost: null,
    otherSide: `${cardCode}b`,
    meta: {},
    errata: null
  }
}
</script>
<template>
  <div class="intro-text">
    <div class="entry-row" :class="{ 'task-layout': readChoices.some((choice) => 'label' in choice && !!drownedCityTaskCardCode(choice.label)) }">
      <div class="entry">
        <h1 v-if="question.flavorText.title">{{maybeFormat(question.flavorText.title)}}</h1>
        <section v-if="focusedChaosTokens.length > 0" class="focused-tokens">
          <Token v-for="(focusedToken, index) in focusedChaosTokens" :key="index" :token="focusedToken" :playerId="playerId" :game="game" @choose="() => {}" />
        </section>
        <div class="entry-body">
          <img :src="cardImage(cardCode)" v-for="cardCode in readCards" class="card no-overlay" />
          <FormattedEntry v-for="(paragraph, index) in question.flavorText.body" :key="index" :entry="paragraph" />
        </div>
        <div class="pick-cards" v-if="pickCards.length > 0">
          <template v-for="card in pickCards" :key="card.index">
            <CardImage v-if="card.flippable" :card="flippableCard(card.cardCode)" class="no-overlay pick" @click="choose(card.index)" />
            <img v-else :src="cardImage(card.cardCode)" class="card no-overlay pick" @click="choose(card.index)" />
          </template>
        </div>
      </div>
      <div class="task-selection">
        <div class="task-row">
        <aside v-if="readChoices.some((choice) => 'label' in choice && !!drownedCityTaskCardCode(choice.label))" class="task-card-panel">
          <div class="task-card-frame" :class="{ empty: !selectedTaskChoice }">
            <img
              v-if="selectedTaskChoice"
              :src="cardImage(selectedTaskChoice.cardCode)"
              class="no-overlay task-card-image"
              :alt="maybeFormat(selectedTaskChoice.label)"
            />
            <span v-else>Select a task to preview its card.</span>
          </div>
        </aside>
        <div class="options">
          <template v-for="(readChoice, choiceIndex) in readChoices" :key="readChoice.tag === 'Info' ? `info-${choiceIndex}` : readChoice.index">
            <div class="choice-wrapper">
              <button
                v-if="readChoice.tag === 'InvalidLabel'"
                :class="{ 'task-choice': drownedCityTaskCardCode(readChoice.label), selected: selectedTaskChoice?.index === readChoice.index }"
                :disabled="!drownedCityTaskCardCode(readChoice.label)"
                @click="selectDrownedCityTask(readChoice)"
                >
                <i class="option"></i>
                <span class="choice-content">
                  <span class="choice-label" v-html="formatContent(maybeFormat(readChoice.label))"></span>
                  <span
                    v-if="drownedCityTaskRecommendation(readChoice.label)"
                    class="choice-subtext"
                    v-html="formatContent(drownedCityTaskRecommendation(readChoice.label) ?? '')"
                  ></span>
                </span>
              </button>
              <button
                v-else-if="readChoice.tag === 'Label'"
                :class="{ 'task-choice': drownedCityTaskCardCode(readChoice.label), selected: selectedTaskChoice?.index === readChoice.index }"
                @click="handleChoice(readChoice)"
                >
                <i class="option"></i>
                <span class="choice-content">
                  <span class="choice-label" v-html="formatContent(maybeFormat(readChoice.label))"></span>
                  <span
                    v-if="drownedCityTaskRecommendation(readChoice.label)"
                    class="choice-subtext"
                    v-html="formatContent(drownedCityTaskRecommendation(readChoice.label) ?? '')"
                  ></span>
                </span>
              </button>
            </div>
          </template>
        </div>
        </div>
        <button
          v-if="selectedTaskChoice"
          class="confirm-task-button"
          :disabled="!selectedTaskChoice.canConfirm"
          @click="confirmDrownedCityTask"
        >Confirm</button>
      </div>
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
      z-index: var(--z-index-1);
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
      z-index: var(--z-index-2);
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
  &:has(.haunted) {
    background:
      radial-gradient(ellipse at 50% 30%, #838938 0%, #5a5e22 70%, #3a3d16 100%);
    background-image: v-bind(grunge), radial-gradient(ellipse at 50% 30%, #838938 0%, #5a5e22 70%, #3a3d16 100%);
    background-blend-mode: overlay;
    background-size: cover;
    box-shadow:
      inset 0 0 120px rgba(31, 33, 12, 0.55),
      inset 0 0 60px rgba(66, 69, 28, 0.35),
      0 0 40px rgba(66, 69, 28, 0.45),
      0 0 80px rgba(66, 69, 28, 0.25);
    color: #c1c49c;
    border: 1px solid rgba(66, 69, 28, 0.5);
    position: relative;
    overflow: hidden;
    isolation: isolate;
    animation: haunted-flicker 7s ease-in-out infinite;

    &::before {
      content: "";
      position: absolute;
      inset: 0;
      pointer-events: none;
      background:
        radial-gradient(circle at 20% 80%, rgba(131, 137, 56, 0.12), transparent 40%),
        radial-gradient(circle at 80% 20%, rgba(131, 137, 56, 0.08), transparent 40%),
        radial-gradient(circle at 50% 100%, rgba(0, 0, 0, 0.7), transparent 60%);
      z-index: var(--z-index-0);
    }

    &::after {
      content: "";
      position: absolute;
      inset: 6px;
      pointer-events: none;
      border: 1px solid rgba(131, 137, 56, 0.25);
      box-shadow:
        inset 0 0 40px rgba(131, 137, 56, 0.15),
        inset 0 0 4px rgba(131, 137, 56, 0.35);
      z-index: var(--z-index-0);
    }

    h1 {
      color: #c9d2a8;
      text-shadow:
        0 0 8px rgba(131, 137, 56, 0.65),
        0 0 18px rgba(66, 69, 28, 0.55),
        0 2px 2px rgba(0, 0, 0, 0.9);
      letter-spacing: 0.08em;
      border-bottom-color: rgba(131, 137, 56, 0.4) !important;
      animation: haunted-title-pulse 4.5s ease-in-out infinite;
      position: relative;
      z-index: var(--z-index-1);
      &::after {
        border-bottom-color: rgba(131, 137, 56, 0.4) !important;
      }
    }

    .entry-body {
      position: relative;
      z-index: var(--z-index-1);
    }
  }
}

@keyframes haunted-flicker {
  0%, 100% { filter: brightness(1); }
  3% { filter: brightness(0.78); }
  6% { filter: brightness(1.05); }
  9% { filter: brightness(0.85); }
  12% { filter: brightness(1); }
  62% { filter: brightness(1); }
  64% { filter: brightness(0.7); }
  66% { filter: brightness(1.02); }
  68% { filter: brightness(1); }
}

@keyframes haunted-title-pulse {
  0%, 100% {
    text-shadow:
      0 0 8px rgba(131, 137, 56, 0.65),
      0 0 18px rgba(66, 69, 28, 0.55),
      0 2px 2px rgba(0, 0, 0, 0.9);
  }
  50% {
    text-shadow:
      0 0 14px rgba(131, 137, 56, 0.95),
      0 0 28px rgba(66, 69, 28, 0.85),
      0 0 50px rgba(66, 69, 28, 0.45),
      0 2px 2px rgba(0, 0, 0, 0.9);
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
    z-index: var(--z-index-1);
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
    z-index: var(--z-index-2);
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
  color: var(--neutral-dark);
  font-size: 1.5em;
  overflow: auto;
  height: 100%;
  padding: 20px;
  margin-bottom: 20px;
  :deep(h1) {
    font-family: "Teutonic";
    font-weight: 500;
    color: var(--green-title);
    margin: 0;
    padding-bottom: 2px;
    margin-bottom: 10px;
    border-bottom: 1px solid var(--green-title);
    &::after {
      display: block;
      content: " ";
      margin-top: 2px;
      border-bottom: 1px solid var(--green-title);
    }
  }

  :deep(p) {
    margin: 10px;
    font-style: italic;
  }

}

.entry-row {
  display: block;
}

.entry-row.task-layout {
  display: block;
}

.entry-row .entry {
  min-width: 0;
}

.task-selection {
  margin-top: 20px;
}

.entry-row.task-layout .task-row {
  align-items: flex-start;
  display: flex;
  gap: 16px;
  width: 100%;
}

.entry-row.task-layout .options {
  flex: 1 1 auto;
  min-width: 0;
}

.task-card-panel {
  flex: 0 0 260px;
  position: sticky;
  top: 0;
}

.task-card-frame {
  align-items: center;
  aspect-ratio: 0.714;
  background: rgba(17, 13, 20, 0.72);
  border: 2px solid rgba(220, 214, 208, 0.45);
  border-radius: 13px;
  box-shadow: 0 12px 28px rgba(0, 0, 0, 0.35), inset 0 0 0 1px rgba(255, 255, 255, 0.08);
  color: #d8d0df;
  display: flex;
  font-family: Arial, sans-serif;
  font-size: 0.72em;
  font-weight: 700;
  justify-content: center;
  line-height: 1.2;
  padding: 10px;
  text-align: center;
}

.task-card-frame.empty {
  border-style: dashed;
}

.task-card-frame:not(.empty) {
  overflow: hidden;
  padding: 0;
}

.task-card-image {
  border-radius: 10px;
  display: block;
  height: 100%;
  object-fit: cover;
  width: 100%;
}

.confirm-task-button {
  margin-top: 10px;
  background-color: #2f6141;
  &:hover { background-color: #3c7d54; }
}

@media (max-width: 780px) {
  .entry-row.task-layout .task-row {
    flex-direction: column;
  }

  .task-card-panel {
    position: static;
    width: min(260px, 100%);
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
  background-color: var(--button-2);
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

  &.task-choice {
    display: flex;
    align-items: flex-start;
    gap: 10px;
    transition: none;
    &:hover { transform: none; }
  }

  &.selected {
    background-color: #241430;
    box-shadow: inset 0 0 0 2px #d8d0df;
    &:hover { background-color: #241430; }
  }
}

/* Buttons here are width:100%, so the global scale(0.97) press bounce grows
   with viewport width. Ramp the scale toward 1 on wider screens to keep the
   pixel bounce roughly constant. */
button:active:not(:disabled) {
  transform: scale(0.985);
}
@media (min-width: 900px) {
  button:active:not(:disabled) { transform: scale(0.99); }
}
@media (min-width: 1400px) {
  button:active:not(:disabled) { transform: scale(0.994); }
}

.task-choice .choice-content,
.task-choice .choice-label {
  display: block;
}

.task-choice .choice-content {
  flex: 1 1 auto;
}

.task-choice .choice-label {
  font-size: 1.15em;
}

.choice-subtext {
  color: #d8d0df;
  display: block;
  font-size: 0.95em;
  line-height: 1.3;
  margin-top: 6px;
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

.task-choice .option:before {
  margin-right: 0;
}

.card {
  flex-basis: 30%;
  flex-shrink: 0;
  height: fit-content;
  border-radius: 15px;
}

.entry-body {
  width: 100%;
  display: flex;
  flex-direction: column;
  p {
    flex: 1;
  }

  img {
    height: auto;
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

  :deep(p.indent) {
    margin-left: 2em;
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
      z-index: var(--z-index-neg-2);
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
      z-index: var(--z-index-neg-1);
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
