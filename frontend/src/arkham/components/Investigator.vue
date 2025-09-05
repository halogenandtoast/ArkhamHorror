<script lang="ts" setup>
import { useSettings } from '@/stores/settings';
import { storeToRefs } from 'pinia';
import { onMounted, computed, ref, watch } from 'vue'
import Draggable from '@/components/Draggable.vue';
import CardView from '@/arkham/components/Card.vue';
import Modifiers from '@/arkham/components/Modifiers.vue';
import { useDebug } from '@/arkham/debug'
import { PaperClipIcon } from '@heroicons/vue/20/solid'
import type { Game } from '@/arkham/types/Game'
import { imgsrc } from '@/arkham/helpers'
import * as ArkhamGame from '@/arkham/types/Game';
import * as Arkham from '@/arkham/types/Investigator'
import type { AbilityLabel, AbilityMessage, Message } from '@/arkham/types/Message'
import { MessageType } from '@/arkham/types/Message'
import type { Modifier } from '@/arkham/types/Modifier'
import Token from '@/arkham/components/Token.vue';
import AbilityButton from '@/arkham/components/AbilityButton.vue'
import { useMenu } from '@/composeable/menu';
import { useI18n } from 'vue-i18n';
import useEmitter from '@/composeable/useEmitter';
import Resources from '@/arkham/components/Resources.vue';
import Draw from '@/arkham/components/Draw.vue';
import { IsMobile } from '@/arkham/isMobile';
const { t } = useI18n();

export interface Props {
  choices: Message[]
  investigator: Arkham.Investigator
  playerId: string
  game: Game
  portrait?: boolean
}

const props = withDefaults(defineProps<Props>(), { portrait: false })
const emit = defineEmits(['showCards', 'choose'])

const id = computed(() => props.investigator.id)
const debug = useDebug()
const choose = (idx: number) => emit('choose', idx)

const { addEntry, removeEntry } = useMenu()
const settingsStore = useSettings()
const { toggleShowBonded } = settingsStore
const { showBonded } = storeToRefs(settingsStore)
const { isMobile } = IsMobile();

const doShowBonded = computed(() => {
  return showBonded.value && props.playerId == props.investigator.playerId
})

watch(() => props.playerId, () => {
  if (!props.portrait) {
    if (props.playerId == props.investigator.playerId) {
      addEntry({
        id: `viewBonded-${props.investigator.playerId}`,
        icon: PaperClipIcon,
        content: t('gameBar.viewBonded'),
        shortcut: "b",
        nested: 'view',
        action: () => toggleShowBonded()
      })
    } else {
      removeEntry(`viewBonded-${props.investigator.playerId}`)
    }
  }
}, { immediate: true })

function canActivateAbility(c: Message): boolean {
  if (c.tag  === MessageType.ABILITY_LABEL) {
    if ("contents" in c.ability.source) {
      return c.ability.source.contents === id.value
    }
  }
  return false
}
const activateAbilityAction = computed(() => props.choices.findIndex(canActivateAbility))

const labelAction = computed(() => {
  return props.choices
    .findIndex((c) => c.tag === MessageType.TARGET_LABEL
      && c.target.tag === "InvestigatorTarget" && c.target.contents === id.value)
})

const investigatorAction = computed(() => {
  if (labelAction.value !== -1) {
    return labelAction.value
  }

  return activateAbilityAction.value
})

const choices = computed(() => ArkhamGame.choices(props.game, props.playerId))

function isAbility(v: Message): v is AbilityLabel {
  if (v.tag !== MessageType.ABILITY_LABEL) {
    return false
  }

  const { source } = v.ability;

  if (source.sourceTag === 'ProxySource') {
    if ("contents" in source.source) {
      return source.source.contents === id.value
    }
  } else if (source.tag === 'InvestigatorSource') {
    return source.contents === id.value
  }

  return false
}

const abilities = computed(() => {
  return choices
    .value
    .reduce<AbilityMessage[]>((acc, v, i) => {
      if (isAbility(v)) {
        return [...acc, { contents: v, displayAsAction: false, index: i }];
      }

      return acc;
    }, []);
})

const endTurnAction = computed(() => {
  return props.choices
    .findIndex((c) => c.tag === MessageType.END_TURN_BUTTON && c.investigatorId === id.value);
})

const skipTriggersAction = computed(() => {
  return props.choices
    .findIndex((c) => c.tag === MessageType.SKIP_TRIGGERS_BUTTON && c.investigatorId === id.value);
})

const image = computed(() => {
  if (props.investigator.form.tag === 'YithianForm') {
    return imgsrc("cards/04244.avif");
  }

  if (props.investigator.form.tag === 'HomunculusForm') {
    return imgsrc("cards/11068b.avif");
  }

  if (props.investigator.form.tag === "TransfiguredForm") {
    return imgsrc(`cards/${props.investigator.form.contents.replace('c', '')}.avif`)
  }

  const mutated = props.investigator.mutated ? `_${props.investigator.mutated}` : ''
  const classVariant = props.investigator.cardCode === 'c03006' && props.investigator.class !== 'Neutral' ? `_${props.investigator.class}` : ''
  return imgsrc(`cards/${props.investigator.art.replace('c', '')}${classVariant}${mutated}.avif`);
})

const portraitImage = computed(() => {
  if (props.investigator.form.tag === "YithianForm") {
    return imgsrc(`portraits/${id.value.replace('c', '')}.jpg`)
  }

  if (props.investigator.form.tag === "HomunculusForm") {
    return imgsrc(`portraits/${id.value.replace('c', '')}.jpg`)
  }

  return imgsrc(`portraits/${props.investigator.cardCode.replace('c', '')}.jpg`)
})


const emitter = useEmitter()
const cardsUnderneath = computed(() => props.investigator.cardsUnderneath)
const cardsUnderneathLabel = computed(() => t('investigator.underneathCards', {count: cardsUnderneath.value.length}))
const devoured = computed(() => props.investigator.devoured)

onMounted(() => {
  emitter.on('showUnder', (id: string) => {
    if (id === props.investigator.id) {
      showCardsUnderneath(new Event('click'))
    }
  })
})


const showCardsUnderneath = (e: Event) => emit('showCards', e, cardsUnderneath, "Cards Underneath", false)
const showDevoured = (e: Event) => emit('showCards', e, devoured, "Devoured", false)

const modifiers = computed(() => props.investigator.modifiers)

const captured = computed(() => {
  return modifiers.value?.some((m) => m.type.tag === "ScenarioModifier" && m.type.contents === "captured") ?? false
})

const ethereal = computed(() => {
  return modifiers.value?.some((m) => m.type.tag === "UIModifier" && m.type.contents === "Ethereal") ?? false
})

function calculateSkill(base: number, skillType: string, modifiers: Modifier[]) {
  let modified = base

  modifiers.forEach((modifier) => {
    if (modifier.type.tag === "BaseSkillOf" && modifier.type.skillType === skillType) {
      modified = modifier.type.value
    }

    if (modifier.type.tag === "BaseSkill" && props.game.skillTest && props.game.skillTest.skills.includes(skillType)) {
      modified = modifier.type.contents
    }
  })

  modifiers.forEach((modifier) => {
    if (modifier.type.tag === "SkillModifier" && modifier.type.skillType === skillType) {
      modified = modified + modifier.type.value
    }

    if (modifier.type.tag === "AnySkillValue") {
      if (props.game.skillTest && props.game.skillTest.skills.includes(skillType)) {
        modified = modified + modifier.type.contents
      }
    }

    if (modifier.type.tag === "ActionSkillModifier" && modifier.type.skillType === skillType && props.game.skillTest && props.game.skillTest.action === modifier.type.action) {
      modified = modified + modifier.type.value
    }
  })

  modifiers.forEach((modifier) => {
    if (modifier.type.tag === "SetSkillValue" && modifier.type.skillType === skillType) {
      modified = modifier.type.value
    }
  })

  return modified
}

function useEffectAction(action: { contents: string[] }) {
  const choice = choices.value.findIndex((c) => c.tag === 'EffectActionButton' && c.effectId == action.contents[1])
  if (choice !== -1) {
    emit('choose', choice)
  }
}

function isActiveEffectAction(action: { tag?: "EffectAction"; contents: any }) {
  const choice = choices.value.findIndex((c) => c.tag === 'EffectActionButton' && c.effectId == action.contents[1])
  return choice !== -1
}

const willpower = computed(() => calculateSkill(props.investigator.willpower, "SkillWillpower", modifiers.value ?? []))
const intellect = computed(() => calculateSkill(props.investigator.intellect, "SkillIntellect", modifiers.value ?? []))
const combat = computed(() => calculateSkill(props.investigator.combat, "SkillCombat", modifiers.value ?? []))
const agility = computed(() => calculateSkill(props.investigator.agility, "SkillAgility", modifiers.value ?? []))

const dragging = ref(false)
const showModifiers = ref(false)
function startDrag(event: DragEvent) {
  dragging.value = true
  if (event.dataTransfer) {
    event.dataTransfer.effectAllowed = 'move'
    event.dataTransfer.setData('text/plain', JSON.stringify({ "tag": "InvestigatorTarget", "contents": id.value }))
  }
}

const dragover = (e: DragEvent) => {
  e.preventDefault()
  if (e.dataTransfer) {
    e.dataTransfer.dropEffect = 'move'
  }
}

function onDrop(event: DragEvent) {
  event.preventDefault()
  if (event.dataTransfer) {
    const data = event.dataTransfer.getData('text/plain')
    if (data) {
      const json = JSON.parse(data)
      if (json.tag === "KeyTarget") {
        debug.send(props.game.id, {tag: 'PlaceKey', contents: [{ tag: "InvestigatorTarget", contents: id.value }, { "tag": json.contents }]})
      }

      if (json.tag === "EnemyTarget") {
        debug.send(props.game.id, {tag: 'EnemyEngageInvestigator', contents: [json.contents, id.value]})
      }
    }
  }
}
</script>

<template>
  <div v-if="portrait" class="portrait-container">
    <span v-if="isMobile"><i class="action" v-for="n in investigator.remainingActions" :key="n"></i></span>
    <span v-if="isMobile && investigator.additionalActions.length > 0">
      <template v-for="action in investigator.additionalActions" :key="action">
        <button @click="useEffectAction(action)" v-if="action.tag === 'EffectAction'" v-tooltip="action.contents[0]" :class="[{ activeButton: isActiveEffectAction(action)}, `${investigator.class.toLowerCase()}ActionButton`]">
          <i class="action"></i>
        </button>
        <i v-else class="action" :class="`${investigator.class.toLowerCase()}Action`"></i>
      </template>
    </span>
    <img
      :src="portraitImage"
      class="portrait"
      :class="{ 'investigator--can-interact--portrait': investigatorAction !== -1, ethereal, dragging, captured }"
      :draggable="debug.active"
      @click="$emit('choose', investigatorAction)"
      @dragstart="startDrag($event)"
      @drop="onDrop($event)"
      @dragover.prevent="dragover($event)"
      @dragenter.prevent
    />
  </div>
  <div v-else class="player-container">
    <div class="player-area">
      <div class="player-card">
        <div class="stats">
          <div class="willpower willpower-icon">{{willpower}}</div>
          <div class="intellect intellect-icon">{{intellect}}</div>
          <div class="combat combat-icon">{{combat}}</div>
          <div class="agility agility-icon">{{agility}}</div>
        </div>
        <div class="investigator-image">
          <img
            :class="{ 'investigator--can-interact': investigatorAction !== -1 }"
            class="card card--sideways"
            :src="image"
            @click="$emit('choose', investigatorAction)"
            @drop="onDrop($event)"
            @dragover.prevent="dragover($event)"
            @dragenter.prevent
          />
          <Token v-for="(sealedToken, index) in investigator.sealedChaosTokens" :key="index" :token="sealedToken" :playerId="playerId" :game="game" @choose="choose" class="sealed" />
        </div>
      </div>
      <div>
        <div class="player-buttons">
          <span v-if="!isMobile" class="action-container"><i class="action" v-for="n in investigator.remainingActions" :key="n"></i></span>
          <span v-if="investigator.additionalActions.length > 0">
            <template v-for="action in investigator.additionalActions" :key="action">
            <button @click="useEffectAction(action)" v-if="action.tag === 'EffectAction'" v-tooltip="action.contents[0]" :class="[{ activeButton: isActiveEffectAction(action)}, `${investigator.class.toLowerCase()}ActionButton`]">
              <i class="action"></i>
            </button>
            <i v-else class="action" :class="`${investigator.class.toLowerCase()}Action`"></i>
            </template>
          </span>
          <template v-if="debug.active">
            <button
              @click.exact="debug.send(game.id, {tag: 'GainActions', contents: [id, {tag: 'TestSource', contents: []}, 1]})"
              @click.shift="debug.send(game.id, {tag: 'GainActions', contents: [id, {tag: 'TestSource', contents: []}, 5]})"
            >+</button>
          </template>
          <AbilityButton
            v-for="ability in abilities"
            :key="ability.index"
            :ability="ability.contents"
            :game="game"
            @click="$emit('choose', ability.index)"
            />
          <button
          :class="{ active: endTurnAction !== -1 && investigator.remainingActions === 0 }"
          :disabled="endTurnAction == -1"
          @click="$emit('choose', endTurnAction)"
          >{{ isMobile ? 'End' : $t('investigator.endTurn') }}</button>

          <button
            v-if="devoured && devoured.length > 0"
            @click="showDevoured"
          >{{ $t('investigator.devouredCards', {count: devoured.length}) }}</button>

          <button
            :disabled="skipTriggersAction == -1"
            @click="$emit('choose', skipTriggersAction)"
            class="skip-triggers-button"
          >{{ isMobile ? 'Skip' : $t('investigator.skipTriggers') }}</button>

          <button
            v-if="debug && debug.active && (investigator.modifiers ?? []).length > 0"
            @click="showModifiers = true"
            >Show Modifiers</button>

          <Modifiers v-if="showModifiers" :game="game" :modifiers="investigator.modifiers" @close="showModifiers = false" />

          <button v-if="cardsUnderneath.length > 0" class="view-discard-button" @click="showCardsUnderneath">{{cardsUnderneathLabel}}</button>
          <Draw
            v-if="isMobile"
            :game="game"
            :playerId="playerId"
            :investigator="investigator"
            @choose="$emit('choose', $event)"
          />
        </div>
        <Resources
          v-if="isMobile"
          :game="game"
          :investigator="investigator"
          :choices="choices"
          @choose="$emit('choose', $event)"
        />
      </div>
    </div>
    <Resources
      v-if="!isMobile"
      :game="game"
      :investigator="investigator"
      :choices="choices"
      @choose="$emit('choose', $event)"
    />

    <Draggable v-if="doShowBonded">
      <template #handle><header><h2>{{$t('gameBar.bonded')}}</h2></header></template>
      <div class="card-row-cards">
        <div v-for="card in investigator.bondedCards" :key="card.id" class="card-row-card">
          <CardView :game="game" :card="card" :playerId="playerId" />
        </div>
      </div>
      <button class="close button" @click="toggleShowBonded">{{$t('close')}}</button>
    </Draggable>
  </div>
</template>

<style scoped lang="scss">
i.action {
  font-family: 'Arkham';
  font-style: normal;
  font-weight: normal;
  font-variant: normal;
  text-transform: none;
  line-height: 1;
  -webkit-font-smoothing: antialiased;
  position: relative;
  color: #EEE;

  &:before {
    font-family: "Arkham";
    content: "\0049";
  }
}

.turn-info {
  display: flex;
  align-self: center;
  align-items: center;
}

.investigator--can-interact {
  border: 2px solid var(--select);
  cursor: pointer;
  &--portrait {
    cursor: pointer;
    border: 3px solid var(--select);
  }
}

.card {
  width: auto;
  height: var(--card-width);
}

.guardianAction {
  color: var(--guardian-extra-dark) !important;
}

.survivorAction {
  color: var(--survivor-extra-dark) !important;
}

.mysticAction {
  color: var(--mystic-extra-dark) !important;
}

.seekerAction {
  color: var(--seeker-extra-dark) !important;
}

.rogueAction {
  color: var(--rogue-extra-dark) !important;
}

.neutralAction {
  color: var(--neutral) !important;
}

.guardianActionButton {
  background-color: var(--guardian) !important;
  border: 0;
  border-radius: 2px;
  margin: 0 2px;
}

.seekerActionButton {
  background-color: var(--seeker) !important;
  border: 0;
  border-radius: 2px;
  margin: 0 2px;
}

.rogueActionButton {
  background-color: var(--rogue) !important;
  border: 0;
  border-radius: 2px;
  margin: 0 2px;
}

.mysticActionButton {
  background-color: var(--mystic) !important;
  border: 0;
  border-radius: 2px;
  margin: 0 2px;
}

.survivorActionButton {
  background-color: var(--survivor) !important;
  border: 0;
  border-radius: 2px;
  margin: 0 2px;
}

.neutralActionButton {
  background-color: var(--neutral) !important;
  border: 0;
  border-radius: 2px;
  margin: 0 2px;
}

.player-card {
  display: flex;
  flex-direction: column;
  width: calc(var(--card-width) * var(--card-sideways-aspect));
  @media (max-width: 800px) and (orientation: portrait) {
    width: 48%;
    display: flex;
    flex-direction: row;
    gap: 1px;
    :deep(.card) {
      height: calc(var(--card-width) * 3);
    }
  }
}

.portrait-container{
  position: relative;
  display: flex;
  justify-content: center;
  align-items: center;
  flex-direction: column;
  overflow: visible;
  :deep(span) {
    height: 0.87rem;
    overflow: visible;
    z-index: 10;
  }
  :deep(.action) {
    font-size: 0.35rem;
  }
}

.portrait {
  border-radius: 3px;
  width: calc(var(--card-width) * 0.6);
}

.supplies {
  & ul {
    display: flex;
    flex-direction: row;
    list-style: none;
  }
}

.stats {
  display: grid;
  grid-template-columns: 1fr 1fr 1fr 1fr;
  @media (max-width: 800px) and (orientation: portrait) {
    display: flex;
    flex-direction: column;
    width:6.8vw;
  }

}

.willpower {
  background-color: var(--guardian-dark);
  color: white;
  text-align: center;
  border-top-left-radius: 5px;
}

.intellect {
  background-color: var(--mystic-dark);
  color: white;
  text-align: center;
}

.combat {
  background-color: var(--survivor-dark);
  color: white;
  text-align: center;
}

.agility {
  background-color: var(--rogue-dark);
  color: white;
  text-align: center;
  border-top-right-radius: 5px;
}

.activeButton {
  border: 1px solid #FF00FF;
}

@keyframes become-ghost {
  100% {
    filter: drop-shadow(0px 0 20px #FF0099) invert(75%);
  }
}

@keyframes ghost {
  0% {
    filter: drop-shadow(0px 0 20px #FF0099) invert(75%);
  }

  50% {
    filter: drop-shadow(0px 0 10px #FF0099) invert(70%);
  }

  100% {
    filter: drop-shadow(0px 0 20px #FF0099) invert(75%);
  }
}

@keyframes rotate {
  100% {
    transform: rotate(-360deg);
  }
}

.ethereal {
  height: fit-content;
  will-change: filter;
  transition: filter .2s ease-out;
  animation-direction: forwards;
  animation: become-ghost 1s linear, ghost 3s linear 1s infinite;
}

.player-container{
  @media (max-width: 800px) and (orientation: portrait) {
    width: 100%;
  }
}

.player-area {
  display: flex;
}

.player-buttons {
  margin-left: 10px;
  display: flex;
  gap: 2px;
  flex-direction: column;

  @media (prefers-color-scheme: light) {
    color: #efefef;
  }
  @media (max-width: 800px) and (orientation: portrait) {
    margin-left: 0;
    flex-direction: row;
    gap: 8px;
    height: calc(var(--pool-token-width)*1.2);
    overflow: hidden;
    :deep(button) {
      width: calc(var(--pool-token-width) * 1.2);
      font-size:small;
    }
    :deep(img) {
      width: calc(var(--pool-token-width) * 1.2);
    }
  }
}

.skip-triggers-button {
  transition: all 0.2s ease-in;
  background-color: var(--select);
  color: white;
  border: 0;
  border-radius: 2px;

  &[disabled] {
    background-color: #999;
    color: #666;
  }

  &:not([disabled]):hover {
    background-color: var(--select-dark);
  }
}

.investigator-image {
  position: relative;
}

.card-row-cards {
  display: flex;
  flex-direction: row;
  gap: 5px;
  flex-wrap: wrap;
  padding: 10px;
}

.close {
  width: 100%;
  background: var(--button-2);
  display: inline;
  border: 0;
  color: white;
  padding: 0.5em;
  text-transform: uppercase;

  &:hover {
    background: var(--button-2-highlight);
  }
}

.sealed {
  position: absolute;
  width: calc(var(--card-width) / 2);
  left: 0;
  top: calc(var(--card-width) / 2);
}

.captured {
  rotate: 90deg;
}

button.active {
  background-color: var(--select-dark-20);
  border-color: var(--select-dark-20);
  border-radius: 2px;
  border-style: solid;
  color: white;
}
</style>
