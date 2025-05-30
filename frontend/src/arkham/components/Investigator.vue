<script lang="ts" setup>
import { useSettings } from '@/stores/settings';
import { storeToRefs } from 'pinia';
import { onMounted, computed, ref, watch } from 'vue'
import Draggable from '@/components/Draggable.vue';
import CardView from '@/arkham/components/Card.vue';
import { useDebug } from '@/arkham/debug'
import { PaperClipIcon } from '@heroicons/vue/20/solid'
import type { Game } from '@/arkham/types/Game'
import { imgsrc } from '@/arkham/helpers'
import { TokenType } from '@/arkham/types/Token'
import * as ArkhamGame from '@/arkham/types/Game';
import * as Arkham from '@/arkham/types/Investigator'
import type { AbilityLabel, AbilityMessage, Message } from '@/arkham/types/Message'
import { MessageType } from '@/arkham/types/Message'
import type { Modifier } from '@/arkham/types/Modifier'
import Token from '@/arkham/components/Token.vue';
import PoolItem from '@/arkham/components/PoolItem.vue'
import Key from '@/arkham/components/Key.vue';
import Seal from '@/arkham/components/Seal.vue';
import AbilityButton from '@/arkham/components/AbilityButton.vue'
import { useMenu } from '@/composeable/menu';
import { useI18n } from 'vue-i18n';
import useEmitter from '@/composeable/useEmitter'
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

function canAdjustHealth(c: Message): boolean {
  if (c.tag === MessageType.COMPONENT_LABEL && c.component.tag === "InvestigatorComponent" && c.component.tokenType === "DamageToken") {
    return c.component.investigatorId === id.value
  }
  return false
}

function canAdjustSanity(c: Message): boolean {
  if (c.tag === MessageType.COMPONENT_LABEL && c.component.tag === "InvestigatorComponent" && c.component.tokenType === "HorrorToken") {
    return c.component.investigatorId === id.value
  }
  return false
}

const healthAction = computed(() => props.choices.findIndex(canAdjustHealth))
const sanityAction = computed(() => props.choices.findIndex(canAdjustSanity))

const takeResourceAction = computed(() => {
  return props.choices
    .findIndex((c) => {
      if (c.tag === MessageType.COMPONENT_LABEL && c.component.tag === "InvestigatorComponent" && c.component.tokenType === "ResourceToken") {
        return c.component.investigatorId === id.value
      }
      return false
    });
})

const spendCluesAction = computed(() => {
  return props.choices
    .findIndex((c) => {
      if (c.tag === MessageType.COMPONENT_LABEL && c.component.tag === "InvestigatorComponent"  && c.component.tokenType === "ClueToken") {
        return c.component.investigatorId === id.value
      }
      return false
    });
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

const keys = computed(() => props.investigator.keys)
const seals = computed(() => props.investigator.seals)

const willpower = computed(() => calculateSkill(props.investigator.willpower, "SkillWillpower", modifiers.value ?? []))
const intellect = computed(() => calculateSkill(props.investigator.intellect, "SkillIntellect", modifiers.value ?? []))
const combat = computed(() => calculateSkill(props.investigator.combat, "SkillCombat", modifiers.value ?? []))
const agility = computed(() => calculateSkill(props.investigator.agility, "SkillAgility", modifiers.value ?? []))


const doom = computed(() => props.investigator.tokens[TokenType.Doom])
const clues = computed(() => props.investigator.tokens[TokenType.Clue] || 0)
const resources = computed(() => props.investigator.tokens[TokenType.Resource] || 0)
const horror = computed(() => (props.investigator.tokens[TokenType.Horror] || 0) + props.investigator.assignedSanityDamage)
const damage = computed(() => (props.investigator.tokens[TokenType.Damage] || 0) + props.investigator.assignedHealthDamage)
const alarmLevel = computed(() => props.investigator.tokens[TokenType.AlarmLevel] || 0)
const leylines = computed(() => props.investigator.tokens[TokenType.Leyline] || 0)

const dragging = ref(false)
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
  <div v-if="portrait">
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
  <div v-else>
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

      <div class="player-buttons">
        <span><i class="action" v-for="n in investigator.remainingActions" :key="n"></i></span>
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
        >{{ $t('investigator.endTurn') }}</button>

        <button
          v-if="devoured && devoured.length > 0"
          @click="showDevoured"
        >{{ $t('investigator.devouredCards', {count: devoured.length}) }}</button>

        <button
          :disabled="skipTriggersAction == -1"
          @click="$emit('choose', skipTriggersAction)"
          class="skip-triggers-button"
        >{{ $t('investigator.skipTriggers') }}</button>

        <button v-if="cardsUnderneath.length > 0" class="view-discard-button" @click="showCardsUnderneath">{{cardsUnderneathLabel}}</button>
      </div>
    </div>

    <div class="resources">
      <div class="keys" v-if="keys.length > 0">
        <Key v-for="key in keys" :key="key" :name="key" />
      </div>
      <div class="seals" v-if="seals.length > 0">
        <Seal v-for="seal in seals" :key="seal.sealKind" :seal="seal" />
      </div>
      <template v-if="debug.active">
        <button
          @click.exact="debug.send(game.id, {tag: 'LoseResources', contents: [id, {tag: 'GameSource'}, 1]})"
          @click.shift="debug.send(game.id, {tag: 'LoseAllResources', contents: [id, {tag: 'GameSource'}]})"
        >-</button>
      </template>
      <PoolItem
        type="resource"
        :amount="resources"
        :class="{ 'resource--can-take': takeResourceAction !== -1 }"
        @choose="$emit('choose', takeResourceAction)"
      />
      <template v-if="debug.active">
        <button
          class="plus-button"
          @click.exact="debug.send(game.id, {tag: 'TakeResources', contents: [id, 1, {tag: 'GameSource' }, false]})"
          @click.shift="debug.send(game.id, {tag: 'TakeResources', contents: [id, 5, {tag: 'GameSource' }, false]})"
        >+</button>
      </template>
      <template v-if="debug.active">
        <button
          @click.exact="debug.send(game.id, {tag: 'GainClues', contents: [id, {tag: 'GameSource' }, -1]})"
          @click.shift="debug.send(game.id, {tag: 'InvestigatorDiscardAllClues', contents: [{tag: 'GameSource' }, id]})"
        >-</button>
      </template>
      <PoolItem
        type="clue"
        :amount="clues"
        :class="{ 'resource--can-spend': spendCluesAction !== -1 }"
        @choose="$emit('choose', spendCluesAction)"
      />
      <template v-if="debug.active">
        <button
          class="plus-button"
          @click.exact="debug.send(game.id, {tag: 'GainClues', contents: [id, {tag: 'GameSource' }, 1]})"
          @click.shift="debug.send(game.id, {tag: 'GainClues', contents: [id, {tag: 'GameSource' }, 5]})"
        >+</button>
      </template>
      <template v-if="debug.active">
        <button
          @click.exact="debug.send(game.id, {tag: 'HealDamage', contents: [{tag: 'InvestigatorTarget', contents: id}, {tag: 'TestSource', contents: []}, 1]})"
          @click.shift="debug.send(game.id, {tag: 'HealDamage', contents: [{tag: 'InvestigatorTarget', contents: id}, {tag: 'TestSource', contents: []}, investigator.tokens['Damage'] ?? 0]})"
        >-</button>
      </template>
      <PoolItem
        type="health"
        :amount="damage"
        :class="{ 'health--can-interact': healthAction !== -1 }"
        @choose="$emit('choose', healthAction)"
      />
      <template v-if="debug.active">
        <button
          class="plus-button"
          @click.exact="debug.send(game.id, {tag: 'InvestigatorDirectDamage', contents: [id, {tag: 'TestSource', contents: []}, 1, 0]})"
          @click.shift="debug.send(game.id, {tag: 'InvestigatorDirectDamage', contents: [id, {tag: 'TestSource', contents: []}, 5, 0]})"
        >+</button>
      </template>
      <template v-if="debug.active">
        <button
          @click.exact="debug.send(game.id, {tag: 'HealHorror', contents: [{tag: 'InvestigatorTarget', contents: id}, {tag: 'TestSource', contents: []}, 1]})"
          @click.shift="debug.send(game.id, {tag: 'HealHorror', contents: [{tag: 'InvestigatorTarget', contents: id}, {tag: 'TestSource', contents: []}, investigator.tokens['Horror'] ?? 0]})"
        >-</button>
      </template>
      <PoolItem
        type="sanity"
        :amount="horror"
        :class="{ 'sanity--can-interact': sanityAction !== -1 }"
        @choose="$emit('choose', sanityAction)"
      />
      <template v-if="debug.active">
        <button
          class="plus-button"
          @click.exact="debug.send(game.id, {tag: 'InvestigatorDirectDamage', contents: [id, {tag: 'TestSource', contents: []}, 0, 1]})"
          @click.shift="debug.send(game.id, {tag: 'InvestigatorDirectDamage', contents: [id, {tag: 'TestSource', contents: []}, 0, 5]})"
        >+</button>
      </template>

      <PoolItem v-if="doom > 0" type="doom" :amount="doom" />

      <PoolItem
        v-if="alarmLevel > 0"
        type="doom"
        :amount="alarmLevel"
        tooltip="Alarm Level"
      />

      <PoolItem
        v-if="leylines > 0"
        type="resource"
        :amount="leylines"
        tooltip="Leyline"
      />
    </div>

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

  @media (prefers-color-scheme: dark) {
    color: #EEE;
  }

  &:before {
    font-family: "Arkham";
    content: "\0049";
  }
}

.resources {
  display: flex;
  justify-content: space-between;
  button {
    height: min-content;
    align-self: center;
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
}

.plus-button {
  margin-right: 10px;
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
