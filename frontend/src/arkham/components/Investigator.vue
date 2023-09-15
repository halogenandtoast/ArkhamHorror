<script lang="ts" setup>
import { computed } from 'vue'
import { useDebug } from '@/arkham/debug'
import type { Game } from '@/arkham/types/Game'
import { imgsrc } from '@/arkham/helpers'
import { TokenType } from '@/arkham/types/Token'
import * as ArkhamGame from '@/arkham/types/Game';
import * as Arkham from '@/arkham/types/Investigator'
import type { AbilityLabel, AbilityMessage, Message } from '@/arkham/types/Message'
import { MessageType } from '@/arkham/types/Message'
import type { Modifier } from '@/arkham/types/Modifier'
import PoolItem from '@/arkham/components/PoolItem.vue'
import Key from '@/arkham/components/Key.vue';
import AbilityButton from '@/arkham/components/AbilityButton.vue'

export interface Props {
  choices: Message[]
  player: Arkham.Investigator
  investigatorId: string
  game: Game
  portrait?: boolean
}

const props = withDefaults(defineProps<Props>(), { portrait: false })
const emit = defineEmits(['showCards', 'choose'])

const id = computed(() => props.player.id)
const debug = useDebug()

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

const choices = computed(() => ArkhamGame.choices(props.game, props.investigatorId))

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
        return [...acc, { contents: v, index: i }];
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

const image = computed(() => {
  if (props.player.isYithian) {
    return imgsrc("cards/04244.jpg");
  }

  return imgsrc(`cards/${props.player.cardCode.replace('c', '')}.jpg`);
})

const portraitImage = computed(() => {
  if (props.player.isYithian) {
    return imgsrc(`portraits/${id.value.replace('c', '')}.jpg`)
  }

  return imgsrc(`portraits/${props.player.cardCode.replace('c', '')}.jpg`)
})


const cardsUnderneath = computed(() => props.player.cardsUnderneath)
const cardsUnderneathLabel = computed(() => `Underneath (${cardsUnderneath.value.length})`)

const showCardsUnderneath = (e: Event) => emit('showCards', e, cardsUnderneath, "Cards Underneath", false)

const modifiers = computed(() => props.player.modifiers)

function calculateSkill(base: number, skillType: string, modifiers: Modifier[]) {
  let modified = base

  modifiers.forEach((modifier) => {
    if (modifier.type.tag === "BaseSkillOf" && modifier.type.skillType === skillType) {
      modified = modifier.type.value
    }
  })

  modifiers.forEach((modifier) => {
    if (modifier.type.tag === "SkillModifier" && modifier.type.skillType === skillType) {
      modified = modified + modifier.type.value
    }

    if (modifier.type.tag === "ActionSkillModifier" && modifier.type.skillType === skillType && props.game.skillTest && props.game.skillTest.action === modifier.type.action) {
      modified = modified + modifier.type.value
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

const keys = computed(() => props.player.keys)

const willpower = computed(() => calculateSkill(props.player.willpower, "SkillWillpower", modifiers.value ?? []))
const intellect = computed(() => calculateSkill(props.player.intellect, "SkillIntellect", modifiers.value ?? []))
const combat = computed(() => calculateSkill(props.player.combat, "SkillCombat", modifiers.value ?? []))
const agility = computed(() => calculateSkill(props.player.agility, "SkillAgility", modifiers.value ?? []))


// const doom = computed(() => props.player.tokens[TokenType.Doom])
const clues = computed(() => props.player.tokens[TokenType.Clue] || 0)
const resources = computed(() => props.player.tokens[TokenType.Resource] || 0)
const horror = computed(() => (props.player.tokens[TokenType.Horror] || 0) + props.player.assignedSanityDamage)
const damage = computed(() => (props.player.tokens[TokenType.Damage] || 0) + props.player.assignedHealthDamage)
</script>

<template>
  <img v-if="portrait"
    :src="portraitImage"
    class="portrait"
    :class="{ 'investigator--can-interact--portrait': investigatorAction !== -1 }"
    @click="$emit('choose', investigatorAction)"
  />
  <div v-else>
    <div class="player-card">
      <div class="stats">
        <div class="willpower willpower-icon">{{willpower}}</div>
        <div class="intellect intellect-icon">{{intellect}}</div>
        <div class="combat combat-icon">{{combat}}</div>
        <div class="agility agility-icon">{{agility}}</div>
      </div>
      <img
        :class="{ 'investigator--can-interact': investigatorAction !== -1 }"
        class="card"
        :src="image"
        @click="$emit('choose', investigatorAction)"
      />

      <button v-if="cardsUnderneath.length > 0" class="view-discard-button" @click="showCardsUnderneath">{{cardsUnderneathLabel}}</button>
    </div>

    <div class="resources">
      <div class="keys" v-if="keys.length > 0">
        <Key v-for="key in keys" :key="key" :name="key" />
      </div>
      <PoolItem
        type="resource"
        :amount="resources"
        :class="{ 'resource--can-take': takeResourceAction !== -1 }"
        @choose="$emit('choose', takeResourceAction)"
      />
      <template v-if="debug.active">
        <button @click="debug.send(game.id, {tag: 'TakeResources', contents: [id, 1, {tag: 'GameSource' }, false]})">+</button>
        <button @click="debug.send(game.id, {tag: 'SpendResources', contents: [id, 1]})">-</button>
      </template>
      <PoolItem
        type="clue"
        :amount="clues"
        :class="{ 'resource--can-spend': spendCluesAction !== -1 }"
        @choose="$emit('choose', spendCluesAction)"
      />
      <template v-if="debug.active">
        <button @click="debug.send(game.id, {tag: 'GainClues', contents: [id, {tag: 'GameSource' }, 1]})">+</button>
      </template>
      <PoolItem
        type="health"
        :amount="damage"
        :class="{ 'health--can-interact': healthAction !== -1 }"
        @choose="$emit('choose', healthAction)"
      />
      <template v-if="debug.active">
        <button @click="debug.send(game.id, {tag: 'InvestigatorDirectDamage', contents: [id, {tag: 'TestSource', contents: []}, 1, 0]})">+</button>
        <button @click="debug.send(game.id, {tag: 'HealDamage', contents: [{tag: 'InvestigatorTarget', contents: id}, {tag: 'TestSource', contents: []}, 1]})">-</button>
      </template>
      <PoolItem
        type="sanity"
        :amount="horror"
        :class="{ 'sanity--can-interact': sanityAction !== -1 }"
        @choose="$emit('choose', sanityAction)"
      />
      <template v-if="debug.active">
        <button @click="debug.send(game.id, {tag: 'InvestigatorDirectDamage', contents: [id, {tag: 'TestSource', contents: []}, 0, 1]})">+</button>
        <button @click="debug.send(game.id, {tag: 'HealHorror', contents: [{tag: 'InvestigatorTarget', contents: id}, {tag: 'TestSource', contents: []}, 1]})">-</button>
      </template>
      <span><i class="action" v-for="n in player.remainingActions" :key="n"></i></span>
      <span v-if="player.additionalActions.length > 0">
        <template v-for="action in player.additionalActions" :key="action">
          <button @click="useEffectAction(action)" v-if="action.tag === 'EffectAction'" v-tooltip="action.contents[0]" :class="[{ activeButton: isActiveEffectAction(action)}, `${player.class.toLowerCase()}ActionButton`]">
            <i class="action"></i>
          </button>
          <i v-else class="action" :class="`${player.class.toLowerCase()}Action`"></i>
        </template>
      </span>
      <template v-if="debug.active">
        <button @click="debug.send(game.id, {tag: 'GainActions', contents: [id, {tag: 'TestSource', contents: []}, 1]})">+</button>
      </template>
      <AbilityButton
        v-for="ability in abilities"
        :key="ability.index"
        :ability="ability.contents"
        @click="$emit('choose', ability.index)"
        />
      <button
        :disabled="endTurnAction == -1"
        @click="$emit('choose', endTurnAction)"
      >End turn</button>
    </div>
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
  align-self: center;
  align-items: center;
}

.turn-info {
  display: flex;
  align-self: center;
  align-items: center;
}

.investigator--can-interact {
  border: 2px solid $select;
  cursor: pointer;
  &--portrait {
    border: 3px solid $select;
  }
}

.card {
  width: auto;
  height: $card-width;
}

.guardianAction {
  color: $guardian !important;
}

.survivorAction {
  color: $survivor !important;
}

.mysticAction {
  color: $mystic !important;
}

.seekerAction {
  color: $seeker !important;
}

.rogueAction {
  color: $rogue !important;
}

.neutralAction {
  color: $neutral !important;
}

.guardianActionButton {
  background-color: $guardian !important;
  border: 0;
  border-radius: 2px;
  margin: 0 2px;
}

.seekerActionButton {
  background-color: $seeker !important;
  border: 0;
  border-radius: 2px;
  margin: 0 2px;
}

.rogueActionButton {
  background-color: $rogue !important;
  border: 0;
  border-radius: 2px;
  margin: 0 2px;
}

.mysticActionButton {
  background-color: $mystic !important;
  border: 0;
  border-radius: 2px;
  margin: 0 2px;
}

.survivorActionButton {
  background-color: $survivor !important;
  border: 0;
  border-radius: 2px;
  margin: 0 2px;
}

.neutralActionButton {
  background-color: $neutral !important;
  border: 0;
  border-radius: 2px;
  margin: 0 2px;
}

.player-card {
  display: flex;
  flex-direction: column;
  width: $card-width * 1.4;
}

.portrait {
  border-radius: 3px;
  width: $card-width * 0.6;
  margin-right: 2px;
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
  background-color: $guardian;
  color: white;
  text-align: center;
  border-top-left-radius: 5px;
}

.intellect {
  background-color: $mystic;
  color: white;
  text-align: center;
}

.combat {
  background-color: $survivor;
  color: white;
  text-align: center;
}

.agility {
  background-color: $rogue;
  color: white;
  text-align: center;
  border-top-right-radius: 5px;
}

.activeButton {
  border: 1px solid #FF00FF;
}
</style>
