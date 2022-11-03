<script lang="ts" setup>
import { computed, inject } from 'vue'
import type { Game } from '@/arkham/types/Game'
import * as Arkham from '@/arkham/types/Investigator'
import type { Message } from '@/arkham/types/Message'
import { MessageType } from '@/arkham/types/Message'
import type { Modifier } from '@/arkham/types/Modifier'
import PoolItem from '@/arkham/components/PoolItem.vue'

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
const debug = inject('debug')
const debugChoose = inject('debugChoose')

function canActivateAbility(c: Message): boolean {
  if (c.tag  === MessageType.ABILITY_LABEL) {
    return c.ability.source.contents === id.value
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

function canAdjustHealth(c: Message): boolean {
  if (c.tag === MessageType.COMPONENT_LABEL && c.component.tokenType === "DamageToken") {
    return c.component.investigatorId === id.value
  }
  return false
}

function canAdjustSanity(c: Message): boolean {
  if (c.tag === MessageType.COMPONENT_LABEL && c.component.tokenType === "HorrorToken") {
    return c.component.investigatorId === id.value
  }
  return false
}

const healthAction = computed(() => props.choices.findIndex(canAdjustHealth))
const sanityAction = computed(() => props.choices.findIndex(canAdjustSanity))

const takeResourceAction = computed(() => {
  return props.choices
    .findIndex((c) => {
      if (c.tag === MessageType.COMPONENT_LABEL && c.component.tokenType === "ResourceToken") {
        return c.component.investigatorId === id.value
      }
      return false
    });
})

const spendCluesAction = computed(() => {
  return props.choices
    .findIndex((c) => {
      if (c.tag === MessageType.COMPONENT_LABEL && c.component.tokenType === "ClueToken") {
        return c.component.investigatorId === id.value
      }
      return false
    });
})

const endTurnAction = computed(() => {
  return props.choices
    .findIndex((c) => c.tag === MessageType.END_TURN_BUTTON && c.investigatorId === id.value);
})

const baseUrl = inject('baseUrl')
const image = computed(() => {
  if (props.player.isYithian) {
    return `${baseUrl}/img/arkham/cards/04244.jpg`;
  }

  return `${baseUrl}/img/arkham/cards/${id.value.replace('c', '')}.jpg`;
})

const portraitImage = computed(() => `${baseUrl}/img/arkham/portraits/${id.value.replace('c', '')}.jpg`)


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

const willpower = computed(() => calculateSkill(props.player.willpower, "SkillWillpower", modifiers.value))
const intellect = computed(() => calculateSkill(props.player.intellect, "SkillIntellect", modifiers.value))
const combat = computed(() => calculateSkill(props.player.combat, "SkillCombat", modifiers.value))
const agility = computed(() => calculateSkill(props.player.agility, "SkillAgility", modifiers.value))
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
      <PoolItem
        type="resource"
        :amount="player.resources"
        :class="{ 'resource--can-take': takeResourceAction !== -1 }"
        @choose="$emit('choose', takeResourceAction)"
      />
      <template v-if="debug">
        <button @click="debugChoose({tag: 'TakeResources', contents: [id, 1, false]})">+</button>
        <button @click="debugChoose({tag: 'SpendResources', contents: [id, 1]})">-</button>
      </template>
      <PoolItem
        type="clue"
        :amount="player.clues"
        :class="{ 'resource--can-spend': spendCluesAction !== -1 }"
        @choose="$emit('choose', spendCluesAction)"
      />
      <template v-if="debug">
        <button @click="debugChoose({tag: 'GainClues', contents: [id, 1]})">+</button>
      </template>
      <PoolItem
        type="health"
        :amount="player.healthDamage + player.assignedHealthDamage"
        :class="{ 'health--can-interact': healthAction !== -1 }"
        @choose="$emit('choose', healthAction)"
      />
      <template v-if="debug">
        <button @click="debugChoose({tag: 'InvestigatorDirectDamage', contents: [id, {tag: 'TestSource', contents: []}, 1, 0]})">+</button>
        <button @click="debugChoose({tag: 'HealDamage', contents: [{tag: 'InvestigatorTarget', contents: id}, 1]})">-</button>
      </template>
      <PoolItem
        type="sanity"
        :amount="player.sanityDamage + player.assignedSanityDamage"
        :class="{ 'sanity--can-interact': sanityAction !== -1 }"
        @choose="$emit('choose', sanityAction)"
      />
      <template v-if="debug">
        <button @click="debugChoose({tag: 'InvestigatorDirectDamage', contents: [id, {tag: 'TestSource', contents: []}, 0, 1]})">+</button>
        <button @click="debugChoose({tag: 'HealHorror', contents: [{tag: 'InvestigatorTarget', contents: id}, 1]})">-</button>
      </template>
      <span><i class="action" v-for="n in player.remainingActions" :key="n"></i></span>
      <span v-if="player.additionalActions.length > 0">
        <i class="action" :class="`${player.class.toLowerCase()}Action`" v-for="n in player.additionalActions" :key="n"></i>
      </span>
      <template v-if="debug">
        <button @click="debugChoose({tag: 'GainActions', contents: [id, {tag: 'TestSource', contents: []}, 1]})">+</button>
      </template>
      <button
        :disabled="endTurnAction === -1"
        @click="$emit('choose', endTurnAction)"
      >End turn</button>
    </div>

    <div v-if="player.supplies.length > 0" class="supplies">
      <ul>
        <li v-for="(supply, index) in player.supplies" :key="index">{{supply}}</li>
      </ul>
    </div>
  </div>
</template>

<style scoped lang="scss">
i.action {
  font-family: 'Arkham';
  speak: none;
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

.seekerAction {
  color: $seeker !important;
}

.rogueAction {
  color: $rogue !important;
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
</style>
