<script lang="ts" setup>
import { computed, inject } from 'vue'
import * as Arkham from '@/arkham/types/Investigator'
import type { Message } from '@/arkham/types/Message'
import { MessageType } from '@/arkham/types/Message'
import { QuestionType } from '@/arkham/types/Question'
import PoolItem from '@/arkham/components/PoolItem.vue'

export interface Props {
  choices: Message[]
  player: Arkham.Investigator
  investigatorId: string
  portrait?: boolean
}

const props = withDefaults(defineProps<Props>(), { portrait: false })
const emit = defineEmits(['showCards', 'choose'])

const id = computed(() => props.player.id)
const debug = inject('debug')
const debugChoose = inject('debugChoose')

const searchTopOfDeckAction = computed(() => {
  return -1
  // return props.choices
  //   .findIndex((c) => c.tag === MessageType.SEARCH
  //     && c.contents[2].contents === id.value);
})

const runSkillTestAction = computed(() => {
  // if (props.choices.filter((c) => c.tag === MessageType.BEGIN_SKILL_TEST
  //   && c.contents[0] === id.value).length === 1) {
  //   return props.choices
  //     .findIndex((c) => c.tag === MessageType.BEGIN_SKILL_TEST && c.contents[0] === id.value)
  // }

  return -1
})

function canActivateAbility(c: Message): boolean {
  return false
  // switch (c.tag) {
  //   case MessageType.ACTIVATE_ABILITY:
  //     return c.contents[1].source.contents === id.value;
  //   case MessageType.RUN:
  //     return c.contents.some((c1: Message) => canActivateAbility(c1));
  //   default:
  //     return false;
  // }
}
const activateAbilityAction = computed(() => props.choices.findIndex(canActivateAbility))

const enemyEngageInvestigatorAction = computed(() => {
  return -1
  // return props.choices
  //   .findIndex((c) => c.tag === MessageType.ENEMY_ENGAGE_INVESTIGATOR
  //     && c.contents[1] === id.value)
})

const labelAction = computed(() => {
  return props.choices
    .findIndex((c) => c.tag === "TargetLabel"
      && c.target.tag === "InvestigatorTarget" && c.target.contents === id.value)
})

const investigatorAction = computed(() => {
  if (labelAction.value !== -1) {
    return labelAction.value
  }

  if (searchTopOfDeckAction.value !== -1) {
    return searchTopOfDeckAction.value
  }

  if (runSkillTestAction.value !== -1) {
    return runSkillTestAction.value
  }

  if (enemyEngageInvestigatorAction.value !== -1) {
    return enemyEngageInvestigatorAction.value
  }

  return activateAbilityAction.value
})

function canAdjustHealth(c: Message): boolean {
  if (c.tag === "ComponentLabel" && c.component.tokenType === "DamageToken") {
    return c.component.investigatorId === id.value
  }
  return false
}

function canAdjustSanity(c: Message): boolean {
  if (c.tag === "ComponentLabel" && c.component.tokenType === "HorrorToken") {
    return c.component.investigatorId === id.value
  }
  return false
}

const healthAction = computed(() => props.choices.findIndex(canAdjustHealth))
const sanityAction = computed(() => props.choices.findIndex(canAdjustSanity))

const takeResourceAction = computed(() => {
  return props.choices
    .findIndex((c) => {
      if (c.tag === "ComponentLabel" && c.component.tokenType === "ResourceToken") {
        return c.component.investigatorId === id.value
      }
      return false
    });
})

const spendCluesAction = computed(() => {
  return -1
  // return props.choices
  //   .findIndex((c) => (c.tag === MessageType.INVESTIGATOR_SPEND_CLUES || c.tag === MessageType.INVESTIGATOR_PLACE_CLUES_ON_LOCATION)
  //     && c.contents[0] === id.value);
})

const endTurnAction = computed(() => {
  return props.choices
    .findIndex((c) => c.tag === 'EndTurnButton' && c.investigatorId === id.value);
})

const baseUrl = inject('baseUrl')
const image = computed(() => {
  return `${baseUrl}/img/arkham/cards/${id.value.replace('c', '')}.jpg`;
})

const portraitImage = computed(() => `${baseUrl}/img/arkham/portraits/${id.value.replace('c', '')}.jpg`)


const cardsUnderneath = computed(() => props.player.cardsUnderneath)
const cardsUnderneathLabel = computed(() => `Underneath (${cardsUnderneath.value.length})`)

const showCardsUnderneath = (e: Event) => emit('showCards', e, cardsUnderneath, "Cards Underneath", false)
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
        :amount="player.healthDamage"
        :class="{ 'health--can-interact': healthAction !== -1 }"
        @choose="$emit('choose', healthAction)"
      />
      <template v-if="debug">
        <button @click="debugChoose({tag: 'InvestigatorDirectDamage', contents: [id, {tag: 'TestSource', contents: []}, 1, 0]})">+</button>
        <button @click="debugChoose({tag: 'HealDamage', contents: [{tag: 'InvestigatorTarget', contents: id}, 1]})">-</button>
      </template>
      <PoolItem
        type="sanity"
        :amount="player.sanityDamage"
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

    <div v-if="player.supplies.length > 0">
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
</style>
