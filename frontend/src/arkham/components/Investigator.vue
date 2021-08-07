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

      <button v-if="cardsUnderneath.length > 0" class="view-discard-button" @click="$emit('show-cards', cardsUnderneath)">{{cardsUnderneathLabel}}</button>
    </div>

    <div class="resources">
      <PoolItem
        type="resource"
        :amount="player.contents.resources"
        :class="{ 'resource--can-take': takeResourceAction !== -1 }"
        @choose="$emit('choose', takeResourceAction)"
      />
      <template v-if="debug">
        <button @click="debugChoose({tag: 'TakeResources', contents: [id, 1, false]})">+</button>
      </template>
      <PoolItem
        type="clue"
        :amount="player.contents.clues"
        :class="{ 'resource--can-spend': spendCluesAction !== -1 }"
        @choose="$emit('choose', spendCluesAction)"
      />
      <template v-if="debug">
        <button @click="debugChoose({tag: 'GainClues', contents: [id, 1]})">+</button>
      </template>
      <PoolItem
        type="health"
        :amount="player.contents.healthDamage"
        :class="{ 'health--can-interact': healthAction !== -1 }"
        @choose="$emit('choose', healthAction)"
      />
      <template v-if="debug">
        <button @click="debugChoose({tag: 'InvestigatorDirectDamage', contents: [id, {tag: 'TestSource', contents: []}, 1, 0]})">+</button>
        <button @click="debugChoose({tag: 'HealDamage', contents: [{tag: 'InvestigatorTarget', contents: id}, 1]})">-</button>
      </template>
      <PoolItem
        type="sanity"
        :amount="player.contents.sanityDamage"
        :class="{ 'sanity--can-interact': sanityAction !== -1 }"
        @choose="$emit('choose', sanityAction)"
      />
      <template v-if="debug">
        <button @click="debugChoose({tag: 'InvestigatorDirectDamage', contents: [id, {tag: 'TestSource', contents: []}, 0, 1]})">+</button>
        <button @click="debugChoose({tag: 'HealHorror', contents: [{tag: 'InvestigatorTarget', contents: id}, 1]})">-</button>
      </template>
      <span><i class="action" v-for="n in player.contents.remainingActions" :key="n"></i></span>
      <span v-if="player.contents.tomeActions && player.contents.tomeActions > 0">
        <i class="action tomeAction" v-for="n in player.contents.tomeActions" :key="n"></i>
      </span>
      <template v-if="debug">
        <button @click="debugChoose({tag: 'GainActions', contents: [id, {tag: 'TestSource', contents: []}, 1]})">+</button>
      </template>
      <button
        :disabled="endTurnAction === -1"
        @click="$emit('choose', endTurnAction)"
      >End turn</button>
    </div>
  </div>
</template>

<script lang="ts">
import { defineComponent, computed, inject } from 'vue'
import * as Arkham from '@/arkham/types/Investigator'
import { Game } from '@/arkham/types/Game'
import * as ArkhamGame from '@/arkham/types/Game'
import { Message, MessageType } from '@/arkham/types/Message'
import PoolItem from '@/arkham/components/PoolItem.vue'

export default defineComponent({
  components: { PoolItem },
  props: {
    game: { type: Object as () => Game, required: true },
    player: { type: Object as () => Arkham.Investigator, required: true },
    investigatorId: { type: String, required: true },
    portrait: { type: Boolean, default: false }
  },
  setup(props) {
    const choices = computed(() => ArkhamGame.choices(props.game, props.investigatorId))
    const id = computed(() => props.player.contents.id)
    const debug = inject('debug')
    const debugChoose = inject('debugChoose')

    const searchTopOfDeckAction = computed(() => {
      return choices
        .value
        .findIndex((c) => c.tag === MessageType.SEARCH_TOP_OF_DECK
          && c.contents[2].contents === id.value);
    })

    const runSkillTestAction = computed(() => {
      if (choices.value.filter((c) => c.tag === MessageType.BEGIN_SKILL_TEST
        && c.contents[0] === id.value).length === 1) {
        return choices
          .value
          .findIndex((c) => c.tag === MessageType.BEGIN_SKILL_TEST && c.contents[0] === id.value)
      }

      return -1
    })

    function canActivateAbility(c: Message): boolean {
      switch (c.tag) {
        case MessageType.ACTIVATE_ABILITY:
          return c.contents[1].source.contents === id.value;
        case MessageType.RUN:
          return c.contents.some((c1: Message) => canActivateAbility(c1));
        default:
          return false;
      }
    }
    const activateAbilityAction = computed(() => choices.value.findIndex(canActivateAbility))

    const enemyEngageInvestigatorAction = computed(() => {
      return choices
        .value
        .findIndex((c) => c.tag === MessageType.ENEMY_ENGAGE_INVESTIGATOR
          && c.contents[1] === id.value)
    })

    const takeDamageAction = computed(() => {
      const isRunDamage = choices.value.findIndex((c) => c.tag === MessageType.RUN
        && c.contents[0]
        && c.contents[0].tag === MessageType.INVESTIGATOR_ASSIGN_DAMAGE
        && c.contents[0].contents[0] === id.value);
      return isRunDamage
        || choices.value.findIndex((c) => c.tag === MessageType.INVESTIGATOR_DAMAGE);
    })

    const labelAction = computed(() => {
      return choices
        .value
        .findIndex((c) => c.tag === MessageType.TARGET_LABEL
          && c.contents[0].tag === "InvestigatorTarget" && c.contents[0].contents === id.value)
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

      if (activateAbilityAction.value !== -1) {
        return activateAbilityAction.value
      }

      return takeDamageAction.value
    })

    function canAdjustHealth(c: Message): boolean {
      switch (c.tag) {
        case MessageType.INVESTIGATOR_DAMAGE:
          return c.contents[0] === id.value && c.contents[2] > 0;
        case MessageType.HEAL_DAMAGE:
          return c.contents[0].contents === id.value;
        case MessageType.INVESTIGATOR_ASSIGN_DAMAGE:
          return c.contents[0] === id.value && c.contents[2] > 0;
        case MessageType.RUN:
          return c.contents.some((c1: Message) => canAdjustHealth(c1));
        default:
          return false;
      }
    }

    function canAdjustSanity(c: Message): boolean {
      switch (c.tag) {
        case MessageType.INVESTIGATOR_DAMAGE:
          return c.contents[0] === id.value && c.contents[3] > 0;
        case MessageType.INVESTIGATOR_ASSIGN_DAMAGE:
          return c.contents[0] === id.value && c.contents[3] > 0;
        case MessageType.HEAL_HORROR:
          return c.contents[0].contents === id.value;
        case MessageType.RUN:
          return c.contents.some((c1: Message) => canAdjustSanity(c1));
        default:
          return false;
      }
    }

    const healthAction = computed(() => choices.value.findIndex(canAdjustHealth))
    const sanityAction = computed(() => choices.value.findIndex(canAdjustSanity))

    const takeResourceAction = computed(() => {
      return choices
        .value
        .findIndex((c) => c.tag === MessageType.TAKE_RESOURCES && c.contents[0] === id.value);
    })

    const spendCluesAction = computed(() => {
      return choices
        .value
        .findIndex((c) => (c.tag === MessageType.INVESTIGATOR_SPEND_CLUES || c.tag === MessageType.INVESTIGATOR_PLACE_CLUES_ON_LOCATION)
          && c.contents[0] === id.value);
    })

    const endTurnAction = computed(() => {
      return choices
        .value
        .findIndex((c) => c.tag === MessageType.END_TURN && c.contents === id.value);
    })

    const baseUrl = process.env.NODE_ENV == 'production' ? "https://assets.arkhamhorror.app" : '';
    const image = computed(() => {
      return `${baseUrl}/img/arkham/cards/${id.value.replace('c', '')}.jpg`;
    })

    const portraitImage = computed(() => `${baseUrl}/img/arkham/portraits/${id.value.replace('c', '')}.jpg`)


    const cardsUnderneath = computed(() => props.player.contents.cardsUnderneath)
    const cardsUnderneathLabel = computed(() => `Underneath (${cardsUnderneath.value.length})`)

    return {
      id,
      portraitImage,
      cardsUnderneath,
      cardsUnderneathLabel,
      debug,
      debugChoose,
      image,
      endTurnAction,
      spendCluesAction,
      takeResourceAction,
      healthAction,
      sanityAction,
      investigatorAction
    }
  }
})
</script>

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

.tomeAction {
  color: $seeker;
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
