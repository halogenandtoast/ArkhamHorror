<script lang="ts" setup>
import { withDefaults, computed, inject } from 'vue';
import type { Card } from '@/arkham/types/Card';
import type { Game } from '@/arkham/types/Game';
import * as ArkhamGame from '@/arkham/types/Game';
import type { Message } from '@/arkham/types/Message';
import { MessageType } from '@/arkham/types/Message';

export interface Props {
  game: Game
  card: Card
  revealed?: boolean
  investigatorId: string
}

const props = withDefaults(defineProps<Props>(), { revealed: false })
const emit = defineEmits(['choose'])
const baseUrl = inject('baseUrl')

const image = computed(() => {
  const { cardCode } = props.card.contents
  const suffix = !props.revealed && props.card.contents.isFlipped ? 'b' : ''
  return `${baseUrl}/img/arkham/cards/${cardCode.replace(/^c/, '')}${suffix}.jpg`
})

const id = computed(() => props.card.contents.id)
const choices = computed(() => ArkhamGame.choices(props.game, props.investigatorId))

function canInteract(c: Message): boolean {
  return false
  // switch (c.tag) {
  //   case MessageType.DISCARD:
  //     // TODO: Check the contents tag
  //     return c.contents.contents[1] === id.value;
  //   case MessageType.RETURN_TO_HAND:
  //     return c.contents[1].contents === id.value;
  //   case MessageType.REMOVE_FROM_GAME:
  //     return c.contents.contents === id.value;
  //   case MessageType.ADD_FOCUSED_TO_HAND:
  //     return c.contents[3] === id.value;
  //   case MessageType.SEARCH_FOUND:
  //     return c.contents[2].contents.id === id.value;
  //   case MessageType.PLAY_CARD:
  //     return c.contents[1] === id.value;
  //   case MessageType.PLAY_CARD_AS:
  //     return c.contents[1] === id.value;
  //   case MessageType.PLAY_FAST_EVENT:
  //     return c.contents[1] === id.value
  //   case MessageType.ADD_FOCUSED_TO_TOP_OF_DECK:
  //     return c.contents[2] === id.value;
  //   case MessageType.FOUND_AND_DREW_ENCOUNTER_CARD:
  //     return c.contents[2].id === id.value;
  //   case MessageType.FOUND_ENCOUNTER_CARD_FROM:
  //     return c.contents[3].id === id.value;
  //   case MessageType.FOUND_ENEMY_IN_VOID:
  //     return c.contents[2] === id.value;
  //   case MessageType.TARGET_LABEL:
  //     return (c.contents[0].tag === "EncounterCardTarget" && c.contents[0].contents.id === id.value) || (c.contents[0].tag === "CardIdTarget" && c.contents[0].contents === id.value) || (c.contents[0].tag === "SkillTarget" && c.contents[0].contents === id.value)
  //
  //   case MessageType.RUN:
  //     return c.contents.some((c1: Message) => canInteract(c1));
  //   default:
  //     return false;
  // }
}

function canPlay(c: Message): boolean {
  return false
  // switch (c.tag) {
  //   case MessageType.PLAY_CARD:
  //     return c.contents[1] === id.value
  //   case MessageType.PLAY_CARD_AS:
  //     return c.contents[1] === id.value
  //   case MessageType.PLAY_DYNAMIC_CARD:
  //     return c.contents[1] === id.value
  //   case MessageType.PLAY_FAST_EVENT:
  //     return c.contents[1] === id.value
  //   case MessageType.LEGACY_PLAY_CARD:
  //     return c.contents[1] === id.value
  //   case MessageType.RUN:
  //     return c.contents.some((c1: Message) => canPlay(c1))
  //   default:
  //     return false
  // }
}

const playCardAction = computed(() => choices.value.findIndex(canPlay))
const cardAction = computed(() => {
  if (playCardAction.value !== -1) {
    return playCardAction.value
  }
  return choices.value.findIndex(canInteract)
})

function abilityLabel(idx: number) {
  // const label = choices.value[idx].tag === 'Run'
  //   ? ( choices.value[idx].contents[0].contents[1].type.contents[0].tag === 'AmongSearchedCards'
  //       ? null
  //       : choices.value[idx].contents[0].contents[1].type.contents[0]
  //     )
  //   : choices.value[idx].contents[1].type.contents[0]
  // if (label) {
  //   return typeof label === "string" ? label : label.contents
  // }

  return ""
}

function isReactionAbility(idx: number) {
  return false
  // if (choices.value[idx].tag == 'Run') {
  //   return choices.value[idx].contents[0].contents[1].type.tag === "ReactionAbility";
  // } else {
  //   return choices.value[idx].contents[1].type.tag === "ReactionAbility";
  // }
}

function isActivate(v: Message) {
  return false
  // if (v.tag !== 'AbilityLabel') {
  //   return false
  // }

  // const { contents } = v.contents[1].source;

  // return contents === id.value
}

const abilities = computed(() => {
  return choices
    .value
    .reduce<number[]>((acc, v, i) => {
      if (isActivate(v)) {
        return [...acc, i];
      }

      return acc;
    }, []);
})
</script>

<template>
  <div class="card-container">
    <img
      :class="{'card--can-interact': cardAction !== -1}"
      class="card"
      :src="image"
      @click="emit('choose', cardAction)"
    />
    <button
      v-for="ability in abilities"
      :key="ability"
      class="button"
      :class="{ 'reaction-ability-button': isReactionAbility(ability) }"
      @click="emit('choose', ability)"
      >{{abilityLabel(ability)}}</button>
  </div>
</template>

<style scoped lang="scss">

.card {
  width: $card-width;
  min-width: $card-width;
  border-radius: 7px;
  box-shadow: 0 3px 6px rgba(0,0,0,0.23), 0 3px 6px rgba(0,0,0,0.53);
  border-radius: 6px;
  margin: 2px;
  display: inline-block;

  &--can-interact {
    border: 2px solid $select;
    cursor: pointer;
  }
}

.reaction-ability-button {
  background-color: #A02ECB;
  &:before {
    font-family: "arkham";
    content: "\0059";
    margin-right: 5px;
  }
}

.button{
  margin-top: 2px;
  border: 0;
  color: #fff;
  border-radius: 4px;
  border: 1px solid #ff00ff;
}

.card-container {
  display: flex;
  flex-direction: column;
}
</style>
