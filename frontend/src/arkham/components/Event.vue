<script lang="ts" setup>
import { computed, inject } from 'vue';
import { Game } from '@/arkham/types/Game';
import * as ArkhamGame from '@/arkham/types/Game';
import { Message, MessageType } from '@/arkham/types/Message';
import PoolItem from '@/arkham/components/PoolItem.vue';
import AbilityButton from '@/arkham/components/AbilityButton.vue'
import * as Arkham from '@/arkham/types/Event';

export interface Props {
  game: Game
  event: Arkham.Event
  investigatorId: string
}

const props = defineProps<Props>()

const id = computed(() => props.event.id)
const hasPool = computed(() => {
  const { doom } = props.event
  return doom > 0
})

const cardCode = computed(() => props.event.cardCode)
const baseUrl = inject('baseUrl')
const image = computed(() => {
  return `${baseUrl}/img/arkham/cards/${cardCode.value.replace('c', '')}.jpg`
})
const choices = computed(() => ArkhamGame.choices(props.game, props.investigatorId))

function canInteract(c: Message): boolean {
  switch (c.tag) {
    case MessageType.DISCARD:
      return c.contents.contents === id.value
    case MessageType.READY:
      return c.contents.contents === id.value
    case MessageType.REMOVE_DOOM:
      return c.contents[0].contents === id.value
    case MessageType.USE_CARD_ABILITY:
      return c.contents[1].contents === id.value
    case MessageType.RUN:
      return c.contents.some((c1: Message) => canInteract(c1))
    case MessageType.TARGET_LABEL:
      return c.contents[0].tag === "EventTarget" && c.contents[0].contents === id.value
    default:
      return false;
  }
}

const cardAction = computed(() => choices.value.findIndex(canInteract))

function isActivate(v: Message) {
  if (v.tag !== 'UseAbility') {
    return false
  }

  const { tag, contents } = v.contents[1].source;

  if (tag === 'EventSource' && contents === id.value) {
    return true
  }

  if (tag === 'ProxySource' && contents[0].tag === 'EventSource' && contents[0].contents === id.value) {
    return true
  }

  return false
}

const abilities = computed(() => {
  return choices
    .value
    .reduce<number[]>((acc, v, i) => {
      if (v.tag === 'Run' && isActivate(v.contents[0])) {
        return [...acc, i];
      } else if (isActivate(v)) {
        return [...acc, i];
      }

      return acc;
    }, []);
})
</script>

<template>
  <div class="event">
    <img
      :src="image"
      :class="{ 'event--can-interact': cardAction !== -1 }"
      class="card event"
      @click="$emit('choose', cardAction)"
    />
    <AbilityButton
      v-for="ability in abilities"
      :key="ability"
      :ability="choices[ability]"
      :data-image="image"
      @click="$emit('choose', ability)"
      />
    <div v-if="hasPool" class="pool">
      <PoolItem v-if="event.doom > 0" type="doom" :amount="event.doom" />
    </div>
  </div>
</template>

<style lang="scss" scoped>
.card {
  width: $card-width;
  max-width: $card-width;
  border-radius: 5px;
}

.event {
  display: flex;
  flex-direction: column;
}

.event--can-interact {
  border: 2px solid $select;
  cursor:pointer;
}

.pool {
  display: flex;
  flex-direction: row;
  height: 2em;
  justify-content: center;
}

.button{
  margin-top: 2px;
  border: 0;
  color: #fff;
  border-radius: 4px;
  border: 1px solid #ff00ff;
}
</style>
