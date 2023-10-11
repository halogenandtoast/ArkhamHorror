<script lang="ts" setup>
import { computed, inject, Ref } from 'vue'
import { CardContents, type Card } from '@/arkham/types/Card'
import type { Game } from '@/arkham/types/Game'
import type { AbilityLabel, AbilityMessage, Message } from '@/arkham/types/Message'
import { MessageType} from '@/arkham/types/Message'
import { imgsrc } from '@/arkham/helpers'
import AbilityButton from '@/arkham/components/AbilityButton.vue'
import * as ArkhamGame from '@/arkham/types/Game'

export interface Props {
  game: Game
  card: Card
  playerId: string
  ownerId: string
}

const props = defineProps<Props>()

const investigatorId = computed(() => Object.values(props.game.investigators).find((i) => i.playerId === props.playerId)?.id)

const cardContents = computed<CardContents>(() =>
  props.card.tag == 'VengeanceCard' ? props.card.contents.contents : props.card.contents)

const id = computed(() => cardContents.value.id)
const choices = computed(() => ArkhamGame.choices(props.game, props.playerId))

const cardAction = computed(() => {
  return choices.value.findIndex((choice) => {
    if (choice.tag === MessageType.TARGET_LABEL) {
      return choice.target.contents === id.value
    }

    return false
  })
})

const solo = inject<Ref<boolean>>('solo')

function isAbility(v: Message): v is AbilityLabel {
  if (v.tag !== 'AbilityLabel') {
    return false
  }

  const { source } = v.ability;

  if (source.sourceTag === 'ProxySource') {
    if ("contents" in source.source) {
      return source.source.contents === id.value
    }
  } else if (source.tag === 'CardIdSource') {
    return source.contents === id.value
  } else if (source.tag === 'EventSource') {
    return source.contents === id.value
  } else if (source.tag === 'AssetSource') {
    return source.contents === id.value
  }

  return false
}

const abilities = computed(() => {
  return choices
    .value
    .reduce<AbilityMessage[]>((acc, v, i) => {
      if (isAbility(v)) {
        return [...acc, { contents: v, index: i}];
      }

      return acc;
    }, []);
})

const classObject = computed(() => {
  return {
    'card--can-interact': cardAction.value !== -1,
    // 'card--committed': uncommitCardAction.value !== -1,
  }
})

const cardBack = computed(() => {
  return imgsrc("player_back.jpg")
})

const image = computed(() => {
  const { cardCode } = cardContents.value;
  return imgsrc(`cards/${cardCode.replace('c', '')}.jpg`);
})
</script>

<template>
  <div class="card-container" :data-index="id" v-if="solo || (investigatorId == ownerId)">
    <img
      :class="classObject"
      class="card"
      :src="image"
      @click="$emit('choose', cardAction)"
    />

    <AbilityButton
      v-for="ability in abilities"
      :key="ability.index"
      :ability="ability.contents"
      :data-image="image"
      @click="$emit('choose', ability.index)"
      />

  </div>
  <div class="card-container" v-else>
    <img class="card" :src="cardBack" />
  </div>
</template>

<style scoped lang="scss">

.card {
  width: $card-width;
  min-width: $card-width;
  border-radius: 7px;
  box-shadow: 0 3px 6px rgba(0,0,0,0.23), 0 3px 6px rgba(0,0,0,0.53);
  border-radius: 6px;
  display: flex;
  border: 2px solid rgba(0, 0, 0, 0);

  &--can-interact {
    border: 2px solid $select;
    cursor: pointer;
  }

  &--committed {
    margin-top: -10px;
  }
}
</style>
