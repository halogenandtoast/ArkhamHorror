<script lang="ts" setup>
import { computed } from 'vue';
import { imgsrc } from '@/arkham/helpers';
import { TokenType } from '@/arkham/types/Token';
import type { Card, CardContents } from '@/arkham/types/Card';
import type { Game } from '@/arkham/types/Game';
import type { Enemy } from '@/arkham/types/Enemy';
import * as ArkhamGame from '@/arkham/types/Game';
import type { AbilityLabel, AbilityMessage, Message } from '@/arkham/types/Message';
import { MessageType } from '@/arkham/types/Message';
import AbilityButton from '@/arkham/components/AbilityButton.vue'
import PoolItem from '@/arkham/components/PoolItem.vue'

const props = withDefaults(defineProps<{
  game: Game
  card: Card | CardContents
  revealed?: boolean
  playerId: string
}>(), { revealed: false })

const emit = defineEmits<{
  choose: [value: number]
}>()

const cardContents = computed<CardContents>(() => props.card.tag === "CardContents" ? props.card : (
  props.card.tag === "VengeanceCard" ? props.card.contents.contents : props.card.contents))

const image = computed(() => {
  if (props.card.tag === 'VengeanceCard') {
    const back = props.card.contents.tag === 'PlayerCard' ? 'player_back' : 'encounter/back'
    return imgsrc(`${back}.jpg`);
  }

  const { cardCode, isFlipped } = cardContents.value
  const suffix = !props.revealed && isFlipped ? 'b' : ''
  return imgsrc(`cards/${cardCode.replace(/^c/, '')}${suffix}.jpg`)
})

const id = computed(() => props.card.tag === 'VengeanceCard' ? props.card.contents.contents.id : cardContents.value.id)
const choices = computed(() => ArkhamGame.choices(props.game, props.playerId))

function canInteract(c: Message): boolean {
  if (c.tag === MessageType.TARGET_LABEL) {
    return c.target.contents === id.value
  }

  return false
}

const cardAction = computed(() => {
  return choices.value.findIndex(canInteract)
})


function isAbility(v: Message): v is AbilityLabel {
  if (v.tag !== MessageType.ABILITY_LABEL) {
    return false
  }

  const { source } = v.ability;

  if (source.sourceTag === 'ProxySource') {
    if ("contents" in source.source) {
      return source.source.contents === id.value
    }
  } else {
    return source.contents === id.value
  }

  return false
}

const abilities = computed(() => {
  return choices.value
    .reduce<AbilityMessage[]>((acc, v, i) => {
      if (isAbility(v)) {
        return [...acc, { contents: v, index: i }];
      }

      return acc;
    }, []);
})

const outOfPlayEnemy = computed<Enemy | null>(() => {
  return Object.values(props.game.outOfPlayEnemies).find(e => e.cardId === id.value)
})

const tokens = computed(() => {
  if (outOfPlayEnemy.value) {
    return outOfPlayEnemy.value.tokens
  }
  return cardContents.value.tokens || {}
})

const doom = computed(() => tokens.value[TokenType.Doom])
const clues = computed(() => tokens.value[TokenType.Clue])
const resources = computed(() => tokens.value[TokenType.Resource])
const damage = computed(() => tokens.value[TokenType.Damage])
const horror = computed(() => tokens.value[TokenType.Horror])
const lostSouls = computed(() => tokens.value[TokenType.LostSoul])

const hasPool = computed(() => {
  return doom.value || clues.value || resources.value || damage.value || horror.value || lostSouls.value
})

</script>

<template>
  <div class="card-container" :data-index="card.id">
    <canvas ref="canvas" class="card">
      <img
        :class="{'card--can-interact': cardAction !== -1}"
        class="card"
        :src="image"
        @click="emit('choose', cardAction)"
      />
    </canvas>
    <span class="vengeance" v-if="card.tag === 'VengeanceCard'">Vengeance 1</span>
    <div class="pool" v-if="hasPool">
      <PoolItem v-if="damage" type="doom" :amount="damage" />
      <PoolItem v-if="horror" type="horror" :amount="horror" />
      <PoolItem v-if="doom" type="doom" :amount="doom" />
      <PoolItem v-if="clues" type="clue" :amount="clues" />
      <PoolItem v-if="resources" type="resource" :amount="resources" />
      <PoolItem v-if="lostSouls" type="resource" :amount="lostSouls" />
    </div>
    <AbilityButton
      v-for="ability in abilities"
      :key="ability.index"
      :ability="ability.contents"
      :data-image="image"
      @click="$emit('choose', ability.index)"
      />
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

.vengeance {
  position: absolute;
  top: 50%;
  background: rgba(0, 0, 0, 0.6);
  color: white;
  font-size: 0.8em;
  left: 50%;
  transform: translateX(-50%) translateY(-50%);
  text-align: center;
  padding: 5px;
}

.card-container {
  display: flex;
  flex-direction: column;
  position: relative;
}
</style>
