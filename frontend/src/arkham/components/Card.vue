<script lang="ts" setup>
import { computed } from 'vue';
import { imgsrc } from '@/arkham/helpers';
import { TokenType } from '@/arkham/types/Token';
import type { Card, CardContents } from '@/arkham/types/Card';
import type { Game } from '@/arkham/types/Game';
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
    const back = props.card.contents.tag === 'PlayerCard' ? 'player_back' : 'encounter_back'
    return imgsrc(`${back}.jpg`);
  }

  const { cardCode, isFlipped, mutated } = cardContents.value
  if (cardCode === "c05178b" && !isFlipped) {
    return imgsrc(`cards/${cardCode.replace(/^c/, '').replace(/b$/, 'a')}.avif`)
  }
  if (cardCode === "c05178d" && !isFlipped) {
    return imgsrc(`cards/${cardCode.replace(/^c/, '').replace(/d$/, 'c')}.avif`)
  }
  if (cardCode === "c05178f" && !isFlipped) {
    return imgsrc(`cards/${cardCode.replace(/^c/, '').replace(/f$/, 'e')}.avif`)
  }
  if (cardCode === "c05178h" && !isFlipped) {
    return imgsrc(`cards/${cardCode.replace(/^c/, '').replace(/h$/, 'g')}.avif`)
  }
  if (cardCode === "c05178j" && !isFlipped) {
    return imgsrc(`cards/${cardCode.replace(/^c/, '').replace(/j$/, 'i')}.avif`)
  }
  if (cardCode === "c05178l" && !isFlipped) {
    return imgsrc(`cards/${cardCode.replace(/^c/, '').replace(/l$/, 'k')}.avif`)
  }
  const suffix = !props.revealed && isFlipped ? 'b' : ''
  const mutatedSuffix = mutated ? `_${mutated}` : ''
  return imgsrc(`cards/${cardCode.replace(/^c/, '')}${suffix}${mutatedSuffix}.avif`)
})

const id = computed(() => props.card.tag === 'VengeanceCard' ? props.card.contents.contents.id : cardContents.value.id)
const choices = computed(() => ArkhamGame.choices(props.game, props.playerId))

function canInteract(c: Message): boolean {
  if (c.tag === MessageType.TARGET_LABEL) {
    if (c.target.tag === 'SkillTarget') {
      if (props.game.skills[c.target.contents].cardId == id.value) {
        return true
      }
    }
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

const abilities = computed<AbilityMessage[]>(() => {
  return choices.value
    .reduce<AbilityMessage[]>((acc, v, i) => {
      if (isAbility(v)) {
        return [...acc, { contents: v, displayAsAction: false, index: i }];
      }

      return acc;
    }, []);
})

const tokens = computed(() => {
  return cardContents.value.tokens || {}
})

const doom = computed(() => tokens.value[TokenType.Doom])
const clues = computed(() => tokens.value[TokenType.Clue])
const resources = computed(() => tokens.value[TokenType.Resource])
const damage = computed(() => tokens.value[TokenType.Damage])
const horror = computed(() => tokens.value[TokenType.Horror])
const lostSouls = computed(() => tokens.value[TokenType.LostSoul])
const leylines = computed(() => tokens.value[TokenType.Leyline])

const hasPool = computed(() => {
  return doom.value || clues.value || resources.value || damage.value || horror.value || lostSouls.value
})

const forceSideways = computed(() => {
  const { cardCode, isFlipped } = cardContents.value
  if (isFlipped) {
    return ["c04242"].includes(cardCode)
  }
  return false
})

</script>

<template>
  <div class="card-container" :data-index="id">
    <img
      :class="{'card--can-interact': cardAction !== -1, 'sideways': forceSideways}"
      class="card"
      :src="image"
      :data-customizations="JSON.stringify(cardContents.customizations)"
      @click="emit('choose', cardAction)"
    />
    <span class="vengeance" v-if="card.tag === 'VengeanceCard'">{{$t('card.vengeance', {value: 1})}}</span>
    <div class="pool" v-if="hasPool">
      <PoolItem v-if="damage" type="doom" :amount="damage" />
      <PoolItem v-if="horror" type="horror" :amount="horror" />
      <PoolItem v-if="doom" type="doom" :amount="doom" />
      <PoolItem v-if="clues" type="clue" :amount="clues" />
      <PoolItem v-if="resources" type="resource" :amount="resources" />
      <PoolItem v-if="lostSouls" type="resource" :amount="lostSouls" />
      <PoolItem v-if="leylines" type="resource" :amount="leylines" />
    </div>
    <AbilityButton
      v-for="ability in abilities"
      :key="ability.index"
      :ability="ability.contents"
      :data-image="image"
      :game="game"
      @click="$emit('choose', ability.index)"
      />
  </div>
</template>

<style scoped lang="scss">

.card {
  width: var(--card-width);
  min-width: var(--card-width);
  border-radius: 7px;
  box-shadow: 0 3px 6px rgba(0,0,0,0.23), 0 3px 6px rgba(0,0,0,0.53);
  border-radius: 6px;
  margin: 2px;
  display: inline-block;

  &--can-interact {
    border: 2px solid var(--select);
    cursor: pointer;
  }
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
