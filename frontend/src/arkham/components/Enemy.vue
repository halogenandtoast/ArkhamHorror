<script lang="ts" setup>
import { computed } from 'vue'
import { useDebug } from '@/arkham/debug'
import { Game } from '@/arkham/types/Game'
import { TokenType } from '@/arkham/types/Token';
import { imgsrc } from '@/arkham/helpers';
import * as ArkhamGame from '@/arkham/types/Game'
import { AbilityLabel, AbilityMessage, EvadeLabel, FightLabel, Message, MessageType } from '@/arkham/types/Message'
import PoolItem from '@/arkham/components/PoolItem.vue'
import Key from '@/arkham/components/Key.vue';
import AbilityButton from '@/arkham/components/AbilityButton.vue'
import Treachery from '@/arkham/components/Treachery.vue';
import Asset from '@/arkham/components/Asset.vue';
import Token from '@/arkham/components/Token.vue';
import Story from '@/arkham/components/Story.vue';
import * as Arkham from '@/arkham/types/Enemy'

const props = withDefaults(defineProps<{
  game: Game
  enemy: Arkham.Enemy
  investigatorId: string
  atLocation?: boolean
}>(), { atLocation: false })

const emits = defineEmits<{
  choose: [value: number]
}>()

const enemyStory = computed(() => {
  const { stories } = props.game
  return Object.values(stories).find((s) => s.otherSide?.contents === props.enemy.id)
})

const image = computed(() => {
  const { cardCode } = props.enemy
  return imgsrc(`cards/${cardCode.replace('c', '')}.jpg`);
})

const id = computed(() => props.enemy.id)

const choices = computed(() => ArkhamGame.choices(props.game, props.investigatorId))

function canInteract(c: Message): boolean {
  if (c.tag === MessageType.TARGET_LABEL && c.target.contents === id.value) {
    return true
  }
  return false
}

const cardAction = computed(() => choices.value.findIndex(canInteract))

function isAbility(v: Message): v is AbilityLabel | FightLabel | EvadeLabel {
  if (v.tag === MessageType.FIGHT_LABEL && v.enemyId === id.value) {
    return true
  }

  if (v.tag === MessageType.EVADE_LABEL && v.enemyId === id.value) {
    return true
  }

  if (v.tag !== MessageType.ABILITY_LABEL) {
    return false
  }

  const { source } = v.ability

  if (source.sourceTag === 'ProxySource') {
    if ("contents" in source.source) {
      return source.source.contents === id.value
    }
  } else if (source.tag === 'EnemySource') {
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

const isExhausted = computed(() => props.enemy.exhausted)

const keys = computed(() => props.enemy.keys)

const debug = useDebug()

const enemyDamage = computed(() => (props.enemy.tokens[TokenType.Damage] || 0) + props.enemy.assignedDamage)
const doom = computed(() => props.enemy.tokens[TokenType.Doom])
const clues = computed(() => props.enemy.tokens[TokenType.Clue])
const resources = computed(() => props.enemy.tokens[TokenType.Resource])
const lostSouls = computed(() => props.enemy.tokens[TokenType.LostSoul])

const choose = (index: number) => emits('choose', index)

</script>

<template>
  <div class="enemy">
    <Story v-if="enemyStory" :story="enemyStory" :game="game" :investigatorId="investigatorId" @choose="choose"/>
    <template v-else>
      <img :src="image"
        :class="{'enemy--can-interact': cardAction !== -1, exhausted: isExhausted }"
        class="card enemy"
        @click="$emit('choose', cardAction)"
      />
      <AbilityButton
        v-for="ability in abilities"
        :key="ability.index"
        :ability="ability.contents"
        :data-image="image"
        @click="$emit('choose', ability.index)"
        />
    </template>
    <div class="pool">
      <div class="keys" v-if="keys.length > 0">
        <Key v-for="key in keys" :key="key" :name="key" />
      </div>
      <PoolItem type="health" :amount="enemyDamage" />
      <PoolItem v-if="doom && doom > 0" type="doom" :amount="doom" />
      <PoolItem v-if="clues && clues > 0" type="clue" :amount="clues" />
      <PoolItem v-if="resources && resources > 0" type="resource" :amount="resources" />
      <PoolItem v-if="lostSouls && lostSouls > 0" type="resource" :amount="lostSouls" />
      <Token
        v-for="(sealedToken, index) in enemy.sealedChaosTokens"
        :key="index"
        :token="sealedToken"
        :investigatorId="investigatorId"
        :game="game"
        @choose="choose"
      />
    </div>
    <Treachery
      v-for="treacheryId in enemy.treacheries"
      :key="treacheryId"
      :treachery="game.treacheries[treacheryId]"
      :game="game"
      :investigatorId="investigatorId"
      :attached="true"
      :class="{ 'small-treachery': atLocation }"
      @choose="$emit('choose', $event)"
    />
    <Asset
      v-for="assetId in enemy.assets"
      :key="assetId"
      :asset="game.assets[assetId]"
      :game="game"
      :investigatorId="investigatorId"
      @choose="$emit('choose', $event)"
    />
    <template v-if="debug.active">
      <button @click="debug.send(game.id, {tag: 'DefeatEnemy', contents: [id, investigatorId, {tag: 'TestSource', contents:[]}]})">Defeat</button>
    </template>
  </div>
</template>

<style scoped lang="scss">
.small-treachery :deep(.card) {
  height: $card-width * 0.35;
}

.enemy--can-interact {
  border: 3px solid $select;
  border-radius: 15px;
  cursor: pointer;
}

.enemy {
  display: flex;
  flex-direction: column;
}

.button{
  margin-top: 2px;
  border: 0;
  color: #fff;
  border-radius: 4px;
  border: 1px solid #ff00ff;
}

.card {
  width: $card-width;
  max-width: $card-width;
  border-radius: 5px;
}

.pool {
  display: flex;
  flex-direction: row;
  height: 2em;
}

.button{
  margin-top: 2px;
  border: 0;
  color: #fff;
  border-radius: 4px;
  border: 1px solid #ff00ff;
}

.exhausted {
  transform: rotate(90deg);
  margin-left: 13px;
  margin-bottom: -10px;
  height: 80px;
  width: auto;
}

:deep(.token) {
  width: 30px;
}
</style>
