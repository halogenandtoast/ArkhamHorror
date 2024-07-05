<script lang="ts" setup>
import { watch, computed, ref } from 'vue'
import { useDebug } from '@/arkham/debug'
import { Game } from '@/arkham/types/Game'
import { TokenType } from '@/arkham/types/Token';
import { imgsrc } from '@/arkham/helpers';
import * as ArkhamGame from '@/arkham/types/Game'
import { AbilityLabel, AbilityMessage, Message, MessageType } from '@/arkham/types/Message'
import PoolItem from '@/arkham/components/PoolItem.vue'
import Key from '@/arkham/components/Key.vue';
import AbilityButton from '@/arkham/components/AbilityButton.vue'
import Treachery from '@/arkham/components/Treachery.vue';
import Asset from '@/arkham/components/Asset.vue';
import Event from '@/arkham/components/Event.vue';
import Token from '@/arkham/components/Token.vue';
import Story from '@/arkham/components/Story.vue';
import * as Arkham from '@/arkham/types/Enemy'

const props = withDefaults(defineProps<{
  game: Game
  enemy: Arkham.Enemy
  playerId: string
  atLocation?: boolean
}>(), { atLocation: false })

const emits = defineEmits<{
  choose: [value: number]
}>()

const investigatorId = computed(() => Object.values(props.game.investigators).find((i) => i.playerId === props.playerId)?.id)

const enemyStory = computed(() => {
  const { stories } = props.game
  return Object.values(stories).find((s) => s.otherSide?.contents === props.enemy.id)
})

const isTrueForm = computed(() => {
  const { cardCode } = props.enemy
  return cardCode === 'cxnyarlathotep'
})

const image = computed(() => {
  const { cardCode, flipped } = props.enemy
  const suffix = flipped ? 'b' : ''
  return imgsrc(`cards/${cardCode.replace('c', '')}${suffix}.jpg`)
})

const id = computed(() => props.enemy.id)

const choices = computed(() => ArkhamGame.choices(props.game, props.playerId))

function isCardAction(c: Message): boolean {
  if (c.tag === MessageType.TARGET_LABEL && c.target.contents === id.value) {
    return true
  }
  return false
}

const cardAction = computed(() => choices.value.findIndex(isCardAction))
const canInteract = computed(() => abilities.value.length > 0 || cardAction.value !== -1)

const swarmEnemies = computed(() =>
  Object.values(props.game.enemies).filter((e) => e.placement.tag === 'AsSwarm' && e.placement.swarmHost === props.enemy.id)
)

function isAbility(v: Message): v is AbilityLabel {
  if (v.tag === MessageType.FIGHT_LABEL && v.enemyId === id.value) {
    return true
  }

  if (v.tag === MessageType.EVADE_LABEL && v.enemyId === id.value) {
    return true
  }

  if (v.tag === MessageType.ENGAGE_LABEL && v.enemyId === id.value) {
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

const abilities = computed<AbilityMessage[]>(() => {
  return choices.value
    .reduce<AbilityMessage[]>((acc, v, i) =>
      isAbility(v) ? [...acc, { contents: v, displayAsAction: false, index: i}] : acc
    , [])
})

const isExhausted = computed(() => props.enemy.exhausted)

const keys = computed(() => props.enemy.keys)

const debug = useDebug()

const enemyDamage = computed(() => (props.enemy.tokens[TokenType.Damage] || 0) + props.enemy.assignedDamage)
const doom = computed(() => props.enemy.tokens[TokenType.Doom])
const clues = computed(() => props.enemy.tokens[TokenType.Clue])
const resources = computed(() => props.enemy.tokens[TokenType.Resource])
const leylines = computed(() => props.enemy.tokens[TokenType.Leyline])
const lostSouls = computed(() => props.enemy.tokens[TokenType.LostSoul])
const bounties = computed(() => props.enemy.tokens[TokenType.Bounty])
const evidence = computed(() => props.enemy.tokens[TokenType.Evidence])

const omnipotent = computed(() => {
  const {modifiers} = props.enemy

  return modifiers.some(modifier =>
    modifier.type.tag === "OtherModifier" && modifier.type.contents === "Omnipotent"
  )
})

const health = computed(() => {
  return props.enemy.health?.tag == "Static" ? props.enemy.health.contents : null
})

const gainedVictory = computed(() => {
  const {modifiers} = props.enemy

  return modifiers.reduce((acc, modifier) =>
    acc + (modifier.type.tag === "GainVictory" ? modifier.type.contents : 0)
  , 0)
})

function mapMaybe<T, U>(arr: T[], fn: (item: T) => U | null | undefined): U[] {
  return arr.reduce((acc: U[], item: T) => {
    const result = fn(item);
    if (result !== null && result !== undefined) {
      acc.push(result);
    }
    return acc;
  }, []);
}

const addedKeywords = computed(() => {
  const {modifiers} = props.enemy
  return mapMaybe(modifiers, modifier => modifier.type.tag === "AddKeyword" ? modifier.type.contents : null).join(". ")
})

const choose = (index: number) => emits('choose', index)

const showAbilities = ref<boolean>(false)

async function clicked() {
  if(cardAction.value !== -1) {
    emits('choose', cardAction.value)
  } else if (abilities.value.length > 0) {
    showAbilities.value = !showAbilities.value
  }
}

async function chooseAbility(ability: number) {
  showAbilities.value = false
  emits('choose', ability)
}

watch(abilities, (abilities) => {
  // ability is forced we must show
  if (abilities.some(a => "ability" in a.contents && a.contents.ability.type.tag === "ForcedAbility")) {
    showAbilities.value = true
  }

  if (abilities.length === 0) {
    showAbilities.value = false
  }
})

</script>

<template>
  <div class="enemy--outer">
    <div class="enemy">
      <Story v-if="enemyStory" :story="enemyStory" :game="game" :playerId="playerId" @choose="choose"/>
      <template v-else>
        <div class="card-frame">
          <div class="card-wrapper" :class="{ 'enemy--can-interact': canInteract, exhausted: isExhausted}">
            <img v-if="isTrueForm" :src="image"
              class="card enemy"
              :data-id="id"
              :data-fight="enemy.fight"
              :data-evade="enemy.evade"
              :data-health="health"
              :data-damage="enemy.healthDamage"
              :data-horror="enemy.sanityDamage"
              :data-victory="gainedVictory"
              :data-keywords="addedKeywords"
              @click="clicked"
            />
            <img v-else
              :src="image"
              class="card enemy"
              :data-id="id"
              @click="clicked"
            />
          </div>

          <div class="pool">
            <div class="keys" v-if="keys.length > 0">
              <Key v-for="key in keys" :key="key" :name="key" />
            </div>
            <PoolItem v-if="!omnipotent" type="health" :amount="enemyDamage" />
            <PoolItem v-if="doom && doom > 0" type="doom" :amount="doom" />
            <PoolItem v-if="clues && clues > 0" type="clue" :amount="clues" />
            <PoolItem v-if="resources && resources > 0" type="resource" :amount="resources" />
            <PoolItem v-if="leylines && leylines > 0" type="resource" tooltip="Leyline" :amount="leylines" />
            <PoolItem v-if="lostSouls && lostSouls > 0" type="resource" :amount="lostSouls" />
            <PoolItem v-if="bounties && bounties > 0" type="resource" :amount="bounties" />
            <PoolItem v-if="evidence && evidence > 0" type="resource" :amount="evidence" />
            <Token
              v-for="(sealedToken, index) in enemy.sealedChaosTokens"
              :key="index"
              :token="sealedToken"
              :playerId="playerId"
              :game="game"
              @choose="choose"
            />
          </div>

          <div v-if="showAbilities" class="abilities">
            <AbilityButton
              v-for="ability in abilities"
              :key="ability.index"
              :ability="ability.contents"
              @click="chooseAbility(ability.index)"
              />
          </div>
        </div>

      </template>
      <Treachery
        v-for="treacheryId in enemy.treacheries"
        :key="treacheryId"
        :treachery="game.treacheries[treacheryId]"
        :game="game"
        :playerId="playerId"
        :attached="true"
        :class="{ 'small-treachery': atLocation }"
        @choose="$emit('choose', $event)"
      />
      <Asset
        v-for="assetId in enemy.assets"
        :key="assetId"
        :asset="game.assets[assetId]"
        :game="game"
        :playerId="playerId"
        @choose="$emit('choose', $event)"
      />
      <Event
        v-for="eventId in enemy.events"
        :key="eventId"
        :event="game.events[eventId]"
        :game="game"
        :playerId="playerId"
        @choose="$emit('choose', $event)"
      />
      <template v-if="debug.active">
        <button @click="debug.send(game.id, {tag: 'DefeatEnemy', contents: [id, investigatorId, {tag: 'InvestigatorSource', contents:investigatorId}]})">Defeat</button>
        <button @click="debug.send(game.id, {tag: 'EnemyEvaded', contents: [investigatorId, id]})">Evade</button>
      </template>
    </div>

    <div class="swarm" v-if="swarmEnemies.length > 0">
      <Enemy
        v-for="enemy in swarmEnemies"
        :key="enemy.id"
        :enemy="enemy"
        :game="game"
        :playerId="playerId"
        :atLocation="true"
        @choose="$emit('choose', $event)"
        class="enemy--swarming"
      />
    </div>
  </div>
</template>

<style scoped lang="scss">
.small-treachery :deep(.card) {
  height: $card-width * 0.35;
}

.enemy--can-interact {
  border: 3px solid $select;
  border-radius: 5px;
  cursor: pointer;
}

.enemy {
  display: flex;
  flex-direction: column;
  position: relative;
  z-index: 0;
  isolation: isolate;
}

.card {
  width: $card-width;
  max-width: $card-width;
  border-radius: 5px;
}

.pool {
  position: absolute;
  top: 10%;
  align-items: center;
  display: flex;
  align-self: flex-start;
  align-items: flex-end;
  * {
    transform: scale(0.6);
  }

  pointer-events: none;
}

.exhausted {
  transform: rotate(90deg);
  margin-left: 2px;
  margin-bottom: -10px;
  height: fit-content;
  width: auto;
}

:deep(.token) {
  width: 30px;
}

.card-frame {
  z-index: 10;
  isolation: isolate;
  position: relative;
  display: flex;
  align-items: center;
  justify-content: center;
}

.abilities {
  position: absolute;
  padding: 10px;
  background: rgba(0, 0, 0, 0.8);
  border-radius: 10px;
  display: grid;
  gap: 5px;
  bottom:100%;
  left: 0;
  z-index: 20000000000;
}

.swarm {
  display: flex;
  flex-direction: row-reverse;
  justify-content: space-evenly;
  z-index: -1000;
}

.enemy--swarming {
  margin-left: calc(($card-width / 1.5) * -1);
}

.enemy--outer {
  isolation: isolate;
  display: flex;
  justify-content: space-evenly;
}
</style>
