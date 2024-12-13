<script lang="ts" setup>
import { computed, ref } from 'vue'
import { useDebug } from '@/arkham/debug'
import { Game } from '@/arkham/types/Game'
import { TokenType } from '@/arkham/types/Token';
import { imgsrc } from '@/arkham/helpers';
import * as ArkhamGame from '@/arkham/types/Game'
import { AbilityLabel, AbilityMessage, Message, MessageType } from '@/arkham/types/Message'
import AbilitiesMenu from '@/arkham/components/AbilitiesMenu.vue'
import DebugEnemy from '@/arkham/components/debug/Enemy.vue';
import PoolItem from '@/arkham/components/PoolItem.vue'
import Key from '@/arkham/components/Key.vue';
import Treachery from '@/arkham/components/Treachery.vue';
import Asset from '@/arkham/components/Asset.vue';
import Event from '@/arkham/components/Event.vue';
import Skill from '@/arkham/components/Skill.vue';
import Token from '@/arkham/components/Token.vue';
import Story from '@/arkham/components/Story.vue';
import * as Arkham from '@/arkham/types/Enemy'

const props = withDefaults(defineProps<{
  game: Game
  enemy: Arkham.Enemy
  playerId: string
  atLocation?: boolean
  attached?: boolean
}>(), { atLocation: false, attached: false })

const emits = defineEmits<{
  choose: [value: number]
}>()


const frame = ref(null)
const debugging = ref(false)
const dragging = ref(false)
const enemyStory = computed(() => {
  const { stories } = props.game
  return Object.values(stories).find((s) => s.otherSide?.contents === props.enemy.id)
})

const isTrueForm = computed(() => {
  const { cardCode } = props.enemy
  return cardCode === 'cxnyarlathotep'
})

const imageId = computed(() => {
  const { cardCode, flipped } = props.enemy
  const suffix = flipped ? 'b' : ''
  return `${cardCode.replace('c', '')}${suffix}`
})

const image = computed(() => {
  return imgsrc(`cards/${imageId.value}.avif`)
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

const inVoid = computed(() => props.enemy.placement.tag === 'OutOfPlay' && props.enemy.placement.contents === 'VoidZone')
const global = computed(() => props.enemy.placement.tag === 'OtherPlacement' && props.enemy.placement.contents === 'Global')

const swarmEnemies = computed(() =>
  Object.values(props.game.enemies).filter((e) => e.placement.tag === 'AsSwarm' && e.placement.swarmHost === props.enemy.id)
)

const isSwarm = computed(() => props.enemy.placement.tag === 'AsSwarm')

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

function startDrag(event: DragEvent, enemy: Arkham.Enemy) {
  dragging.value = true
  if (event.dataTransfer) {
    event.dataTransfer.effectAllowed = 'move'
    event.dataTransfer.setData('text/plain', JSON.stringify({ "tag": "EnemyTarget", "contents": enemy.id }))
  }
}

</script>

<template>
  <div class="enemy--outer" :class="{showAbilities}">
    <div class="enemy">
      <Story v-if="enemyStory" :story="enemyStory" :game="game" :playerId="playerId" @choose="choose"/>
      <template v-else>
        <div class="card-frame" ref="frame">
          <div class="card-wrapper" :class="{ exhausted: isExhausted }">
            <img v-if="isTrueForm" :src="image"
              class="card enemy"
              :class="{ dragging, 'enemy--can-interact': canInteract, attached }"
              :data-id="id"
              :data-fight="enemy.fight"
              :data-evade="enemy.evade"
              :data-health="health"
              :data-damage="enemy.healthDamage"
              :data-horror="enemy.sanityDamage"
              :data-victory="gainedVictory"
              :data-keywords="addedKeywords"
              :data-swarm="isSwarm"
              @click="clicked"
            />
            <img v-else
              :draggable="debug.active"
              @dragstart="startDrag($event, enemy)"
              :src="isSwarm ? imgsrc('player_back.jpg') : image"
              class="card enemy"
              :class="{ exhausted: isExhausted, 'enemy--can-interact': canInteract, attached}"
              :data-id="id"
              :data-image-id="imageId"
              :data-swarm="isSwarm || undefined"
              @click="clicked"
            />
          </div>

          <div class="pool">
            <div class="keys" v-if="keys.length > 0">
              <Key v-for="key in keys" :key="key" :name="key" />
            </div>
            <PoolItem v-if="!omnipotent && !attached" type="health" :amount="enemyDamage" />
            <PoolItem v-if="doom && doom > 0" type="doom" :amount="doom" />
            <PoolItem v-if="clues && clues > 0" type="clue" :amount="clues" />
            <PoolItem v-if="resources && resources > 0" type="resource" :amount="resources" />
            <PoolItem v-if="leylines && leylines > 0" type="resource" tooltip="Leyline" :amount="leylines" />
            <PoolItem v-if="lostSouls && lostSouls > 0" type="resource" :amount="lostSouls" />
            <PoolItem v-if="bounties && bounties > 0" type="resource" :amount="bounties" />
            <PoolItem v-if="evidence && evidence > 0" type="resource" tooltip="Evidence" :amount="evidence" />
            <PoolItem v-if="enemy.cardsUnderneath.length > 0" type="card" :amount="enemy.cardsUnderneath.length" />
            <Token
              v-for="(sealedToken, index) in enemy.sealedChaosTokens"
              :key="index"
              :token="sealedToken"
              :playerId="playerId"
              :game="game"
              @choose="choose"
              class="sealed"
            />
          </div>

          <AbilitiesMenu
            :frame="frame"
            v-model="showAbilities"
            :abilities="abilities"
            :position="atLocation ? 'right' : (inVoid || global) ? 'left' : 'top'"
            @choose="chooseAbility"
            />
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
        :attached="true"
        @choose="$emit('choose', $event)"
      />
      <Event
        v-for="eventId in enemy.events"
        :key="eventId"
        :event="game.events[eventId]"
        :game="game"
        :playerId="playerId"
        :attached="true"
        @choose="$emit('choose', $event)"
      />
      <Skill
        v-for="skillId in enemy.skills"
        :key="skillId"
        :skill="game.skills[skillId]"
        :game="game"
        :playerId="playerId"
        :attached="true"
        @choose="$emit('choose', $event)"
      />
      <template v-if="debug.active">
        <button @click="debugging = true">Debug</button>
      </template>
    </div>

    <div class="swarm" v-if="swarmEnemies.length > 0">
      <Enemy
        v-for="enemy in swarmEnemies"
        :key="enemy.id"
        :enemy="enemy"
        :game="game"
        :playerId="playerId"
        :atLocation="atLocation"
        @choose="$emit('choose', $event)"
        class="enemy--swarming"
      />
    </div>
    <DebugEnemy v-if="debugging" :game="game" :enemy="enemy" :playerId="playerId" @close="debugging = false" />
  </div>
</template>

<style scoped lang="scss">
.small-treachery :deep(.card) {
  height: calc(var(--card-width) * 0.35);
}

:deep(.event) img {
  object-fit: cover;
  object-position: bottom;
  height: calc(var(--card-width) * 0.6);
  margin-top: 2px;
}

.enemy--can-interact {
  border: 2px solid var(--select);
  border-radius: 5px;
  cursor: pointer;
}

.enemy {
  display: flex;
  flex-direction: column;
  position: relative;
  z-index: 5;
  isolation: isolate;
}

.card {
  width: var(--card-width);
  max-width: var(--card-width);
  border-radius: 5px;
}

.pool {
  position: absolute;
  top: 10%;
  align-items: center;
  display: flex;
  flex-wrap: wrap;
  align-self: flex-start;
  align-items: flex-end;
  z-index: 15;
  :deep(img) {
    width: 20px;
    height: auto;
  }

  :deep(.token-container) {
    width: 20px;
  }

  pointer-events: none;
}

.exhausted {
  :deep(img) {
    transition: transform 0.2s linear;
    transform: rotate(90deg) translateX(-10px);
  }
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

  &.right {
    bottom:50%;
    left: 100%;
    transform: translateY(50%) translateZ(0);
    z-index: 20000000000;
    //z-index: 0;
  }

  &.left {
    bottom:0%;
    right: 100%;
    left: unset;
    transform: unset;
  }
}

.swarm {
  display: flex;
  flex-direction: row-reverse;
  width: fit-content;
  justify-content: space-evenly;
  :has(.exhausted) {
    gap: 10px;
    margin-left: 5px;
  }
  &:hover {
    flex-wrap: wrap;

    .enemy--swarming {
      margin-left: 5px;
    }
  }

  &:has(.enemy--swarming.showAbilities) {
    .enemy--swarming {
      margin-left: 5px;
    }
  }

  .enemy--swarming {
    margin-left: calc((var(--card-width) / 1.5) * -1);
  }
}


.enemy--outer {
  isolation: isolate;
  display: flex;
  justify-content: space-evenly;
}

.attached.card {
  object-fit: cover;
  object-position: left bottom;
  height: calc(var(--card-width)*0.6);
}
</style>
