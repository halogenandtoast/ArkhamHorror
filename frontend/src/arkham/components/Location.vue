<script lang="ts" setup>
import { ref, computed } from 'vue';
import { useDebug } from '@/arkham/debug';
import { Game } from '@/arkham/types/Game';
import { imgsrc } from '@/arkham/helpers';
import * as ArkhamGame from '@/arkham/types/Game';
import { AbilityLabel, AbilityMessage, Message } from '@/arkham/types/Message';
import Key from '@/arkham/components/Key.vue';
import Enemy from '@/arkham/components/Enemy.vue';
import Investigator from '@/arkham/components/Investigator.vue';
import Asset from '@/arkham/components/Asset.vue';
import Event from '@/arkham/components/Event.vue';
import Treachery from '@/arkham/components/Treachery.vue';
import AbilityButton from '@/arkham/components/AbilityButton.vue';
import PoolItem from '@/arkham/components/PoolItem.vue';
import * as Arkham from '@/arkham/types/Location';
import { TokenType } from '@/arkham/types/Token';

export interface Props {
  game: Game
  location: Arkham.Location
  investigatorId: string
}

const showAbilities = ref<boolean>(false)

const props = defineProps<Props>()
const emits = defineEmits(['choose'])

const image = computed(() => {
  const { cardCode, revealed } = props.location
  const suffix = revealed ? '' : 'b'

  return imgsrc(`cards/${cardCode.replace('c', '')}${suffix}.jpg`)
})

const id = computed(() => props.location.id)
const choices = computed(() => ArkhamGame.choices(props.game, props.investigatorId))

function isCardAction(c: Message): boolean {
  if (c.tag === "TargetLabel") {
     return c.target.contents === id.value
  }

  // we also allow the move action to cause card interaction
  if (c.tag == "AbilityLabel" && "contents" in c.ability.source) {
    return c.ability.type.tag === "ActionAbility" && c.ability.type.action === "Move" && c.ability.source.contents === id.value
  }

  return false
}

const cardAction = computed(() => choices.value.findIndex(isCardAction))
const canInteract = computed(() => abilities.value.length > 0 || cardAction.value !== -1)

async function clicked() {
  if(cardAction.value !== -1) {
    emits('choose', cardAction.value)
  } else {
    showAbilities.value = !showAbilities.value
  }
}

async function chooseAbility(ability: number) {
  showAbilities.value = false
  emits('choose', ability)
}

function isAbility(v: Message): v is AbilityLabel {
  if (v.tag !== 'AbilityLabel') {
    return false
  }

  const { source } = v.ability;

  if (source.sourceTag === 'ProxySource') {
    if ("contents" in source.source) {
      return source.source.contents === id.value
    }
  } else if (source.tag === 'LocationSource') {
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

const enemies = computed(() => {
  const enemyIds = props.location.enemies;
  return enemyIds
    .filter((e) => props.game.enemies[e].engagedInvestigators.length === 0);
})

const blocked = computed(() => {
  const { modifiers } = props.game.investigators[props.investigatorId]

  if (modifiers) {
    modifiers.some(modifier =>
      (modifier.type.tag == "CannotEnter" && modifier.type.contents == props.location.id) ||
        (modifier.type.tag === "OtherModifier" && modifier.type.contents === "Blocked")
    )
  }

  return false
})


const keys = computed(() => props.location.keys)

const clues = computed(() => props.location.tokens[TokenType.Clue])
const doom = computed(() => props.location.tokens[TokenType.Doom])
const resources = computed(() => props.location.tokens[TokenType.Resource])
const horror = computed(() => props.location.tokens[TokenType.Horror])

const debug = useDebug()
</script>

<template>
  <div class="location-container">
    <div class="location-investigator-column">
      <div
        v-for="cardCode in location.investigators"
        :key="cardCode"
      >
        <Investigator
          :game="game"
          :choices="choices"
          :investigatorId="investigatorId"
          :portrait="true"
          :player="game.investigators[cardCode]"
          @choose="$emit('choose', $event)"
          />
      </div>
    </div>
    <div class="location-column">
      <div class="card-frame">
        <font-awesome-icon v-if="blocked" :icon="['fab', 'expeditedssl']" class="status-icon" />

        <img
          :data-id="id"
          class="card"
          :src="image"
          :class="{ 'location--can-interact': canInteract }"
          @click="clicked"
        />

        <div class="pool">
          <Key v-for="key in keys" :key="key" :name="key" />
          <PoolItem v-if="clues && clues > 0" type="clue" :amount="clues" />
          <PoolItem v-if="doom && doom > 0" type="doom" :amount="doom" />
          <PoolItem v-if="horror && horror > 0" type="horror" :amount="horror" />
          <PoolItem v-if="resources && resources > 0" type="resource" :amount="resources" />
          <PoolItem v-if="location.cardsUnderneath.length > 0" type="card" :amount="location.cardsUnderneath.length" />
        </div>
      </div>
      <div v-if="showAbilities" class="abilities">
        <AbilityButton
          v-for="ability in abilities"
          :key="ability.index"
          :ability="ability.contents"
          :data-image="image"
          @click="chooseAbility(ability.index)"
          />
        <template v-if="debug.active">
          <button v-if="!location.revealed" @click="debug.send(game.id, {tag: 'RevealLocation', contents: [null, id]})">Reveal</button>
        </template>
      </div>
      <Treachery
        v-for="treacheryId in location.treacheries"
        :key="treacheryId"
        :treachery="game.treacheries[treacheryId]"
        :game="game"
        :attached="true"
        :investigatorId="investigatorId"
        @choose="$emit('choose', $event)"
      />
      <Event
        v-for="eventId in location.events"
        :event="game.events[eventId]"
        :game="game"
        :investigatorId="investigatorId"
        :key="eventId"
        @choose="$emit('choose', $event)"
      />
    </div>
    <div class="location-asset-column">
      <Asset
        v-for="assetId in location.assets"
        :asset="game.assets[assetId]"
        :game="game"
        :investigatorId="investigatorId"
        :key="assetId"
        @choose="$emit('choose', $event)"
      />
      <Enemy
        v-for="enemyId in enemies"
        :key="enemyId"
        :enemy="game.enemies[enemyId]"
        :game="game"
        :investigatorId="investigatorId"
        :atLocation="true"
        @choose="$emit('choose', $event)"
      />
    </div>
  </div>
</template>

<style scoped lang="scss">
.location--can-interact {
  border: 2px solid $select;
  cursor: pointer;
}

.card {
  width: calc($card-width + 4px);
  border-radius: 3px;
  box-sizing: border-box;
}

.location-column :deep(.enemy) {
  width: $card-width * 0.8;

}

.location-column :deep(.treachery) {
  object-fit: cover;
  object-position: 0 -74px;
  height: $card-width * 0.35;
  margin-top: 2px;
}

.location-column :deep(.event) {
  object-fit: cover;
  object-position: 0 -74px;
  height: 68px;
  margin-top: 2px;
}

.location-container {
  display: flex;
  margin: 0 5px;
  min-width: 187px;
  position: relative;
}

.button{
  margin-top: 2px;
  border: 0;
  color: #fff;
  border-radius: 4px;
  padding: 5px 10px;
}

.location-column {
  display: flex;
  flex-direction: column;
  position: relative;
}

.pool {
  display: flex;
  flex-direction: column;
  justify-self: flex-start;
  height: 2em;
}

.status-icon {
  position: absolute;
  top: 10%;
  background: rgba(255, 255, 255, 0.7);
  border-radius: 1.5em;
  font-size: 2.6em;
  color: rgba(0, 0, 0, 0.8);
  pointer-events: none;
}

.card-container {
  border-radius: 5px;
}

.location-investigator-column {
  position: absolute;
  right: 100%;
  &:deep(.portrait) {
    height: 25%;
  }
}

.location-asset-column {
  min-width: $card-width * 0.6;
  height: 100%;
  &:deep(.card) {
    width: $card-width * 0.6 !important;
  }
}

.pool {
  position: absolute;
  top: 50%;
  align-items: center;
  display: flex;
  align-self: flex-start;
  align-items: flex-end;
  * {
    transform: scale(0.6);
  }
}

.card-frame {
  position: relative;
  display: flex;
  align-items: center;
  justify-content: center;
}

.abilities {
  position: absolute;
  bottom:100%;
  padding: 10px;
  background: rgba(0, 0, 0, 0.2);
  border-radius: 10px;
}
</style>
