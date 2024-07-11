<script lang="ts" setup>
import { ref, computed, watch, nextTick } from 'vue';
import { useDebug } from '@/arkham/debug';
import { Game } from '@/arkham/types/Game';
import { imgsrc } from '@/arkham/helpers';
import * as ArkhamGame from '@/arkham/types/Game';
import { AbilityLabel, AbilityMessage, Message } from '@/arkham/types/Message';
import Key from '@/arkham/components/Key.vue';
import Locus from '@/arkham/components/Locus.vue';
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
  playerId: string
}

const showAbilities = ref<boolean>(false)
const abilitiesEl = ref<HTMLElement | null>(null)

const handleFocus = () => {
  showAbilities.value = true
}

const handleFocusOut = (e: FocusEvent) => {
  const target = e.target as HTMLElement
  if (target && target.classList.contains('abilities')) {
    return
  }
  showAbilities.value = false
}

const props = defineProps<Props>()
const emits = defineEmits(['choose'])

const image = computed(() => {
  const { cardCode, revealed } = props.location
  const suffix = revealed ? '' : 'b'

  return imgsrc(`cards/${cardCode.replace('c', '')}${suffix}.jpg`)
})

const id = computed(() => props.location.id)
const choices = computed(() => ArkhamGame.choices(props.game, props.playerId))

const locus = computed(() => {
  return modifiers.value?.some((m) => m.type.tag === "OtherModifier" && m.type.contents === "Locus") ?? false
})

function isCardAction(c: Message): boolean {
  if (c.tag === "TargetLabel") {
     return c.target.contents === id.value
  }

  if (c.tag === "GridLabel") {
     return c.gridLabel === props.location.label
  }

  // we also allow the move action to cause card interaction
  if (c.tag == "AbilityLabel" && "contents" in c.ability.source) {
    return c.ability.type.tag === "ActionAbility" && c.ability.type.actions.includes("Move") && c.ability.source.contents === id.value
  }

  return false
}

const cardAction = computed(() => choices.value.findIndex(isCardAction))
const canInteract = computed(() => abilities.value.length > 0 || cardAction.value !== -1)

async function clicked() {
  if(cardAction.value !== -1) {
    emits('choose', cardAction.value)
  } else if (abilities.value.length > 0) {
    showAbilities.value = !showAbilities.value
    await nextTick()
    if (showAbilities.value === true) {
      abilitiesEl.value?.focus()
    } else {
      abilitiesEl.value?.blur()
    }
  }
}

async function chooseAbility(ability: number) {
  showAbilities.value = false
  abilitiesEl.value?.blur()
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
         return [...acc, { contents: v, displayAsAction: false, index: i}];
       }

       return acc;
     }, []);
})

watch(abilities, (abilities) => {
  // ability is forced we must show
  if (abilities.some(a => "ability" in a.contents && a.contents.ability.type.tag === "ForcedAbility")) {
    showAbilities.value = true
    abilitiesEl.value?.focus()
  }

  if (abilities.length === 0) {
    showAbilities.value = false
    abilitiesEl.value?.blur()
  }
})

const enemies = computed(() => {
  const enemyIds = props.location.enemies;

  return enemyIds
    .filter((e) => props.game.enemies[e].placement.tag === 'OtherPlacement' && props.game.enemies[e].asSelfLocation === null)
})

const blocked = computed(() => {
  const investigator = Object.values(props.game.investigators).find(i => i.playerId === props.playerId)
  const { modifiers } = investigator ?? { modifiers: [] }
  const allModifiers = [...modifiers || [], ...props.location.modifiers]

  if (allModifiers) {
    return allModifiers.some(modifier =>
      (modifier.type.tag === "CannotEnter" && modifier.type.contents === props.location.id) ||
        (modifier.type.tag === "OtherModifier" && modifier.type.contents === "Blocked")
    )
  }

  return false
})

const modifiers = computed(() => props.location.modifiers)

const explosion = computed(() => {
  return modifiers.value?.some((m) => m.type.tag === "OtherModifier" && m.type.contents === "Explosion") ?? false
})


const keys = computed(() => props.location.keys)

const clues = computed(() => props.location.tokens[TokenType.Clue])
const doom = computed(() => props.location.tokens[TokenType.Doom])
const resources = computed(() => props.location.tokens[TokenType.Resource])
const depth = computed(() => props.location.tokens[TokenType.Depth])
const leylines = computed(() => props.location.tokens[TokenType.Leyline])
const breaches = computed(() => {
  const {breaches} = props.location
  if (breaches) {
    return breaches.contents
  }

  return 0
})
const horror = computed(() => props.location.tokens[TokenType.Horror])
const damage = computed(() => props.location.tokens[TokenType.Damage])

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
          :playerId="playerId"
          :portrait="true"
          :investigator="game.investigators[cardCode]"
          @choose="$emit('choose', $event)"
          />
      </div>
    </div>
    <div class="location-column">
      <div class="card-frame" :class="{ explosion }">
        <Locus v-if="locus" class="locus" />
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
          <PoolItem v-if="damage && damage > 0" type="health" :amount="damage" />
          <PoolItem v-if="resources && resources > 0" type="resource" :amount="resources" />
          <PoolItem v-if="leylines && leylines > 0" type="resource" tooltip="Leyline" :amount="leylines" />
          <PoolItem v-if="depth && depth > 0" type="resource" :amount="depth" />
          <PoolItem v-if="breaches > 0" type="resource" :amount="breaches" />
          <PoolItem v-if="location.brazier && location.brazier === 'Lit'" type="resource" :amount="1" />
          <PoolItem v-if="location.cardsUnderneath.length > 0" type="card" :amount="location.cardsUnderneath.length" />
        </div>
      </div>
      <div v-if="showAbilities" class="abilities" :data-image="image" tabindex="-1" @focus="handleFocus" @focusout="handleFocusOut" ref="abilitiesEl">
        <AbilityButton
          v-for="ability in abilities"
          :key="ability.index"
          :ability="ability.contents"
          :show-move="false"
          @click="chooseAbility(ability.index)"
          />
      </div>

      <template v-if="debug.active">
        <button v-if="!location.revealed" @click="debug.send(game.id, {tag: 'RevealLocation', contents: [null, id]})">Reveal</button>
        <button v-if="clues && clues > 0" @click="debug.send(game.id, {tag: 'RemoveTokens', contents: [{ tag: 'TestSource', contents: []}, { tag: 'LocationTarget', contents: id }, 'Clue', clues]})">Remove Clues</button>
        <button @click="debug.send(game.id, {tag: 'PlaceTokens', contents: [{ tag: 'TestSource', contents: []}, { tag: 'LocationTarget', contents: id }, 'Clue', 1]})">Place Clue</button>
      </template>
    </div>
    <div class="attachments">
      <Treachery
        v-for="treacheryId in location.treacheries"
        :key="treacheryId"
        :treachery="game.treacheries[treacheryId]"
        :game="game"
        :attached="true"
        :playerId="playerId"
        @choose="$emit('choose', $event)"
      />
      <Event
        v-for="eventId in location.events"
        :event="game.events[eventId]"
        :game="game"
        :playerId="playerId"
        :key="eventId"
        @choose="$emit('choose', $event)"
        :attached="true"
      />
    </div>
    <div class="location-asset-column">
      <Asset
        v-for="assetId in location.assets"
        :asset="game.assets[assetId]"
        :game="game"
        :playerId="playerId"
        :key="assetId"
        @choose="$emit('choose', $event)"
      />
      <Enemy
        v-for="enemyId in enemies"
        :key="enemyId"
        :enemy="game.enemies[enemyId]"
        :game="game"
        :playerId="playerId"
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
  margin: 60px;
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
  flex-direction: row;
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
  display: flex;
  flex-direction: column-reverse;
  position: absolute;
  left: 100%;
  padding: 0 10px;
  min-width: $card-width * 0.8;
  height: fit-content;
  &:deep(.card) {
    width: $card-width * 0.8 !important;
  }

  &:hover {
    animation-fill-mode:forwards;
    div:not(:last-child) {
      margin-top: 10px;
    }
  }

  animation-fill-mode:fowards;

  div {
    transition: all 0.2s;
  }

  div:not(:last-child) {
    margin-top: -40px;
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

  pointer-events: none;
}

.card-frame {
  position: relative;
  display: flex;
  align-items: center;
  justify-content: center;
}

@keyframes explosion {
  from {
    background-position-x: 0px;
  }
  to {
    background-position-x: -3072px;
  }
}

.explosion::before {
  animation: explosion 0.5s steps(48, end) forwards;
  z-index: 100000000000000000000;
  content: ' ';
  position: absolute;
  top: 0;
  left: 0;
  width: 64px;
  height: 62px;
  background: black;
  background: url("./img/arkham/explosion.png") no-repeat;
  background-position: 0 0;
  background-size: 3072px;
}

.abilities {
  position: absolute;
  padding: 10px;
  background: rgba(0, 0, 0, 0.6);
  border-radius: 10px;
  display: flex;
  flex-direction: column;
  gap: 5px;
  right:100%;
  top: 0;
  outline: 0;
  z-index: 10;
}

.attachments {
  position: absolute;
  top: 100%;
}

.location:has(.abilities) {
  z-index: 30 !important;
}

.locus {
  width: calc($card-width - 20px);
  height: calc($card-width - 20px);
  position: absolute;
  pointer-events: none;
  top: 5px;
  margin-left: auto;
  margin-right: auto;
  left: 0;
  right: 0;
  text-align: center;
  z-index:10000000;

  :deep(path) {
    stroke-dasharray: var(--line-length);
    stroke-dashoffset: var(--line-length);
    transition: stroke-dashoffset 1.5s linear;
    animation: draw-locus 1.5s linear forwards;
  }


  animation: locus 3s linear forwards;
}

@keyframes draw-locus {
  0% {
    opacity: 60;
    stroke-dashoffset: var(--line-length);
  }

  80% {
    opacity: 100;
  }

  100% {
    opacity: 100;
    stroke-dashoffset: 0;
  }
}

@keyframes locus {
  0% {
    filter: drop-shadow(0px 0px 0px #fff)
    drop-shadow(0px 0px 0px #fff)
    drop-shadow(0px 0px 0px #ff80b3)
    drop-shadow(0px 0px 0px #ff4d94)
    drop-shadow(0px 0px 0px #ff0066);
  }
  25% {
    filter: drop-shadow(0px 0px 0px #fff)
    drop-shadow(0px 0px 0px #fff)
    drop-shadow(0px 0px 0px #ff80b3)
    drop-shadow(0px 0px 0px #ff4d94)
    drop-shadow(0px 0px 0px #ff0066);
  }
  100% {
    filter: drop-shadow(0px 0px 1px #fff)
    drop-shadow(0px 0px 1px #fff)
    drop-shadow(0px 0px 3px #ff80b3)
    drop-shadow(0px 0px 10px #ff4d94)
    drop-shadow(0px 0px 15px #ff0066);
  }
}

</style>
