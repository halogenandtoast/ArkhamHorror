<script lang="ts" setup>
import { useI18n } from 'vue-i18n'; 
import { onBeforeUnmount, ComputedRef, ref, computed, watch, nextTick } from 'vue';
import { useDebug } from '@/arkham/debug';
import { Game } from '@/arkham/types/Game';
import { imgsrc } from '@/arkham/helpers';
import { keyToId } from '@/arkham/types/Key'
import * as ArkhamGame from '@/arkham/types/Game';
import DebugLocation from '@/arkham/components/debug/Location.vue';
import { AbilityLabel, AbilityMessage, Message, MessageType } from '@/arkham/types/Message';
import ConcealedCard from '@/arkham/components/ConcealedCard.vue';
import KeyToken from '@/arkham/components/Key.vue';
import Seal from '@/arkham/components/Seal.vue';
import Locus from '@/arkham/components/Locus.vue';
import Enemy from '@/arkham/components/Enemy.vue';
import Investigator from '@/arkham/components/Investigator.vue';
import Asset from '@/arkham/components/Asset.vue';
import Event from '@/arkham/components/Event.vue';
import Story from '@/arkham/components/Story.vue';
import ScarletKey from '@/arkham/components/ScarletKey.vue';
import Treachery from '@/arkham/components/Treachery.vue';
import Token from '@/arkham/components/Token.vue'
import AbilitiesMenu from '@/arkham/components/AbilitiesMenu.vue'
import PoolItem from '@/arkham/components/PoolItem.vue';
import * as Arkham from '@/arkham/types/Location';
import { TokenType } from '@/arkham/types/Token';
import { Card } from '../types/Card';
import useHighlighter from '@/composeable/useHighlighter';

export interface Props {
  game: Game
  location: Arkham.Location
  playerId: string
}

const { t } = useI18n()
const explosionPNG = `url(${imgsrc('explosion.png')})`
const frame = ref(null)
const debugging = ref(false)
const showAbilities = ref<boolean>(false)
const abilitiesEl = ref<HTMLElement | null>(null)
const highlighter = useHighlighter()

const dragover = (e: DragEvent) => {
  e.preventDefault()
  if (e.dataTransfer) {
    e.dataTransfer.dropEffect = 'move'
  }
}

const props = defineProps<Props>()
const emits = defineEmits<{
  choose: [value: number]
  show: [cards: ComputedRef<Card[]>, title: string, isDiscards: boolean]
}>()

const choose = (n: number) => emits('choose', n)

const image = computed(() => {
  const { cardCode, revealed } = props.location
  const suffix = revealed ? '' : 'b'

  return imgsrc(`cards/${cardCode.replace('c', '')}${suffix}.avif`)
})

const id = computed(() => props.location.id)
const choices = computed(() => ArkhamGame.choices(props.game, props.playerId))

const locationStory = computed(() => {
  const { stories } = props.game
  return Object.values(stories).find((s) => s.otherSide?.contents === props.location.id)
})

const locus = computed(() => {
  return modifiers.value?.some((m) => m.type.tag === "UIModifier" && m.type.contents === "Locus") ?? false
})

type Important = string



const important = computed<Important[]>(() => {
  return (modifiers.value ?? []).reduce<Important[]>((acc, m) => {
    if (m.type.tag !== "UIModifier") return acc
    if (typeof m.type.contents === "string") return acc
    if (m.type.contents.tag !== "ImportantToScenario") return acc
    const { contents } = m.type.contents
    const text = contents.startsWith('$') ? t(contents.slice(1)) : contents
    return [...acc, text as Important]
  }, [])
})

function isCardAction(c: Message): boolean {
  if (c.tag === "TargetLabel") return c.target.contents === id.value
  if (c.tag === "GridLabel") return c.gridLabel === props.location.label

  // we also allow the move action to cause card interaction
  if (c.tag == "AbilityLabel" && "contents" in c.ability.source) {
    return c.ability.type.tag === "ActionAbility" && c.ability.type.actions.includes("Move") && c.ability.source.contents === id.value && c.ability.index === 102 && abilities.value.length == 1
  }

  return false
}

const concealed = computed(() => Object.values(props.game.concealed).filter((c) => props.location.concealedCards.includes(c.id)))
const unknownConcealed = computed(() => concealed.value.filter(c => !c.known))
const knownConcealed = computed(() => concealed.value.filter(c => c.known))
const cardAction = computed(() => choices.value.findIndex(isCardAction))
const canInteract = computed(() => abilities.value.length > 0 || cardAction.value !== -1)
let clickTimeout: number | null = null
// clickCount is used to determine if the user clicked once or twice
let clickCount = 0

onBeforeUnmount(() => {
  if (clickTimeout) {
    clearTimeout(clickTimeout)
    clickTimeout = null
  }
  clickCount = 0
})

async function clicked(e: MouseEvent) {
  clickCount++;
  if (clickTimeout) {
    clearTimeout(clickTimeout);
  }  
  clickTimeout = setTimeout(async () => {
    // Ensure this does not conflict with the double-click zoom-in functionality (toggleZoom in Scenario.vue)
    if (clickCount === 1){
      if(cardAction.value !== -1) {
        choose(cardAction.value)
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

    // Reset click count and timeout
    clickCount = 0;
    clickTimeout = null;
  }, 300);
}

async function chooseAbility(ability: number) {
  showAbilities.value = false
  abilitiesEl.value?.blur()
  choose(ability)
}

function isAbility(v: Message): v is AbilityLabel {
  if (v.tag === MessageType.FIGHT_LABEL && v.enemyId === id.value) {
    return true
  }

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
  }

  if (abilities.length === 0) {
    showAbilities.value = false
  }
})

const enemies = computed(() => {
  const enemyIds = props.location.enemies;

  return enemyIds
    .filter((e) => props.game.enemies[e].placement.tag === 'AtLocation' && props.game.enemies[e].placement.contents !== "AttachedToAsset" && props.game.enemies[e].asSelfLocation === null)
})

const attachedEnemies = computed(() => {
  const enemyIds = props.location.enemies;

  return enemyIds
    .filter((e) => props.game.enemies[e].placement.tag === 'AttachedToLocation')
})

const attachedKeys = computed(() => {
  const scarletKeyIds = props.location.scarletKeys;

  return scarletKeyIds
    .filter((e) => props.game.scarletKeys[e].placement.tag === 'AttachedToLocation')
})

const stories = computed(() => {
  return Object.values(props.game.stories)
    .filter((s) => {
      const { assets, enemies } = props.game
      if (Object.values(enemies).find((e) => s.otherSide?.contents === e.id)) {
        return false
      }
      if (Object.values(assets).find((a) => s.otherSide?.contents === a.id)) {
        return false
      }
      return s.placement.tag === 'AtLocation' && s.placement.contents === props.location.id && s.otherSide?.contents !== props.location.id
    })
    .map((s) => s.id)
})

const treacheries = computed(() => {
  const treacheryIds = props.location.treacheries;

  return treacheryIds
    .filter((e) => props.game.treacheries[e].placement.tag === 'AttachedToLocation')
})

const hasAttachments = computed(() => {
  return treacheries.value.length > 0 || props.location.events.length > 0 || attachedEnemies.value.length > 0 || attachedKeys.value.length > 0
})

const encounterCardsUnderneath = computed(() => {
  return props.location.cardsUnderneath.filter(c => c.tag === 'EncounterCard')
})

const playerCardsUnderneath = computed(() => {
  return props.location.cardsUnderneath.filter(c => c.tag === 'PlayerCard')
})

const hasPool = computed(() => {
  return keys.value.length > 0 ||
    seals.value.length > 0 ||
    (doom.value && doom.value > 0) ||
    (horror.value && horror.value > 0) ||
    (damage.value && damage.value > 0) ||
    (resources.value && resources.value > 0) ||
    (pillars.value && pillars.value > 0) ||
    (leylines.value && leylines.value > 0) ||
    (antiquities.value && antiquities.value > 0) ||
    (civilians.value && civilians.value > 0) ||
    (study.value && study.value > 0) ||
    (targets.value && targets.value > 0) ||
    (sealTokens.value && sealTokens.value > 0) ||
    (depth.value && depth.value > 0) ||
    (breaches.value && breaches.value > 0) ||
    (shards.value && shards.value > 0) ||
    (scoutingReports.value && scoutingReports.value > 0) ||
    (scraps.value && scraps.value > 0) ||
    (depletion.value && depletion.value > 0) ||
    (props.location.brazier && props.location.brazier === 'Lit') ||
    (props.location.cardsUnderneath.length > 0) ||
    (props.location.sealedChaosTokens.length > 0)
})

const blocked = computed(() => {
  const inv = Object.values(props.game.investigators).find(i => i.playerId === props.playerId)
  const invMods = inv?.modifiers ?? []
  const locMods = props.location.modifiers

  const isBlocked = (m:any) =>
    (m.type.tag === 'CannotEnter' && m.type.contents === props.location.id) ||
    (m.type.tag === 'OtherModifier' && m.type.contents === 'Blocked')

  return invMods.some(isBlocked) || locMods.some(isBlocked)
})

const modifiers = computed(() => props.location.modifiers)

const explosion = computed(() => {
  return modifiers.value?.some((m) => m.type.tag === "UIModifier" && m.type.contents === "Explosion") ?? false
})


const keys = computed(() => props.location.keys)
const seals = computed(() => props.location.seals)

const sealTokens = computed(() => props.location.tokens[TokenType.Seal])
const clues = computed(() => props.location.tokens[TokenType.Clue])
const doom = computed(() => props.location.tokens[TokenType.Doom])
const resources = computed(() => props.location.tokens[TokenType.Resource])
const pillars = computed(() => props.location.tokens[TokenType.Pillar])
const depth = computed(() => props.location.tokens[TokenType.Depth])
const leylines = computed(() => props.location.tokens[TokenType.Leyline])
const shards = computed(() => props.location.tokens[TokenType.Shard])
const scoutingReports = computed(() => props.location.tokens[TokenType.ScoutingReport])
const scraps = computed(() => props.location.tokens[TokenType.Scrap])
const depletion = computed(() => props.location.tokens[TokenType.Depletion])
const antiquities = computed(() => props.location.tokens[TokenType.Antiquity])
const civilians = computed(() => props.location.tokens[TokenType.Civilian])
const study = computed(() => props.location.tokens[TokenType.Study])
const targets = computed(() => props.location.tokens[TokenType.Target])
const breaches = computed(() => {
  const {breaches} = props.location
  if (breaches) {
    return breaches.contents
  }

  return 0
})
const investigators = computed(() => {
  return props.location.investigators.
    map((i) => props.game.investigators[i]).
    filter(i => i.placement.tag === 'AtLocation')
})
const horror = computed(() => props.location.tokens[TokenType.Horror])
const damage = computed(() => props.location.tokens[TokenType.Damage])
const floodLevel = computed(() => {
  if (!props.location.floodLevel) return
  switch (props.location.floodLevel) {
    case "Unflooded": return null
    case "PartiallyFlooded": return imgsrc('partially-flooded.png')
    case "FullyFlooded": return imgsrc('fully-flooded.png')
    default: return null
  }
})

const debug = useDebug()

function onDrop(event: DragEvent) {
  event.preventDefault()
  if (event.dataTransfer) {
    const data = event.dataTransfer.getData('text/plain')
    if (data) {
      const json = JSON.parse(data)
      if (json.tag === "EnemyTarget") {
        if (enemies.value.some(e => e === json.contents)) return false
        debug.send(props.game.id, {tag: 'EnemyMove', contents: [json.contents, id.value]})
      }

      if (json.tag === "AssetTarget") {
        //if (assets.value.some(e => e === json.contents)) return false
        debug.send(props.game.id, {tag: 'PlaceAsset', contents: [json.contents, { tag: "AtLocation", contents: id.value}]})
      }

      if (json.tag === "InvestigatorTarget") {
        if (enemies.value.some(e => e === json.contents)) return false
        debug.send(props.game.id, {tag: 'Move', contents: {moveSource: {tag:"GameSource"}, moveTarget: json, moveDestination: {tag:"ToLocation", contents: id.value}, moveMeans: "Direct", moveCancelable: false, movePayAdditionalCosts: false, moveAfter: []}})
      }
    }
  }
}

const showCardsUnderneath = () => emits('show', playerCardsUnderneath, "Cards Underneath", false)
const highlighted = computed(() => highlighter.highlighted.value === props.location.id)
</script>

<template>
  <div>
    <div class="location-container">
      <div class="location-investigator-column">
        <div
          v-for="investigator in investigators"
          :key="investigator.cardCode"
        >
          <Investigator
            :game="game"
            :choices="choices"
            :playerId="playerId"
            :portrait="true"
            :investigator="investigator"
            @choose="choose"
            />
        </div>
      </div>
      <div class="location-column">
        <div class="card-frame" :class="{ explosion }" ref="frame" @click="clicked">
          <Locus v-if="locus" class="locus" />
          <font-awesome-icon v-if="blocked" :icon="['fab', 'expeditedssl']" class="status-icon" />
            <span v-for="ui in important" class="important" :class="{ 'important--can-interact': canInteract }" v-tooltip="ui">
            <font-awesome-icon :icon="['fa', 'circle-exclamation']" />
          </span>

          <div class="card-frame-inner" :class="{ highlighted }">
            <Story v-if="locationStory" :story="locationStory" :game="game" :playerId="playerId" @choose="choose"/>
            <template v-else>
              <div class="wave" v-if="location.floodLevel" :class="{ [location.floodLevel]: true }"></div>
              <img
                :data-id="id"
                class="card card--locations"
                :src="image"
                :class="{ 'location--can-interact': canInteract }"
                draggable="false"
                @drop="onDrop"
                @dragover.prevent="dragover"
                @dragenter.prevent
              />
            </template>
          </div>

          <div class="clues pool location-pool" v-if="(clues ?? 0) > 0 || floodLevel">
            <PoolItem v-if="clues && clues > 0" type="clue" :amount="clues" />
            <img v-if="floodLevel" :src="floodLevel" class="flood-level" />
          </div>

          <div class="pool location-pool" v-if="hasPool">
            <KeyToken v-for="k in keys" :key="keyToId(k)" :keyToken="k" :game="game" :playerId="playerId" @choose="choose" />
            <Seal v-for="seal in seals" :key="seal.sealKind" :seal="seal" />
            <PoolItem v-if="doom && doom > 0" type="doom" :amount="doom" />
            <PoolItem v-if="horror && horror > 0" type="horror" :amount="horror" />
            <PoolItem v-if="damage && damage > 0" type="health" :amount="damage" />
            <PoolItem v-if="resources && resources > 0" type="resource" :amount="resources" />
            <PoolItem v-if="pillars && pillars > 0" type="resource" :amount="pillars" />
            <PoolItem v-if="leylines && leylines > 0" type="resource" tooltip="Leyline" :amount="leylines" />
            <PoolItem v-if="shards && shards > 0" type="resource" tooltip="Shard" :amount="shards" />
            <PoolItem v-if="scoutingReports && scoutingReports > 0" type="resource" tooltip="Scouting Report" :amount="scoutingReports" />
            <PoolItem v-if="scraps && scraps > 0" type="resource" tooltip="Scrap" :amount="scraps" />
            <PoolItem v-if="depletion && depletion > 0" type="resource" tooltip="Scouting Report" :amount="depletion" />
            <PoolItem v-if="antiquities && antiquities > 0" type="resource" tooltip="Antiquity" :amount="antiquities" />
            <PoolItem v-if="civilians && civilians > 0" type="resource" tooltip="Civilian" :amount="civilians" />
            <PoolItem v-if="study && study > 0" type="resource" tooltip="Civilian" :amount="study" />
            <PoolItem v-if="targets && targets > 0" type="resource" tooltip="Target" :amount="targets" />
            <PoolItem v-if="sealTokens && sealTokens > 0" type="resource" tooltip="Seal" :amount="sealTokens" />

            <PoolItem v-if="depth && depth > 0" type="resource" :amount="depth" />
            <PoolItem v-if="breaches > 0" type="resource" :amount="breaches" />
            <PoolItem v-if="location.brazier && location.brazier === 'Lit'" type="resource" :amount="1" />
            <PoolItem v-if="encounterCardsUnderneath.length > 0" type="card" :amount="encounterCardsUnderneath.length" />
            <PoolItem v-if="playerCardsUnderneath.length > 0" type="player_card" :amount="playerCardsUnderneath.length" />

            <Token
              v-for="(sealedToken, index) in location.sealedChaosTokens"
              :key="index"
              :token="sealedToken"
              :playerId="playerId"
              :game="game"
              @choose="choose"
              class="sealed"
            />
          </div>
        </div>

        <AbilitiesMenu
          v-model="showAbilities"
          :abilities="abilities"
          :frame="frame"
          :show-move="abilities.length > 1"
          :game="game"
          position="left"
          @choose="chooseAbility"
        />


        <button v-if="playerCardsUnderneath.length > 0" @click="showCardsUnderneath">Under ({{ playerCardsUnderneath.length }})</button>

        <template v-if="debug.active">
          <button @click="debugging = true">Debug</button>
        </template>
      </div>
      <div class="attachments" v-if="hasAttachments">
        <Treachery
          v-for="treacheryId in treacheries"
          :key="treacheryId"
          :treachery="game.treacheries[treacheryId]"
          :game="game"
          :attached="true"
          :playerId="playerId"
          @choose="choose"
        />
        <Event
          v-for="eventId in location.events"
          :event="game.events[eventId]"
          :game="game"
          :playerId="playerId"
          :key="eventId"
          @choose="choose"
          :attached="true"
        />
        <Enemy
          v-for="enemyId in attachedEnemies"
          :enemy="game.enemies[enemyId]"
          :game="game"
          :playerId="playerId"
          :key="enemyId"
          @choose="choose"
          :attached="true"
        />
        <ScarletKey
          v-for="skId in attachedKeys"
          :scarletKey="game.scarletKeys[skId]"
          :game="game"
          :playerId="playerId"
          :key="skId"
          @choose="choose"
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
          :atLocation="true"
          @choose="choose"
        />
        <Enemy
          v-for="enemyId in enemies"
          :key="enemyId"
          :enemy="game.enemies[enemyId]"
          :game="game"
          :playerId="playerId"
          :atLocation="true"
          @choose="choose"
        />
        <Story
          v-for="storyId in stories"
          :key="storyId"
          :story="game.stories[storyId]"
          :game="game"
          :playerId="playerId"
          :atLocation="true"
          @choose="choose"
        />
        <div v-if="unknownConcealed.length > 0" class='concealed-card-stack'>
          <ConcealedCard :card="unknownConcealed[0]" :game="game" :playerId="playerId" @choose="choose" />
          <span class='count'>{{unknownConcealed.length}}</span>
        </div>
        <ConcealedCard v-for="card in knownConcealed" :key="card.id" :card="card" :game="game" :playerId="playerId" @choose="choose" />
      </div>
    </div>
    <DebugLocation v-if="debugging" :game="game" :location="location" :playerId="playerId" @close="debugging = false" />
  </div>
</template>

<style scoped>
.location--can-interact {
  border: 2px solid var(--select);
  cursor: pointer;
}

.card {
  width: calc(var(--card-width) + 4px);
  min-width: calc(var(--card-width) + 4px);
  border-radius: 3px;
}

.card.card--locations {
  width: min(calc(10vw + 20px), 60px);
}

.location-column :deep(.enemy) {
  width: calc(var(--card-width) * 0.8);

}

.location-column :deep(.treachery) {
  object-fit: cover;
  object-position: 0 -74px;
  height: calc(var(--card-width) * 0.35);
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
  grid-area: location;
  width: min(calc(10vw + 20px), 60px);
}

.location-pool {
  display: flex;
  flex-direction: row;
  justify-self: flex-start;
  height: 2em;
  &:not(:has(> .key--can-interact)) {
    pointer-events: none;
  }
  & :deep(.poolItem) {
    pointer-events: none;
  }

  :deep(img) {
    width: 30px;
    height: auto;
  }

  :deep(.token-container) {
    width: 20px;
  }
}

.status-icon {
  position: absolute;
  top: 10%;
  background: rgba(255, 255, 255, 0.7);
  border-radius: 1.5em;
  font-size: 2.6em;
  color: rgba(0, 0, 0, 0.8);
  pointer-events: none;
  z-index: 1;
  min-height: min-content;
  scale: 0.8;
}

.important {
  position: absolute;
  bottom: 10%;
  border-radius: 1000px;
  font-size: 2.6em;
  color: var(--important);
  /*pointer-events: none;*/
  z-index: 1;
  max-width: 40%;
  max-height: min-content;
  aspect-ratio: 1 / 1;
  display: flex;
  align-items: center;
  justify-content: center;
  filter: drop-shadow(0px 0px 1px #000) drop-shadow(0px 0px 2px #000);
}

.important--can-interact {
  cursor: pointer;
}

.card-container {
  border-radius: 5px;
}


.location-investigator-column {
  grid-area: investigators;
  justify-self: end;

  &:deep(.portrait) {
    height: 25%;
    box-shadow: 1px 1px 6px rgba(0, 0, 0, 0.45);
  }

  &:deep(img) {
    max-width: unset;
  }

  div {
    margin-top: -100%;
  }

  div:first-child {
    margin-top: 0;
  }
}

.location-asset-column {
  grid-area: assetsAndEnemies;
  justify-self: start;
  display: flex;
  flex-direction: column-reverse;
  min-width: calc(var(--card-width) * 0.8);
  height: fit-content;
  &:deep(.card) {
    width: calc(var(--card-width) * 0.8) !important;
  }
  &:deep(.pool) {
    height: fit-content;
    top:1em;
    font-size: .5em;
    flex-direction: row;
    align-items: center;
    justify-content: center;
  }
  &:deep(.poolItem) {
    width: calc(var(--card-width) * 0.4) !important;
  }
  &:hover {
    animation-fill-mode:forwards;
    > div:not(:last-child) {
      margin-top: 10px;
    }
  }

  animation-fill-mode:fowards;

  div {
    transition: all 0.2s;
  }

  > div:not(:last-child) {
    margin-top: -40px;
  }
}

.pool.location-pool {
  position: absolute;
  top: 50%;
  align-items: center;
  display: flex;
  align-self: flex-start;
  align-items: flex-end;
  gap: 2px;
  &:not(:has(.keys .key--can-interact)) {
    pointer-events: none;
  }
  &.clues {
    top: 10%;
    @media (max-width: 800px) and (orientation: portrait) {
      top: 35% !important;
    }
  }
  @media (max-width: 800px) and (orientation: portrait) {
    &:deep(.poolItem) {
      width: calc(var(--card-width) * 0.6) !important;
    }
    top: -20% !important;
    left: 80%;
    width: fit-content;
    height: fit-content;
    :deep(span) {
      width: fit-content !important;
    }
  }
}

.card-frame {
  position: relative;
  display: flex;
  align-items: center;
  justify-content: center;
  border-radius: 5px;
  min-width: fit-content;

  .card-frame-inner {
    overflow: hidden;
    position: relative;
    transition: transform 0.2s;
    transform: scale(1);
    line-height: 0;
    box-sizing: border-box;
    box-shadow: var(--card-shadow);
    &:deep(.card) {
      width: calc(var(--card-width) + 4px);
      min-width: calc(var(--card-width) + 4px);
      border-radius: 3px;
      border-width: 1px;
    }
    &.highlighted {
      transform: scale(1.1);
    }
    --gradient-glow: #BDE038, rebeccapurple, rebeccapurple, #BDE038;
  }
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
  background-image: v-bind(explosionPNG);
  background-repeat: no-repeat;
  background-position: 0 0;
  background-size: 3072px;
  pointer-events: none;
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
  grid-area: attachments;

  &:deep(.exhausted) {
    padding: 0;
  }

}

.location:has(.abilities) {
  z-index: 30 !important;
}

.locus {
  width: calc(var(--card-width) - 20px);
  height: calc(var(--card-width) - 20px);
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

.location {
  min-width: calc(var(--card-width) + 120px);
}

.location-container {
  min-height: calc(var(--card-width) / var(--card-aspect) + 40px);
  display: grid;
  grid-template-areas:
    "investigators location    assetsAndEnemies"
    "investigators attachments assetsAndEnemies";
  grid-template-columns: 60px 1fr 60px;
  grid-column-gap: 10px;
  @media (max-width: 800px) and (orientation: portrait) {
    grid-column-gap: .5px;
  }
}

.flood-level {
  width: min(20px, 4vw);
}

.wave {
  animation: wave 30s linear infinite;
  background-color: #3f68c5;
  border-radius: 38%;
  height: 200%;
  left: -50%;
  opacity: 0.4;
  pointer-events: none;
  position: absolute;
  top: 120%;
  transition: top 10s linear, height 10s linear, border-radius 10s linear;
  width: 200%;
}

.Unflooded {
  display: none;
}

.FullyFlooded {
  top: -40%;
  height: 200%;
  border-radius: 38%;
  animation-fill-mode: forwards;

  @starting-style {
    top: 120%;
    height: 200%;
    border-radius: 38%;
  }
}

.PartiallyFlooded {
  top: 50%;
  height: 150%;
  border-radius: 28%;

  @starting-style {
    top: 120%;
    height: 200%;
    border-radius: 15px;
  }
}

@keyframes wave {
  from { transform: rotate(0deg)}
  to { transform: rotate(360deg)}
}

:deep(.token) {
  width: 30px;
}

.concealed-card {
  width: calc(var(--card-width) * 0.55);
  border-radius: 3px;
}

.concealed-card-stack {
  position: relative;
  display: grid;
  grid-template-areas: "stack";
  align-items: center;
  justify-items: center;
  > * {
    grid-area: stack;
    justify-self: center;
  }
  .count {
    align-self: start;
    margin-top: 5%;
    font-weight: bold;
    border-radius: 100vw;
    background-color: rgba(255, 255, 255, 0.6);
    width: auto;
    height: 1.2em;
    display: grid;
    aspect-ratio: 1 / 1;
    text-align: center;
    align-content: center;
    justify-content: center;
    pointer-events: none;
  }
}
</style>
