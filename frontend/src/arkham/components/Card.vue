<script lang="ts" setup>
import { computed, onMounted } from 'vue';
import { imgsrc } from '@/arkham/helpers';
import { cardImage } from '@/arkham/cardImages';
import type { Modifier } from '@/arkham/types/Modifier';
import { type Tokens } from '@/arkham/types/Token';
import { cardFacedown, type Card, type CardContents } from '@/arkham/types/Card';
import type { Game } from '@/arkham/types/Game';
import * as ArkhamGame from '@/arkham/types/Game';
import type { AbilityLabel, AbilityMessage, Message } from '@/arkham/types/Message';
import { MessageType } from '@/arkham/types/Message';
import AbilityButton from '@/arkham/components/AbilityButton.vue'
import TokenPool from '@/arkham/components/TokenPool.vue'
import { useDebug } from '@/arkham/debug'
import * as DebugMove from '@/arkham/debugCardMove'
import { useCardStore } from '@/stores/cards'

const props = withDefaults(defineProps<{
  game: Game
  card: Card | CardContents
  revealed?: boolean
  playerId: string
  allowAbilityButtons?: boolean
  allowInteractions?: boolean
  // An in-play asset/treachery's abilities normally belong to Asset.vue /
  // Treachery.vue, so this card deliberately ignores them. The Hidden stack is
  // the exception: it tucks cards that are still in play out of the play area,
  // so those components never render and this card is the only anchor the
  // ability has. Without it a forced trigger on a tucked card is unreachable.
  allowInPlayAbilities?: boolean
}>(), { revealed: false, allowAbilityButtons: true, allowInteractions: true, allowInPlayAbilities: false })

const emit = defineEmits<{
  choose: [value: number]
}>()
const debug = useDebug()
const cardStore = useCardStore()

onMounted(() => {
  if (!cardStore.loaded) cardStore.fetchCards()
})

const cardContents = computed<CardContents>(() => {
  return props.card.tag === "CardContents" ? props.card : ( props.card.tag === "VengeanceCard" ? props.card.contents.contents : props.card.contents)
})

const cardDef = computed(() => cardStore.cards.find((c) => c.cardCode === cardContents.value.cardCode))
const isPlayerCard = computed(() => {
  if (props.card.tag === 'CardContents') return true
  if (props.card.tag === 'VengeanceCard') return props.card.contents.tag === 'PlayerCard'
  return props.card.tag === 'PlayerCard'
})
const backImage = computed(() => {
  const customBack = cardDef.value?.meta?.customBack
    ?? (cardDef.value?.cardTraits.includes('Artifact') ? 'back_artifact.jpg' : undefined)
  return imgsrc(customBack ? `backs/${customBack}` : `backs/${isPlayerCard.value ? 'back_player' : 'back_encounter'}.jpg`)
})

const isEnemyLocationCard = computed(() => {
  const id = cardContents.value.id
  return Object.values(props.game.locations).some(loc => loc.enemyLocation && loc.cardId === id)
})

const image = computed(() => {
  if (props.card.tag === 'VengeanceCard') return backImage.value

  const { cardCode, isFlipped, mutated } = cardContents.value
  if (cardFacedown(props.card) && !props.revealed) return backImage.value
  // c05178 has 6 pairs of (front,back) variants using extended alphabet
  // suffixes: 05178a/b, 05178c/d, ... 05178k/l. The card code points at
  // the back/Unfinished Business side, so when unflipped render the matching
  // previous-letter front, and when flipped render the card code as-is.
  // Some saved/flipped cards can arrive with the generic "b" suffix appended
  // to the extended code (e.g. c05178lb); canonicalize those to c05178l.
  const unfinishedBusinessBack = cardCode.match(/^(c(?:05178[bcdfhjl]|5403[89]b))b$/)?.[1]
  if (unfinishedBusinessBack) return cardImage(unfinishedBusinessBack)

  const forcedFlippedSuffix: Record<string, string> = {
    c05178b: 'a', c05178d: 'c', c05178f: 'e',
    c05178h: 'g', c05178j: 'i', c05178l: 'k',
    c54038b: '', c54039b: '',
  }
  if (cardCode in forcedFlippedSuffix) {
    return isFlipped
      ? cardImage(cardCode)
      : cardImage(cardCode.slice(0, -1), forcedFlippedSuffix[cardCode])
  }
  const revealed = props.revealed && !isEnemyLocationCard.value
  const suffix = !revealed && isFlipped ? 'b' : ''
  const mutatedSuffix = mutated ? `_${mutated}` : ''
  return cardImage(cardCode, `${suffix}${mutatedSuffix}`)
})

const id = computed(() => props.card.tag === 'VengeanceCard' ? props.card.contents.contents.id : cardContents.value.id)
const isHighlighted = computed(() => props.game.highlightedCards.includes(id.value))
const choices = computed(() => ArkhamGame.choices(props.game, props.playerId))

function canInteract(c: Message): boolean {
  if (isAbility(c)) {
    return true
  }

  if (c.tag === MessageType.TARGET_LABEL) {
    if (c.target.tag === 'SkillTarget') {
      if (typeof c.target.contents === 'string' && props.game.skills[c.target.contents].cardId == id.value) {
        return true
      }
    }
    if (c.target.tag === 'EnemyTarget') {
      if (typeof c.target.contents === 'string') {
        const enemy = props.game.enemies[c.target.contents]
        if (enemy && enemy.cardId == id.value) {
          return true
        }
      }
    }
    return c.target.contents === id.value
  }

  return false
}

const cardAction = computed(() => {
  if (!props.allowInteractions) return -1
  return choices.value.findIndex(canInteract)
})

function isAbility(v: Message): v is AbilityLabel {
  if (v.tag === MessageType.EVADE_LABEL || v.tag === MessageType.EVADE_LABEL_WITH_SKILL) {
    return v.enemyId === id.value
  }

  if (v.tag !== MessageType.ABILITY_LABEL) {
    return false
  }

  const { source } = v.ability;

  if (source.sourceTag === 'ProxySource') {
    if ("contents" in source.source) {
      return source.source.contents === id.value
    }
  }

  if (source.tag === 'AssetSource' && source.contents) {
    const asset = props.game.assets[source.contents]
    if (asset) {
      if (asset.cardId !== id.value) return false
      return props.allowInPlayAbilities
        || asset.placement.tag === 'StillInHand'
        || asset.placement.tag === 'StillInDiscard'
    }
  }

  if (props.allowInPlayAbilities && source.tag === 'TreacherySource' && source.contents) {
    const treachery = props.game.treacheries[source.contents]
    if (treachery) return treachery.cardId === id.value
  }

  return 'contents' in source && source.contents === id.value
}

const abilities = computed<AbilityMessage[]>(() => {
  if (!props.allowAbilityButtons) return []

  return choices.value
    .reduce<AbilityMessage[]>((acc, v, i) => {
      if (isAbility(v)) {
        return [...acc, { contents: v, displayAsAction: false, index: i }];
      }

      return acc;
    }, []);
})

/*
 * A card carries no tokens of its own -- `CardContents.tokens` decodes as a
 * constant {} -- so this pool only ever had something to show for a card that
 * is really an entity in play. Normally that entity draws its own pool and this
 * component is never asked to; the Hidden stack is the exception, since it
 * tucks in-play cards out of the play area. Read the pool off the entity so a
 * tucked card still shows its resources, damage and clues.
 */
const tokens = computed<Tokens>(() => {
  const own = cardContents.value.tokens
  if (own && Object.keys(own).length > 0) return own

  const cardId = id.value
  const entity = Object.values(props.game.assets).find((a) => a.cardId === cardId)
    ?? Object.values(props.game.treacheries).find((t) => t.cardId === cardId)
  return entity?.tokens ?? {}
})

const hasPool = computed(() => Object.values(tokens.value).some((n) => n))

const forceSideways = computed(() => {
  const { cardCode, isFlipped } = cardContents.value
  if (isFlipped) {
    return ["c04242"].includes(cardCode)
  }
  return false
})

const modifiers = computed(() => {
  return props.game.modifiers.reduce<Modifier[]>((acc, [target, ms]) => {
    if (target.tag === 'CardCodeTarget' && target.contents === cardContents.value.cardCode) {
      return [...acc, ...ms]
    }
    return acc
  }, [])
})

const investigatorId = computed(() => Object.values(props.game.investigators).find((i) => i.playerId === props.playerId)?.id)

const canDebugCustomize = computed(() => debug.active && !!investigatorId.value && (cardDef.value?.customizations?.length ?? 0) > 0)

function debugCustomize() {
  if (!investigatorId.value) return
  debug.send(props.game.id, { tag: 'DebugCustomize', contents: [investigatorId.value, id.value] })
}

const modifiedPlayingCard = computed(() => {
  const playingCardModifier = modifiers.value.find(m => m.type.tag === 'ScenarioModifierValue' && m.type.contents[0] === 'setPlayingCard')
  if (playingCardModifier && playingCardModifier.type.tag === 'ScenarioModifierValue') {
    const playingCard = playingCardModifier.type.contents[1]
    if (!playingCard) return
    return imgsrc(`extra/fortune-and-folly/playing-cards/${playingCard.rank}-${playingCard.suit}.png`)
  }
  return null

})

function startDrag(event: DragEvent) {
  if (!debug.active) {
    event.preventDefault()
    return
  }

  if (event.dataTransfer) {
    event.dataTransfer.effectAllowed = 'copy'
    event.dataTransfer.setData('text/plain', JSON.stringify({ tag: 'CardTarget', contents: id.value }))
    // Publish the id so drop zones can tell, mid-drag, whether they would take
    // this card -- dataTransfer is unreadable until the drop itself.
    DebugMove.beginCardDrag(id.value)
  }
}

</script>

<template>
  <div class="card-container" :data-index="id">
    <img
      v-if="modifiedPlayingCard"
      :src="modifiedPlayingCard"
      class="playing-card-overlay"
    />
    <img
      :class="{'card--can-interact': cardAction !== -1, 'card--highlighted': isHighlighted && cardAction === -1, 'sideways': forceSideways}"
      class="card"
      :src="image"
      :data-customizations="JSON.stringify(cardContents.customizations)"
      :data-chained="cardContents.chained || undefined"
      :data-pc="modifiedPlayingCard ? modifiedPlayingCard : null"
      :draggable="debug.active"
      @dragstart="startDrag"
      @click="emit('choose', cardAction)"
    />
    <span class="vengeance" v-if="card.tag === 'VengeanceCard'">{{$t('card.vengeance', {value: 1})}}</span>
    <div class="pool" v-if="hasPool">
      <TokenPool :tokens="tokens" />
    </div>
    <button
      v-if="canDebugCustomize"
      class="debug-customize"
      type="button"
      title="Debug customize"
      @click.stop="debugCustomize"
    ><font-awesome-icon icon="bug" /></button>
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

<style scoped>

.card {
  width: var(--card-width);
  min-width: var(--card-width);
  border-radius: 7px;
  box-shadow: 0 3px 6px rgba(0,0,0,0.23), 0 3px 6px rgba(0,0,0,0.53);
  border-radius: 6px;
  margin: 2px;
  display: inline-block;
}

.playing-card-overlay {
  width: calc(var(--card-width) * 0.3);
  position: absolute;
  top: 1%;
  left: 1%;
  height: auto;
  border-radius: 4px;
  box-shadow: 0 2px 4px rgba(0,0,0,0.23), 0 2px 4px rgba(0,0,0,0.53);
}

.card--can-interact {
  border: 2px solid var(--select);
  cursor: pointer;
}

.card--highlighted {
  border: 2px solid var(--highlight);
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

.debug-customize {
  position: absolute;
  top: 4px;
  left: 4px;
  z-index: var(--z-index-20);
  display: flex;
  align-items: center;
  justify-content: center;
  width: 22px;
  height: 22px;
  padding: 0;
  border: 1px solid #111;
  border-radius: 50%;
  background: rgba(255, 255, 255, 0.9);
  color: #111;
  font-size: 11px;
  cursor: pointer;
}

.debug-customize:hover {
  background: #fff;
}
</style>
