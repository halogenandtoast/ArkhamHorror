<script lang="ts" setup>
import { computed, watch, ref } from 'vue';
import { Dropdown } from 'floating-vue';
import useHighlighter from '@/composable/useHighlighter';
import { useDebug } from '@/arkham/debug';
import { useAi } from '@/arkham/ai';
import { TokenType } from '@/arkham/types/Token';
import { imgsrc } from '@/arkham/helpers';
import { cardImage } from '@/arkham/cardImages';
import { cardImage as cardToImage, asCardCode, toCardContents, type Card as ArkhamCard } from '@/arkham/types/Card';
import { keyToId } from '@/arkham/types/Key'
import type { Game } from '@/arkham/types/Game';
import { useGameChoices } from '@/arkham/composables/useGameChoices';
import type { AbilityLabel, AbilityMessage, Message } from '@/arkham/types/Message';
import type { AbilityType } from '@/arkham/types/Ability';
import { MessageType } from '@/arkham/types/Message';
import ScarletKey from '@/arkham/components/ScarletKey.vue';
import DebugAsset from '@/arkham/components/debug/Asset.vue';
import KeyToken from '@/arkham/components/Key.vue';
import Investigator from '@/arkham/components/Investigator.vue';
import Event from '@/arkham/components/Event.vue';
import Enemy from '@/arkham/components/Enemy.vue';
import Treachery from '@/arkham/components/Treachery.vue';
import TokenPool, { type TokenPoolItem } from '@/arkham/components/TokenPool.vue';
import CardsUnderIndicator from '@/arkham/components/CardsUnderIndicator.vue';
import AbilitiesMenu from '@/arkham/components/AbilitiesMenu.vue'
import AiTargetMenu from '@/arkham/components/AiTargetMenu.vue'
import Story from '@/arkham/components/Story.vue';
import Token from '@/arkham/components/Token.vue';
import * as Arkham from '@/arkham/types/Asset';
import { isManifestedSpiritAsset } from '@/arkham/spiritVisuals';
import { useDbCardStore } from '@/stores/dbCards'

const props = withDefaults(defineProps<{
  game: Game
  asset: Arkham.Asset
  playerId: string
  atLocation?: boolean
}>(), { atLocation: false })

const debugging = ref(false)
const frame = ref(null)
const dbCardStore = useDbCardStore()

const emits = defineEmits<{
  choose: [value: number]
}>()

const id = computed(() => props.asset.id)
const ai = useAi()
const aiMenuOpen = ref(false)
const aiTarget = computed(() => ({ tag: 'AssetTarget', contents: id.value }))
const exhausted = computed(() => props.asset.exhausted)
const jammed = computed(() => props.asset.rifleStatus === 'Jammed')
const highlighter = useHighlighter()
const isHighlighted = computed(() => highlighter.highlighted.value === props.asset.id)
const isAttackTarget = computed(() => props.game.enemyAttackTargets.some((e) => e.target.contents === props.asset.id))

const uiRotation = computed<number>(() => {
  const mods = props.asset.modifiers ?? []

  for (let i = mods.length - 1; i >= 0; i--) {
    const t: any = mods[i]?.type
    if (t?.tag === 'UIModifier' && t?.contents?.tag === 'Rotated') {
      return t.contents.contents
    }
  }

  return 0
})

const cardCode = computed(() => props.asset.cardCode)
const isTheBeyond = computed(() => cardCode.value === 'c90052')
const investigators = computed(() => Object.values(props.game.investigators).filter((i) => {
  if (i.placement.tag === 'InVehicle') return i.placement.contents === id.value
  if (i.placement.tag === 'AttachedToAsset') return i.placement.contents[0] === id.value

  return false
}))
const marketPopoverShown = ref(false)
const knownMarketDeck = computed(() => props.asset.knownMarketDeck ?? [])
const marketDeckCardImage = (card: ArkhamCard) => imgsrc(cardToImage(card))

// Heretics are shuffled into the spirit deck enemy-side faceup, so their identity
// is public. They arrive as EncounterCards (the allies / Vengeful Shade are
// PlayerCards), so when a Heretic is on top we show it (dimmed, since it hasn't
// been drawn yet) instead of the facedown player back.
const spiritDeckTop = computed<ArkhamCard | null>(() => props.asset.spiritDeck?.[0] ?? null)
const spiritDeckTopRevealed = computed(() => spiritDeckTop.value?.tag === 'EncounterCard')
const spiritDeckTopImage = computed(() =>
  spiritDeckTopRevealed.value && spiritDeckTop.value
    ? imgsrc(cardToImage(spiritDeckTop.value))
    : imgsrc('player_back.jpg'),
)
const marketDeckCardCode = (card: ArkhamCard) => asCardCode(card).replace(/^c/, '')
const marketDeckCardImageId = (card: ArkhamCard) => toCardContents(card).art ?? marketDeckCardCode(card)
const marketDeckCardName = (card: ArkhamCard) => {
  const contents = toCardContents(card)
  const dbCard = dbCardStore.getDbCard(contents.art ?? marketDeckCardCode(card))
  if (!dbCard) return 'Unknown card'
  return dbCard.subname ? `${dbCard.name}: ${dbCard.subname}` : dbCard.name
}
const marketDeckSize = computed(() => props.asset.marketDeck?.length ?? 0)
const marketDeckSlots = computed(() => {
  const known = knownMarketDeck.value
  const firstKnownPosition = marketDeckSize.value - known.length + 1

  return Array.from({ length: 10 }, (_, index) => {
    const position = index + 1
    const knownIndex = position - firstKnownPosition
    return {
      position,
      card: knownIndex >= 0 && knownIndex < known.length ? known[knownIndex] : null,
    }
  })
})

const image = computed(() => {
  if (props.asset.flipped) {
    if (cardCode.value === "c90052") return cardImage(cardCode.value, 'b')
    if (cardCode.value === "c88043") return cardImage(cardCode.value, 'b')
    return imgsrc(`player_back.jpg`)
  }
  const mutated = props.asset.mutated ? `_${props.asset.mutated}` : ''
  return cardImage(cardCode.value, mutated)
})

const dataImage = computed(() => {
  const mutated = props.asset.mutated ? `_${props.asset.mutated}` : ''
  if (props.asset.flipped) {
    if (cardCode.value === "c90052") {
      return "90052b"
    }
  }
  return cardCode.value.replace('c', '') + mutated
})
const choices = useGameChoices(() => props.game, () => props.playerId)

function isCardAction(c: Message): boolean {
  if (c.tag === MessageType.TARGET_LABEL) {
    return c.target.contents === id.value || c.target.contents === props.asset.cardId
      || `c${id.value}` === c.target.contents

  }

  return false
}

function canAdjustHealth(c: Message): boolean {
  if (c.tag === MessageType.COMPONENT_LABEL && c.component.tag === "AssetComponent" && c.component.tokenType === "DamageToken") {
    return c.component.assetId === id.value
  }
  return false
}

function canAdjustSanity(c: Message): boolean {
  if (c.tag === MessageType.COMPONENT_LABEL && c.component.tag === "AssetComponent" && c.component.tokenType === "HorrorToken") {
    return c.component.assetId === id.value
  }
  return false
}

const cardAction = computed(() => choices.value.findIndex(isCardAction))
const canInteract = computed(() => abilities.value.length > 0 || cardAction.value !== -1)
const healthAction = computed(() => choices.value.findIndex(canAdjustHealth))
const sanityAction = computed(() => choices.value.findIndex(canAdjustSanity))

const isSpirit = computed(() => isManifestedSpiritAsset(props.asset))

function isAbility(v: Message): v is AbilityLabel {
  if (v.tag === MessageType.FIGHT_LABEL && v.enemyId === id.value) {
    return true
  }

  if (v.tag === MessageType.FIGHT_LABEL_WITH_SKILL && v.enemyId === id.value) {
    return true
  }

  if ((v.tag === MessageType.EVADE_LABEL || v.tag === MessageType.EVADE_LABEL_WITH_SKILL) && v.enemyId === id.value) {
    return true
  }

  if (v.tag !== MessageType.ABILITY_LABEL) {
    return false
  }

  const { source } = v.ability;

  if (source.sourceTag === 'ProxySource') {
    if (source.source.tag === 'CardCodeSource') {
      return 'contents' in source.originalSource && source.originalSource.contents === id.value
    }
    if ("contents" in source.source) {
      return source.source.contents === id.value
    }
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
        return [...acc, { contents: v, displayAsAction: false, index: i }];
      }

      return acc;
    }, []);
})

const cardsUnderneath = computed(() => props.asset.cardsUnderneath)

const keys = computed(() => props.asset.keys)

const debug = useDebug()
const dragging = ref(false)

const assetTokens = computed(() => {
  const { Damage, Horror, ...rest } = props.asset.tokens
  return rest
})
const damage = computed(() => (props.asset.tokens[TokenType.Damage] || 0) + props.asset.assignedHealthDamage - props.asset.assignedHealthHeal)
const horror = computed(() => (props.asset.tokens[TokenType.Horror] || 0) + props.asset.assignedSanityDamage - props.asset.assignedSanityHeal)
const forcedTokenItems = computed<TokenPoolItem[]>(() => [
  {
    key: 'health',
    type: 'health',
    amount: damage.value || 0,
    force: !isSpirit.value && (cardCode.value == 'c07189' || (props.asset.health !== null || (damage.value || 0) > 0)),
    class: { 'health--can-interact': healthAction.value !== -1 },
  },
  {
    key: 'sanity',
    type: 'sanity',
    amount: horror.value || 0,
    force: !isSpirit.value && (cardCode.value == 'c07189' || (props.asset.sanity !== null || (horror.value || 0) > 0)),
    class: { 'sanity--can-interact': sanityAction.value !== -1 },
  },
])

function chooseTokenPoolItem(key: string) {
  if (key === 'health') choose(healthAction.value)
  if (key === 'sanity') choose(sanityAction.value)
}

const hasPool = computed(() => {
  const {
    sanity,
    health,
    tokens,
    sealedChaosTokens,
    keys,
  } = props.asset;

  return cardCode.value == 'c07189' || (Object.values(tokens).some((v) => (v ?? 0) > 0) || sealedChaosTokens.length > 0 || keys.length > 0 || sanity || health)
})

const choose = (idx: number) => emits('choose', idx)

const showAbilities = ref<boolean>(false)

async function clicked() {
  if (ai.targeting) {
    aiMenuOpen.value = true
    return
  }
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
  let isForced = (type: AbilityType) => {
    switch (type.tag) {
      case "ForcedAbility": return true
      case "DelayedAbility": return isForced(type.abilityType)
      default: return false
    }
  }
  if (abilities.some(a => "ability" in a.contents && isForced(a.contents.ability.type))) {
    showAbilities.value = true
  }

  if (abilities.length === 0) {
    showAbilities.value = false
  }
})

const assetStory = computed(() => {
  const { stories } = props.game
  return Object.values(stories).find((s) =>
    s.otherSide?.contents === props.asset.id ||
    (s.placement.tag === "AttachedToAsset" && s.placement.contents[0] === props.asset.id)
  )
})

function startDrag(event: DragEvent) {
  dragging.value = true
  if (event.dataTransfer) {
    event.dataTransfer.effectAllowed = 'move'
    event.dataTransfer.setData('text/plain', JSON.stringify({ "tag": "AssetTarget", "contents": props.asset.id }))
  }
}
</script>

<template>
  <div class="asset--outer">
    <Story v-if="assetStory" :story="assetStory" :game="game" :playerId="playerId" @choose="choose"/>
    <div v-else class="asset" :data-index="asset.cardId">
      <div class="card-frame" ref="frame">
        <div v-if="asset.marketDeck" class="market-deck">
          <img
            class="deck card no-overlay"
            :src="imgsrc('player_back.jpg')"
            width="150px"
          />
          <span class="deck-size">{{asset.marketDeck.length}}</span>
          <Dropdown
            placement="right"
            :distance="12"
            v-model:shown="marketPopoverShown"
            :triggers="[]"
            :auto-hide="true"
            theme="market-deck-popover"
          >
            <button
              type="button"
              class="market-helper-button"
              aria-label="View Underworld Market known order"
              v-tooltip="'View Underworld Market known order'"
              @click.stop.prevent="marketPopoverShown = !marketPopoverShown"
            >
              <font-awesome-icon icon="store" />
            </button>

            <template #popper>
              <div class="market-popover no-card-overlay">
                <div class="market-popover__header">Underworld Market</div>
                <div class="market-popover__slots">
                  <div v-for="slot in marketDeckSlots" :key="slot.position" class="market-popover__slot">
                    <span class="market-popover__number">{{ slot.position }}</span>
                    <template v-if="slot.card">
                      <img
                        class="market-popover__image"
                        :src="marketDeckCardImage(slot.card)"
                        :data-image-id="marketDeckCardImageId(slot.card)"
                      />
                      <span class="market-popover__card-name">{{ marketDeckCardName(slot.card) }}</span>
                    </template>
                    <span v-else class="market-popover__unknown">-------</span>
                  </div>
                </div>
              </div>
            </template>
          </Dropdown>
        </div>
        <div v-if="asset.spiritDeck" class="spirit-deck">
          <img
            class="deck card"
            :class="{ 'spirit-deck--revealed': spiritDeckTopRevealed }"
            :src="spiritDeckTopImage"
            width="150px"
          />
          <span class="deck-size">{{asset.spiritDeck.length}}</span>
        </div>
        <div class="card-wrapper" :class="{ 'asset--can-interact': canInteract}">
          <font-awesome-icon v-if="isSpirit" :icon="['fas', 'ghost']" class="spirit-icon" />
          <span v-if="jammed" class="status-icon" v-tooltip="'Jammed'">
            <font-awesome-icon :icon="['fas', 'wrench']" />
          </span>
          <img
            :data-id="id"
            :data-image-id="dataImage"
            :data-is-spirit="isSpirit || undefined"
            :src="image"
            class="card"
            :class="{ exhausted, 'ability-target': isHighlighted || isAttackTarget, 'ai-target-hover': ai.targeting }"
            :style="{ '--ui-rotation': `${uiRotation}deg` }"
            :data-rotation="uiRotation || undefined"
            :draggable="debug.active"
            @dragstart="startDrag"
            @click="clicked"
            :data-customizations="JSON.stringify(asset.customizations)"
          />
          <div v-if="investigators.length > 0" class="in-vehicle">
            <div v-for="investigator in investigators" :key="investigator.id">
              <Investigator
                :game="game"
                :choices="choices"
                :playerId="playerId"
                :portrait="true"
                :investigator="investigator"
                @choose="$emit('choose', $event)"
                />
            </div>
          </div>
        </div>
        <div v-if="hasPool" class="pool">
          <div class="keys" v-if="keys.length > 0">
            <KeyToken v-for="k in keys" :key="keyToId(k)" :keyToken="k" :game="game" :playerId="playerId" @choose="choose" />
          </div>
          <TokenPool :tokens="assetTokens" :extra-items="forcedTokenItems" @choose="chooseTokenPoolItem" />
          <Token v-for="(sealedToken, index) in asset.sealedChaosTokens" :key="index" :token="sealedToken" :playerId="playerId" :game="game" @choose="choose" />
        </div>
        <AbilitiesMenu
          v-model="showAbilities"
          :frame="frame"
          :abilities="abilities"
          :game="game"
          @choose="chooseAbility"
        />

        <AiTargetMenu
          v-model="aiMenuOpen"
          :frame="frame"
          kind="asset"
          :target="aiTarget"
          :seat="ai.selectedSeat"
          :game-id="game.id"
        />
      </div>
      <CardsUnderIndicator
        v-if="cardsUnderneath.length > 0"
        class="asset-cards-under"
        :cards="cardsUnderneath"
        :game="game"
        :playerId="playerId"
        label="Cards underneath"
        @choose="$emit('choose', $event)"
      />
      <Event
        v-for="eventId in asset.events"
        :event="game.events[eventId]"
        :game="game"
        :playerId="playerId"
        :key="eventId"
        :attached="true"
        @choose="$emit('choose', $event)"
      />
      <Treachery
        v-for="treacheryId in asset.treacheries"
        :treachery="game.treacheries[treacheryId]"
        :game="game"
        :attached="true"
        :playerId="playerId"
        :key="treacheryId"
        @choose="$emit('choose', $event)"
      />
      <ScarletKey
        v-for="skId in asset.scarletKeys"
        :scarletKey="game.scarletKeys[skId]"
        :game="game"
        :playerId="playerId"
        :key="skId"
        @choose="choose"
        :attached="true"
      />
      <template v-if="debug.active">
        <button @click="debugging = true">{{ $t('enemy.debug') }}</button>
      </template>
      <template v-if="isTheBeyond">
        <div v-if="(asset.assets?.length ?? 0) > 0 || (asset.enemies?.length ?? 0) > 0" class="spirit-manifest-row">
          <Asset
            v-for="assetId in asset.assets"
            :asset="game.assets[assetId]"
            :game="game"
            :playerId="playerId"
            :key="assetId"
            @choose="$emit('choose', $event)"
          />
          <Enemy
            v-for="enemyId in asset.enemies"
            :enemy="game.enemies[enemyId]"
            :game="game"
            :playerId="playerId"
            :key="enemyId"
            @choose="$emit('choose', $event)"
          />
        </div>
      </template>
      <template v-else>
        <Asset
          v-for="assetId in asset.assets"
          :asset="game.assets[assetId]"
          :game="game"
          :playerId="playerId"
          :key="assetId"
          @choose="$emit('choose', $event)"
        />
        <Enemy
          v-for="enemyId in asset.enemies"
          :enemy="game.enemies[enemyId]"
          :game="game"
          :playerId="playerId"
          :key="enemyId"
          @choose="$emit('choose', $event)"
        />
      </template>
    </div>
    <DebugAsset v-if="debugging" :game="game" :asset="asset" :playerId="playerId" @close="debugging = false" @choose="$emit('choose', $event)"/>
  </div>
</template>

<style scoped>
.card {
  --exhaust-rotation: 0deg;
  --ui-rotation: 0deg;
  width: var(--card-width);
  max-width: var(--card-width);
  border-radius: 5px;
  transition: transform 0.2s linear;
  transform: rotate(calc(var(--exhaust-rotation) + var(--ui-rotation)));
  transform-origin: center;
}

.asset {
  display: flex;
  flex-direction: column;
}

.exhausted {
  --exhaust-rotation: 90deg;
  margin: 0 30px;
}

.asset--can-interact {
  img {
    border: 2px solid var(--select);
    cursor:pointer;
  }
}

.pool {
  position: absolute;
  top: 50%;
  align-items: center;
  width: 100%;
  display: flex;
  flex-wrap: wrap;
  :deep(.token-container) {
    width: unset;
  }
  :deep(img) {
    width: var(--card-token-width);
    height: auto;
  }

  pointer-events: none;
}

.button{
  margin-top: 2px;
  border: 0;
  color: #fff;
  border-radius: 4px;
  border: 1px solid var(--select);
}

:deep(.token) {
  width: 30px;
  height: 30px;
}

:deep(.event img) {
  object-fit: cover;
  object-position: bottom;
  height: 36px;
  margin-top: 2px;
}

:deep(.event .exhausted) {
  padding: 0px;
}

.card-frame {
  position: relative;
  display: flex;
  align-items: center;
  justify-content: center;
}

img.card {
  transition: box-shadow 120ms ease;
}

img.card.ability-target {
  box-shadow: 0 0 0 2px var(--highlight), 0 0 6px 1px var(--highlight), var(--card-shadow);
}

/* Dev-only "AI targeting mode": class is only bound while targeting is on, so
   normal play is untouched. Green border + pale green wash on hover. */
.ai-target-hover {
  cursor: pointer;
  transition: box-shadow 120ms ease, filter 120ms ease;
}

.ai-target-hover:hover {
  border: 2px solid var(--ai-target);
  border-radius: 5px;
  box-shadow: 0 0 0 2px var(--ai-target), 0 0 12px 3px rgba(74, 222, 128, 0.55);
  filter: brightness(1.05) sepia(0.35) hue-rotate(55deg) saturate(1.3);
}

.deck-size {
  pointer-events: none;
  position: absolute;
  font-weight: bold;
  font-size: 1.2em;
  color: rgba(255, 255, 255, 0.6);
  left: 50%;
  top: 37%;
  transform: translateX(-50%) translateY(-50%);
  background: rgba(0, 0, 0, 0.6);
  border-radius: 50%;
  width: 1.5em;
  aspect-ratio: 1 / 1;
  display: grid;
  place-items: center;
}

.market-deck {
  position: relative;
  margin-right: 5px;
}

.market-helper-button {
  position: absolute;
  left: 50%;
  bottom: 12px;
  z-index: var(--z-index-2);
  transform: translateX(-50%);
  display: inline-flex;
  align-items: center;
  justify-content: center;
  min-width: 30px;
  height: 24px;
  padding: 0 8px;
  color: #c8a96e;
  background: rgba(0, 0, 0, 0.72);
  border: 1px solid rgba(200, 169, 110, 0.52);
  border-radius: 999px;
  box-shadow: 0 2px 8px rgba(0, 0, 0, 0.45);
  cursor: pointer;
}

.market-helper-button:hover {
  background: rgba(0, 0, 0, 0.9);
  border-color: rgba(200, 169, 110, 0.85);
}

.market-popover {
  min-width: 260px;
  padding: 10px;
}

.market-popover__header {
  margin-bottom: 8px;
  color: #c8a96e;
  font-size: 0.85rem;
  font-weight: 800;
  letter-spacing: 0.05em;
  text-transform: uppercase;
}

.market-popover__slots {
  display: grid;
  grid-template-columns: 1fr;
  gap: 4px;
}

.market-popover__slot {
  display: grid;
  grid-template-columns: 24px 34px 1fr;
  align-items: center;
  gap: 8px;
  min-height: 42px;
  padding: 3px 7px;
  color: #e8e1d2;
  background: rgba(255, 255, 255, 0.04);
  border: 1px solid rgba(255, 255, 255, 0.07);
  border-radius: 6px;
}

.market-popover__number {
  display: grid;
  place-items: center;
  width: 22px;
  height: 22px;
  color: #1d170f;
  background: #c8a96e;
  border-radius: 50%;
  font-size: 0.75rem;
  font-weight: 900;
}

.market-popover__image {
  width: 30px;
  border-radius: 3px;
  box-shadow: 0 1px 4px rgba(0, 0, 0, 0.55);
}

.market-popover__card-name {
  min-width: 0;
  overflow: hidden;
  text-overflow: ellipsis;
  white-space: nowrap;
  font-size: 0.78rem;
  font-weight: 700;
}

.market-popover__unknown {
  grid-column: 2 / 4;
  color: rgba(255, 255, 255, 0.48);
  font-family: monospace;
  font-size: 0.85rem;
  letter-spacing: 0.08em;
}

.spirit-deck {
  position: relative;
  margin-right: 5px;
}

/* A Heretic faceup on top of the spirit deck: shown, but dimmed since it has
   not been drawn yet. */
.spirit-deck--revealed {
  filter: brightness(0.7);
}

.spirit-manifest-row {
  display: flex;
  flex-direction: row;
  flex-wrap: wrap;
  gap: 10px;
  align-items: flex-start;
  margin-top: 8px;
}

.card-wrapper {
  position: relative;
}

.asset-cards-under {
  align-self: center;
  margin-top: 1px;
  margin-bottom: 1px;
}

.spirit-icon {
  position: absolute;
  bottom: 8%;
  right: 0%;
  z-index: var(--z-index-3);
  font-size: 0.9em;
  color: rgba(180, 230, 255, 0.95);
  filter:
    drop-shadow(0 0 1px rgba(0, 0, 0, 0.9))
    drop-shadow(0 1px 2px rgba(0, 0, 0, 0.8))
    drop-shadow(0 0 5px rgba(130, 200, 255, 0.7));
  pointer-events: none;
}

.status-icon {
  position: absolute;
  top: 4px;
  left: 4px;
  z-index: var(--z-index-3);
  width: 1.6em;
  height: 1.6em;
  border-radius: 50%;
  background: rgba(40, 40, 40, 0.85);
  border: 1px solid rgba(0, 0, 0, 0.7);
  box-shadow:
    0 0 0 1px rgba(220, 220, 220, 0.5),
    0 2px 4px rgba(0, 0, 0, 0.7);
  font-size: 0.95em;
  color: #e8e8e8;
  display: flex;
  align-items: center;
  justify-content: center;
  pointer-events: auto;
}

.in-vehicle {
  position: absolute;
  top: 2px;
  left: 2px;
  width: 100%;
  display: grid;
  grid-template-columns: 1fr 1fr;
  grid-template-rows: 1fr 1fr;
  pointer-events: none;
  gap: 4px;

  .vehicle-investigator {
    line-height: 0;
    height: fit-content;
  }

  &:deep(.portrait) {
    pointer-events: auto;
    width: 100%;
  }
}
</style>

<style>
.v-popper--theme-market-deck-popover {
  z-index: calc(var(--z-card-hover-overlay) - 1);
}

.v-popper--theme-market-deck-popover .v-popper__inner {
  background: rgba(15, 15, 20, 0.94);
  backdrop-filter: blur(8px);
  border: 1px solid rgba(200, 169, 110, 0.28);
  border-radius: 10px;
  color: #fff;
  box-shadow: 0 8px 30px rgba(0, 0, 0, 0.55);
}

.v-popper--theme-market-deck-popover .v-popper__arrow-outer {
  border-color: rgba(200, 169, 110, 0.28);
}

.v-popper--theme-market-deck-popover .v-popper__arrow-inner {
  border-color: rgba(15, 15, 20, 0.94);
}
</style>
