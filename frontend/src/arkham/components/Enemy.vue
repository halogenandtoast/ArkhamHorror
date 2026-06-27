<script lang="ts" setup>
import { computed, ref } from 'vue'
import { useI18n } from 'vue-i18n'
import { handleEmbeddedI18n } from '@/arkham/i18n'
import { useDebug } from '@/arkham/debug'
import { useAi } from '@/arkham/ai'
import { Game } from '@/arkham/types/Game'
import { keyToId } from '@/arkham/types/Key'
import { TokenType } from '@/arkham/types/Token'
import { imgsrc } from '@/arkham/helpers'
import { cardArt, cardImage, sourceCardCode } from '@/arkham/cardImages'
import { useGameChoices, useStickyChoicesSource, useGameChoicesTooltip } from '@/arkham/composables/useGameChoices'
import { AbilityLabel, AbilityMessage, Message, MessageType } from '@/arkham/types/Message'
import AbilitiesMenu from '@/arkham/components/AbilitiesMenu.vue'
import AiTargetMenu from '@/arkham/components/AiTargetMenu.vue'
import DebugEnemy from '@/arkham/components/debug/Enemy.vue'
import PoolItem from '@/arkham/components/PoolItem.vue'
import TokenPool from '@/arkham/components/TokenPool.vue'
import KeyToken from '@/arkham/components/Key.vue'
import Treachery from '@/arkham/components/Treachery.vue'
import Asset from '@/arkham/components/Asset.vue'
import Event from '@/arkham/components/Event.vue'
import Skill from '@/arkham/components/Skill.vue'
import Token from '@/arkham/components/Token.vue'
import Story from '@/arkham/components/Story.vue'
import ScarletKey from '@/arkham/components/ScarletKey.vue';
import * as Arkham from '@/arkham/types/Enemy'
import { Source } from '@/arkham/types/Source'
import { isManifestedSpiritEnemy } from '@/arkham/spiritVisuals';
import { toCardContents } from '@/arkham/types/Card';

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

const imageId = computed(() => cardArt(props.enemy.cardCode, props.enemy.flipped ? 'b' : ''))

const image = computed(() => cardImage(props.enemy.cardCode, props.enemy.flipped ? 'b' : ''))

const id = computed(() => props.enemy.id)

const choicesSource = useStickyChoicesSource(() => props.game, () => props.playerId)
const isHighlighted = computed(() => {
  const source = choicesSource.value
  return source !== null && 'contents' in source && source.contents === props.enemy.id
})
const isAttacking = computed(() => props.game.enemyAttackTargets.some((e) => e.enemy === props.enemy.id))
const { t } = useI18n()
const choicesTooltip = useGameChoicesTooltip(() => props.game, () => props.playerId)
const sourceTooltip = computed<string | false>(() => {
  const raw = isHighlighted.value ? choicesTooltip.value : null
  return raw ? handleEmbeddedI18n(raw, t as (key: string, params: { [key: string]: any }) => string) : false
})

const choices = useGameChoices(() => props.game, () => props.playerId)

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

const referenceCards = computed(() => props.enemy.referenceCards)
const hasSpiritAura = computed(() => isManifestedSpiritEnemy(props.enemy, props.game))

function isAbility(v: Message): v is AbilityLabel {
  if (v.tag === MessageType.FIGHT_LABEL && v.enemyId === id.value) {
    return true
  }

  if (v.tag === MessageType.FIGHT_LABEL_WITH_SKILL && v.enemyId === id.value) {
    return true
  }

  if (v.tag === MessageType.EVADE_LABEL && v.enemyId === id.value) {
    return true
  }

  if (v.tag === MessageType.EVADE_LABEL_WITH_SKILL && v.enemyId === id.value) {
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

const hasObjective = computed(() =>
  abilities.value.some(
    ({ contents }) => 'ability' in contents && contents.ability.type.tag === 'Objective',
  ),
)

const isExhausted = computed(() => props.enemy.exhausted)

const keys = computed(() => props.enemy.keys)

const debug = useDebug()

const enemyDamage = computed(() => (props.enemy.tokens[TokenType.Damage] || 0) + props.enemy.assignedDamage)
const enemyTokens = computed(() => {
  const { Damage, ...rest } = props.enemy.tokens
  return rest
})
const omnipotent = computed(() => {
  const {modifiers} = props.enemy

  return modifiers.some(modifier =>
    modifier.type.tag === "OtherModifier" && modifier.type.contents === "Omnipotent"
  )
})

const important = computed(() => {
  const {modifiers} = props.enemy
  return modifiers.some((m) => m.type.tag === "UIModifier" && typeof m.type.contents === 'object' && m.type.contents.tag === "ImportantToScenario") ?? false
})

const oversized = computed(() => {
  const {modifiers} = props.enemy
  return modifiers.some((m) => m.type.tag === "UIModifier" && m.type.contents === "Oversized")
})

const uiRotation = computed(() => {
  const modifier = props.enemy.modifiers.find((m) => {
    const t = m.type
    return t.tag === 'UIModifier' && typeof t.contents === 'object' && t.contents.tag === 'Rotated'
  })

  if (!modifier || modifier.type.tag !== 'UIModifier' || typeof modifier.type.contents !== 'object') return 0
  if (modifier.type.contents.tag !== 'Rotated') return 0
  return modifier.type.contents.contents
})

function sourceIsSelf(source: Source): boolean {
  if (source.sourceTag === 'ProxySource') return sourceIsSelf(source.source)
  if (source.tag === 'AbilitySource') {
    const [inner] = (source.contents as unknown) as [Source, number]
    return sourceIsSelf(inner)
  }
  if (source.tag === 'EnemySource') return source.contents === id.value
  return false
}

const cannotBeDamagedModifier = computed(() => {
  const modifiers = props.enemy.modifiers ?? []
  return modifiers.find(
    (m) =>
      (m.type.tag === "CannotBeDamaged"
        || (m.type.tag === "OtherModifier" && m.type.contents === "CannotBeDamaged"))
      && !sourceIsSelf(m.source)
  ) ?? null
})

const isCannotBeDamaged = computed(() => cannotBeDamagedModifier.value !== null)

const cannotBeDamagedCardCode = computed<string | null>(() => {
  const m = cannotBeDamagedModifier.value
  if (!m) return null
  if (m.card) return cardArt(toCardContents(m.card).cardCode)
  return sourceCardCode(m.source, props.game)
})

const health = computed(() => {
  return props.enemy.health?.tag == "Fixed" ? props.enemy.health.contents : null
})

const evade = computed(() => {
  return props.enemy.evade?.tag == "Fixed" ? props.enemy.evade.contents : null
})

const fight = computed(() => {
  return props.enemy.fight?.tag == "Fixed" ? props.enemy.fight.contents : null
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

const ai = useAi()
const aiMenuOpen = ref(false)
const aiTarget = computed(() => ({ tag: 'EnemyTarget', contents: id.value }))

const showAbilities = ref<boolean>(false)

async function clicked() {
  if (ai.targeting) {
    aiMenuOpen.value = true
    return
  }
  if(cardAction.value !== -1) {
    emits('choose', cardAction.value)
    showAbilities.value = false
  } else if (abilities.value.length > 0) {
    showAbilities.value = !showAbilities.value
  } else {
    showAbilities.value = false
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


const dragover = (e: DragEvent) => {
  e.preventDefault()
  if (e.dataTransfer) {
    e.dataTransfer.dropEffect = 'move'
  }
}

function onDrop(event: DragEvent) {
  event.preventDefault()
  if (event.dataTransfer) {
    const data = event.dataTransfer.getData('text/plain')
    if (data) {
      const json = JSON.parse(data)
      if (json.tag === "KeyTarget") {
        debug.send(props.game.id, {tag: 'PlaceKey', contents: [{tag: 'EnemyTarget', contents: id.value}, json.contents]})
      }
    }
  }
}

</script>

<template>
  <div class="enemy--outer" :class="{showAbilities, oversized}">
    <div class="enemy">
      <Story v-if="enemyStory" :story="enemyStory" :game="game" :playerId="playerId" @choose="choose"/>
      <template v-else>
        <div class="card-frame" ref="frame">
          <div
            class="card-wrapper"
            :class="{ exhausted: isExhausted, 'enemy--objective': hasObjective, 'objective-ring': hasObjective }"
            :style="{ '--ui-rotation': `${uiRotation}deg` }"
          >
            <font-awesome-icon v-if="hasSpiritAura" :icon="['fas', 'ghost']" class="spirit-icon" />
            <span class="important" v-if="important">
              <font-awesome-icon :icon="['fa', 'circle-exclamation']" />
            </span>
            <span v-if="isCannotBeDamaged" class="cannot-be-damaged-badge" :data-image-id="cannotBeDamagedCardCode">
              <font-awesome-icon icon="shield-heart" />
            </span>
            <img v-if="isTrueForm" :src="image"
              class="card enemy"
              v-tooltip="sourceTooltip"
              :class="{ dragging, 'enemy--can-interact': canInteract && !hasObjective, 'enemy--can-interact-cursor': canInteract, attached, 'source-highlight': isHighlighted || isAttacking, 'ai-target-hover': ai.targeting }"
              :data-id="id"
              :data-card-code="enemy.cardCode"
              :data-game-id="game.id"
              :data-player-id="playerId"
              :data-is-spirit="hasSpiritAura || undefined"
              :data-fight="fight"
              :data-evade="evade"
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
              v-tooltip="sourceTooltip"
              :class="{ 'enemy--can-interact': canInteract && !hasObjective, 'enemy--can-interact-cursor': canInteract, attached, 'source-highlight': isHighlighted || isAttacking, 'ai-target-hover': ai.targeting }"
              :data-id="id"
              :data-card-code="enemy.cardCode"
              :data-game-id="game.id"
              :data-player-id="playerId"
              :data-is-spirit="hasSpiritAura || undefined"
              :data-image-id="imageId"
              :data-swarm="isSwarm || undefined"
              @click="clicked"
              @drop="onDrop"
              @dragover.prevent="dragover"
              @dragenter.prevent
            />
          </div>

          <div class="pool">
            <div class="keys" v-if="keys.length > 0">
              <KeyToken v-for="k in keys" :key="keyToId(k)" :keyToken="k" :game="game" :playerId="playerId" @choose="choose" />
            </div>
            <PoolItem v-if="!omnipotent && !attached" type="health" :amount="enemyDamage" />
            <TokenPool :tokens="enemyTokens" />
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
            :game="game"
            @choose="chooseAbility"
            />

          <AiTargetMenu
            v-model="aiMenuOpen"
            :frame="frame"
            kind="enemy"
            :target="aiTarget"
            :seat="ai.selectedSeat"
            :game-id="game.id"
            :position="atLocation ? 'right' : (inVoid || global) ? 'left' : 'top'"
            />
        </div>

      </template>
      <img v-for="card in referenceCards" :src="cardImage(card)" :key="card" class="attached card" />
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
      <ScarletKey
        v-for="(skId, idx) in enemy.scarletKeys"
        :scarletKey="game.scarletKeys[skId]"
        :game="game"
        :playerId="playerId"
        :key="skId"
        @choose="choose"
        :attached="true"
        :style="{ 'z-index': enemy.scarletKeys.length - idx }"
      />
      <Story
        v-for="storyId in enemy.stories"
        :key="storyId"
        :story="game.stories[storyId]"
        :game="game"
        :playerId="playerId"
        :atLocation="true"
        @choose="choose"
      />

      <template v-if="debug.active">
        <button @click="debugging = true">{{ $t('enemy.debug') }}</button>
      </template>
    </div>

    <div class="swarm" v-if="swarmEnemies.length > 0" :style="{ '--swarm-count': swarmEnemies.length }">
      <Enemy
        v-for="(swarmEnemy, index) in swarmEnemies"
        :key="swarmEnemy.id"
        :enemy="swarmEnemy"
        :game="game"
        :playerId="playerId"
        :atLocation="atLocation"
        @choose="$emit('choose', $event)"
        class="enemy--swarming"
        :style="{ '--swarm-index': index }"
      />
    </div>
    <DebugEnemy v-if="debugging" :game="game" :enemy="enemy" :playerId="playerId" @close="debugging = false" />
  </div>
</template>

<style scoped>
.small-treachery :deep(.card) {
  height: calc(var(--card-width) * 0.35);
}

:deep(.event) img {
  object-fit: cover;
  object-position: bottom;
  height: calc(var(--card-width) * 0.6);
  margin-top: 2px;
}

:deep(.scarletKey) {
  margin-top: calc(var(--card-width) * -0.4);
}

.enemy--can-interact {
  border: 2px solid var(--select);
  border-radius: 5px;
  cursor: pointer;
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

.enemy--can-interact-cursor {
  cursor: pointer;
}

img.card.source-highlight {
  box-shadow: 0 0 0 2px var(--important), 0 0 6px 1px var(--important), var(--card-shadow);
}

.enemy {
  display: flex;
  flex-direction: column;
  position: relative;
  z-index: var(--z-index-5);
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
  z-index: var(--z-index-15);
  :deep(img) {
    width: var(--card-token-width);
    height: auto;
  }

  :deep(.token-container) {
    width: var(--card-token-width);
  }

  &:not(:has(.key--can-interact)) {
    pointer-events: none;
  }
}

.card-wrapper {
  --ui-rotation: 0deg;
  --exhaust-rotation: 0deg;
  position: relative;
  isolation: isolate;
  width: fit-content;
  border-radius: 5px;
  transition: transform 0.2s linear;
  transform: rotate(calc(var(--ui-rotation) + var(--exhaust-rotation)));
  transform-origin: center;
}

.card-wrapper.exhausted {
  --exhaust-rotation: 90deg;
  transform: rotate(calc(var(--ui-rotation) + var(--exhaust-rotation))) translateX(-10px);
}

.spirit-icon {
  position: absolute;
  bottom: 8%;
  right: 6%;
  z-index: var(--z-index-3);
  font-size: 0.9em;
  color: rgba(180, 230, 255, 0.95);
  filter:
    drop-shadow(0 0 1px rgba(0, 0, 0, 0.9))
    drop-shadow(0 1px 2px rgba(0, 0, 0, 0.8))
    drop-shadow(0 0 5px rgba(130, 200, 255, 0.7));
  pointer-events: none;
}

:deep(.token) {
  width: 30px;
}

.card-frame {
  z-index: var(--z-index-10);
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
  z-index: var(--z-index-20000000000);

  &.right {
    bottom:50%;
    left: 100%;
    transform: translateY(50%) translateZ(0);
    z-index: var(--z-index-20000000000);
    /*z-index: var(--z-index-0);*/
  }

  &.left {
    bottom:0%;
    right: 100%;
    left: unset;
    transform: unset;
  }
}

/*
 * Swarm fan.
 *
 * Collapsed, the swarm cards hide behind the host card (below its z-index), tucked
 * left so only their right edges fan out — the host clearly owns the swarm and the
 * count stays readable. Hovering the host enemy (or the swarm) slides them out from
 * behind it and fans them to the right.
 *
 * Every offset is a `transform`, which never affects layout, so the host and
 * everything around it stay put whether collapsed or fanned (requirement a). The
 * fan stays overlapping (no holes), so the whole group is one contiguous hover
 * surface and sweeping between cards can't drop into a gap and collapse it
 * (requirement b).
 */
.swarm {
  /* how far (in card widths) the stack hides behind the host while collapsed */
  --swarm-tuck: 0.78;
  /* fraction of a card poking out from behind the host per stacked card */
  --swarm-peek: 0.22;
  /* fraction of a card revealed per card once fanned out */
  --swarm-reveal: 0.62;
  --swarm-count: 1;
  position: relative;
  /*
   * A single-cell grid: every swarm card lives in the same cell (grid-area 1/1)
   * and is offset purely with `transform`, immune to flex-shrink quirks. The width
   * reserves only the sliver that pokes out from behind the host; the fan overflows
   * this box on hover (transform only), so neighbours never shift either way.
   */
  display: grid;
  grid-template-columns: var(--card-width);
  justify-content: start;
  width: calc(var(--card-width) * ((1 - var(--swarm-tuck)) + (var(--swarm-count) - 1) * var(--swarm-peek)));
  flex: 0 0 auto;
  align-self: center;
}

.swarm .enemy--swarming {
  --swarm-index: 0;
  grid-area: 1 / 1;
  /* Behind the host (host card is var(--z-index-5)); lead card on top of the rest. */
  z-index: calc(var(--swarm-count) - var(--swarm-index));
  transform: translateX(calc(var(--card-width) * (var(--swarm-peek) * var(--swarm-index) - var(--swarm-tuck))));
  transition: transform 0.18s ease;
}

/* Hovering the swarm (its peeking edges) fans the cards out from behind the host to
   the right; likewise while a swarm card's abilities menu is open. */
.swarm:hover .enemy--swarming,
.swarm:has(.enemy--swarming.showAbilities) .enemy--swarming {
  transform: translateX(calc(var(--card-width) * var(--swarm-reveal) * var(--swarm-index)));
}

/* Lift the whole group above sibling enemies while revealed. */
.enemy--outer:has(.swarm:hover),
.enemy--outer:has(.enemy--swarming.showAbilities) {
  position: relative;
  z-index: var(--z-index-30);
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

.important {
  position: absolute;
  bottom: 10%;
  border-radius: 1000px;
  font-size: 2.6em;
  color: var(--important);
  filter: drop-shadow(0px 0px 1px #000) drop-shadow(0px 0px 2px #000);
  pointer-events: none;
  left: 50%;
  transform: translateX(-50%);
  z-index: var(--z-index-1);
  max-width: 40%;
  max-height: min-content;
  aspect-ratio: 1 / 1;
  display: flex;
  align-items: center;
  justify-content: center;
  z-index: var(--z-index-6);
}

.cannot-be-damaged-badge {
  position: absolute;
  right: 8px;
  bottom: 8px;
  width: 28px;
  height: 28px;
  color: #e05252;
  font-size: 22px;
  line-height: 1;
  filter:
    drop-shadow(0 0 1px #000)
    drop-shadow(0 0 2px #000)
    drop-shadow(0 1px 3px rgba(0, 0, 0, 0.9));
  cursor: default;
  z-index: var(--z-index-7);
  display: flex;
  align-items: center;
  justify-content: center;

  :deep(svg) {
    display: block;
    width: 1em;
    height: 1em;
  }
}

.oversized img {
  transition: width 0.8s ease;
  width: calc(var(--card-width) * 3);
  max-width: calc(var(--card-width) * 3);
}
</style>
