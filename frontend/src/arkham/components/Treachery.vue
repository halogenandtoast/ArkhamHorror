<script lang="ts" setup>
import { computed, ref, watch } from 'vue';
import MissingCardBadge from '@/arkham/components/MissingCardBadge.vue';
import { CARD_FLIGHT_ATTR, useCardFlight } from '@/arkham/cardFlight'
import { useDebug } from '@/arkham/debug';
import { cardDropHandlers, treacheryTarget } from '@/arkham/debugCardDrop';
import { cardImage } from '@/arkham/cardImages';
import { toCardContents } from '@/arkham/types/Card';
import type { Game } from '@/arkham/types/Game';
import * as ArkhamGame from '@/arkham/types/Game';
import type { AbilityLabel, AbilityMessage, Message } from '@/arkham/types/Message';
import TokenPool from '@/arkham/components/TokenPool.vue';
import AbilityButton from '@/arkham/components/AbilityButton.vue'
import AbilitiesMenu from '@/arkham/components/AbilitiesMenu.vue'
import { IsMobile } from '@/arkham/isMobile'
import Token from '@/arkham/components/Token.vue';
import CardDebugWindow from '@/arkham/components/debug/CardDebugWindow.vue';
import * as Arkham from '@/arkham/types/Treachery';

export interface Props {
  game: Game
  treachery: Arkham.Treachery
  playerId: string
  attached?: boolean
  overlayDelay?: number
  isInHand?: boolean
  mobileHandOpen?: boolean
  /* Can be dragged into the hidden-cards stack beside the play area. */
  tuckable?: boolean
}

const props = withDefaults(defineProps<Props>(), { attached: false })

// Where a revealed treachery lands when the revelation overlay hands it over.
const cardFlightStyle = useCardFlight(() => props.treachery.cardId)

const emits = defineEmits<{ choose: [value: number] }>()

function startDrag(event: DragEvent) {
  if (!props.tuckable || !event.dataTransfer) return
  event.dataTransfer.effectAllowed = 'copyMove'
  event.dataTransfer.setData(
    'text/plain',
    JSON.stringify({ tag: 'TreacheryTarget', contents: props.treachery.id }),
  )
}

const choose = (idx: number) => emits('choose', idx)

const debug = useDebug()
const { isMobile } = IsMobile()
const cardFrame = ref<HTMLElement | null>(null)
const showAbilities = ref(false)

watch(() => props.mobileHandOpen, (open) => {
  if (open === false) showAbilities.value = false
})
const image = computed(() => cardImage(props.treachery.cardCode))
const id = computed(() => props.treachery.id)
const debugging = ref(false)
const cardDrop = cardDropHandlers(props.game.id, () => treacheryTarget(props.treachery.id))
const choices = computed(() => ArkhamGame.choices(props.game, props.playerId))
const isExhausted = computed(() => props.treachery.exhausted)

function canInteract(c: Message): boolean {
  if (c.tag === "TargetLabel") {
    return c.target.contents === id.value || `c${id.value}` === c.target.contents || c.target.contents == props.treachery.cardId
  }

  return false
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
  } else if (source.tag === 'TreacherySource') {
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

const tokenOverrides = { Damage: { type: 'damage' } }
const cardAction = computed(() => choices.value.findIndex(canInteract))
const canUseMobileAbilityMenu = computed(() => isMobile && props.isInHand && abilities.value.length > 0)
const canHighlight = computed(() => cardAction.value !== -1 || canUseMobileAbilityMenu.value)

function handleCardClick() {
  if (canUseMobileAbilityMenu.value) {
    showAbilities.value = true
    return
  }

  emits('choose', cardAction.value)
}

/* A card whose identity has been retargeted is really something else underneath
 * -- Unleashed Chaos (Proliferation) becomes an exact copy of the card drawn
 * after it, keeping its own CardId and its real code in `originalCardCode`.
 * Without this the two copies are indistinguishable on the board. */
const impersonating = computed(() => {
  const card = props.game.cards?.[props.treachery.cardId]
  const original = card && toCardContents(card).originalCardCode
  return original && original !== props.treachery.cardCode ? original : null
})

/* Filtered rather than mapped straight off the id list: the ids and the entity
 * map arrive in the same payload, but a treachery that left play between them
 * would otherwise render as `undefined`. */
const attachedTreacheries = computed(() =>
  (props.treachery.treacheries ?? [])
    .map((tid) => props.game.treacheries[tid])
    .filter((t) => !!t),
)
</script>
<template>
  <div
    class="treachery"
    :class="{ attached, exhausted: isExhausted }"
    :[CARD_FLIGHT_ATTR]="treachery.cardId"
    :style="cardFlightStyle"
    v-bind="cardDrop"
  >
    <MissingCardBadge :card-code="treachery.cardCode" />
    <img
      v-if="impersonating"
      class="impersonating-badge"
      :src="cardImage(impersonating)"
      :title="$t('treachery.copyOf')"
      aria-hidden="true"
    />
    <AbilityButton
      v-if="isInHand && !canUseMobileAbilityMenu"
      v-for="ability in abilities"
      :key="ability.index"
      :ability="ability.contents"
      :data-image="image"
      :game="game"
      @click="$emit('choose', ability.index)"
    />
    <div class="treachery-face">
      <img
        ref="cardFrame"
        :src="image"
        class="card"
        :class="{ 'treachery--can-interact': canHighlight, attached, 'in-hand': isInHand }"
        :draggable="tuckable || undefined"
        @dragstart="startDrag"
        @click="handleCardClick"
        :data-delay="overlayDelay"
      />
      <div class="pool">
        <TokenPool
          :tokens="treachery.tokens"
          :overrides="tokenOverrides"
          :target="{ tag: 'TreacheryTarget', contents: treachery.id }"
        />
        <Token v-for="(sealedToken, index) in treachery.sealedChaosTokens" :key="index" :token="sealedToken" :playerId="playerId" :game="game" @choose="choose" />
      </div>
    </div>
    <AbilityButton
      v-if="!isInHand"
      v-for="ability in abilities"
      :key="ability.index"
      :ability="ability.contents"
      :data-image="image"
      :game="game"
      @click="$emit('choose', ability.index)"
    />
    <AbilitiesMenu
      v-if="canUseMobileAbilityMenu"
      v-model="showAbilities"
      :game="game"
      :abilities="abilities"
      :frame="cardFrame"
      position="top"
      @choose="$emit('choose', $event)"
    />

    <!-- A treachery attached to this one, drawn as a strip under the host card
         the way an enemy's attachments are. Recursive by implicit self-reference. -->
    <Treachery
      v-for="attached in attachedTreacheries"
      :key="attached.id"
      :treachery="attached"
      :game="game"
      :playerId="playerId"
      :attached="true"
      @choose="$emit('choose', $event)"
    />

    <template v-if="debug.active">
      <button @click="debug.send(game.id, {tag: 'Discard', contents: [null, { tag: 'GameSource' }, { tag: 'TreacheryTarget', contents: id}]})">{{ $t('treachery.discard') }}</button>
      <button class="debug-open" @click="debugging = true">{{ $t('enemy.debug') }}</button>
    </template>
    <CardDebugWindow
      v-if="debugging"
      :game="game"
      :title="$t('debug.card.treacheryTitle')"
      :image="image"
      :sealedTokens="treachery.sealedChaosTokens"
      :target="{ tag: 'TreacheryTarget', contents: id }"
      @close="debugging = false"
    />
  </div>
</template>


<style scoped>
.card {
  width: var(--card-width);
  max-width: var(--card-width);
  border-radius: 5px;
}

.treachery--can-interact {
  border: 2px solid var(--select);
  cursor:pointer;
}

.treachery {
  display: flex;
  flex-direction: column;
  position: relative;
  width: fit-content;
}

.treachery-face {
  position: relative;
}

.treachery-face > .card {
  display: block;
}

.attached .card {
  object-fit: cover;
  object-position: left bottom;
  height: calc(var(--card-width) * 0.6);
}

.pool {
  position: absolute;
  top: 10%;
  align-items: center;
  display: flex;
  align-self: flex-start;
  align-items: flex-end;
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

.attached .pool {
  top: auto;
  bottom: 5%;
}

/* The real card, shown small in the corner of the copy it is impersonating, so
   two identical-looking cards on the board can be told apart. */
.impersonating-badge {
  position: absolute;
  top: 2px;
  right: 2px;
  width: calc(var(--card-width) * 0.28);
  border-radius: 3px;
  box-shadow: 0 0 0 1px rgba(255, 255, 255, 0.5);
  pointer-events: none;
  z-index: 2;
}

.exhausted {
  :deep(img) {
    transition: transform 0.2s linear;
    transform: rotate(90deg) translateX(-10px);
  }
}
</style>
