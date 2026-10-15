<script lang="ts" setup>

import { computed, ref, onBeforeUnmount, nextTick } from 'vue'
import * as ArkhamGame from '@/arkham/types/Game'
import { useDebug } from '@/arkham/debug';
import { imgsrc } from '@/arkham/helpers';
import { Game } from '@/arkham/types/Game';
import { ConcealedCard, concealedCardBackPath, concealedCardImagePath } from '@/arkham/types/ConcealedCard';
import DebugConcealedCard from '@/arkham/components/debug/ConcealedCard.vue';
import AbilitiesMenu from '@/arkham/components/AbilitiesMenu.vue'
import { AbilityLabel, AbilityMessage, Message, MessageType } from '@/arkham/types/Message'

const props = defineProps<{
  game: Game
  card: ConcealedCard
  playerId: string
}>()

const emit = defineEmits<{ choose: [value: number] }>()

const debug = useDebug()
const debugging = ref(false)
const frame = ref(null)
const id = computed(() => props.card.id)
const choices = computed(() => ArkhamGame.choices(props.game, props.playerId))
const choose = (idx: number) => emit('choose', idx)

const showAbilities = ref<boolean>(false)

const abilitiesEl = ref<HTMLElement | null>(null)

async function chooseAbility(ability: number) {
  abilitiesEl.value?.blur()
  choose(ability)
}

const faceDown = computed(() => !props.card.flipped && !props.card.known)

const image = computed(() =>
  imgsrc(faceDown.value ? concealedCardBackPath : concealedCardImagePath(props.card.kind))
)

const concealedImage = computed(() => imgsrc(concealedCardBackPath))

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
    if ("contents" in source.source) {
      return source.source.contents === id.value
    }
  } else if (source.tag === 'ConcealedCardSource') {
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

function isCardAction(c: Message): boolean {
  if (c.tag === MessageType.TARGET_LABEL && c.target.contents === id.value) {
    return true
  }
  return false
}

const canInteract = computed(() => abilities.value.length > 0 || cardAction.value !== -1)
const cardAction = computed(() => choices.value.findIndex(isCardAction))

let clickTimeout: ReturnType<typeof setTimeout> | null = null
// clickCount is used to determine if the user clicked once or twice
let clickCount = 0

onBeforeUnmount(() => {
  if (clickTimeout) {
    clearTimeout(clickTimeout)
    clickTimeout = null
  }
  clickCount = 0
})

async function clicked() {
  clickCount++;
  if (clickTimeout) clearTimeout(clickTimeout);
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

</script>

<template>
  <div class="concealed-card" ref="frame">
    <img
      v-if="!card.flipped && card.known"
      :src="concealedImage"
      :data-image="image"
      :class="{'concealed-card--can-interact': canInteract}"
      class="concealed-card"
      @click="clicked"
    />
    <img
      v-else
      :src="image"
      :data-image="image"
      :class="{'concealed-card--can-interact': canInteract}"
      class="concealed-card"
      @click="clicked"
    />
    <AbilitiesMenu
      v-model="showAbilities"
      :abilities="abilities"
      :frame="frame"
      :show-move="abilities.length > 1"
      :game="game"
      position="left"
      @choose="chooseAbility"
    />
    <template v-if="debug.active">
      <button class="debug-open" @click="debugging = true">{{ $t('concealedCard.debug') }}</button>
    </template>
    <DebugConcealedCard v-if="debugging" :game="game" :card="card" :playerId="playerId" @close="debugging = false" @choose="$emit('choose', $event)"/>
  </div>
</template>

<style scoped>
.concealed-card--can-interact {
  border: 1px solid var(--select);
  border-radius: 2px;
  cursor: pointer;
}
</style>
