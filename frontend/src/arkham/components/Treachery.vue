<script lang="ts" setup>
import { computed } from 'vue';
import { useDebug } from '@/arkham/debug';
import { imgsrc } from '@/arkham/helpers';
import type { Game } from '@/arkham/types/Game';
import { TokenType } from '@/arkham/types/Token';
import * as ArkhamGame from '@/arkham/types/Game';
import type { AbilityLabel, AbilityMessage, Message } from '@/arkham/types/Message';
import PoolItem from '@/arkham/components/PoolItem.vue';
import AbilityButton from '@/arkham/components/AbilityButton.vue'
import Token from '@/arkham/components/Token.vue';
import * as Arkham from '@/arkham/types/Treachery';

export interface Props {
  game: Game
  treachery: Arkham.Treachery
  playerId: string
  attached?: boolean
}

const props = withDefaults(defineProps<Props>(), { attached: false })

const emits = defineEmits<{ choose: [value: number] }>()

const choose = (idx: number) => emits('choose', idx)

const debug = useDebug()
const image = computed(() => {
  return imgsrc(`cards/${props.treachery.cardCode.replace('c', '')}.jpg`)
})
const id = computed(() => props.treachery.id)
const choices = computed(() => ArkhamGame.choices(props.game, props.playerId))

function canInteract(c: Message): boolean {
  if (c.tag === "TargetLabel") {
    return c.target.contents === id.value || `c${id.value}` === c.target.contents
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

const doom = computed(() => props.treachery.tokens[TokenType.Doom])
const clues = computed(() => props.treachery.tokens[TokenType.Clue])
const resources = computed(() => props.treachery.tokens[TokenType.Resource])
const horror = computed(() => props.treachery.tokens[TokenType.Horror])
const evidence = computed(() => props.treachery.tokens[TokenType.Evidence])

const cardAction = computed(() => choices.value.findIndex(canInteract))
</script>
<template>
  <div class="treachery" :class="{ attached: attached }">
    <img
      :src="image"
      class="card"
      :class="{ 'treachery--can-interact': cardAction !== -1 }"
      @click="$emit('choose', cardAction)"
    />
    <AbilityButton
      v-for="ability in abilities"
      :key="ability.index"
      :ability="ability.contents"
      :data-image="image"
      @click="$emit('choose', ability.index)"
      />
    <div class="pool">
      <PoolItem
        v-if="horror && horror > 0"
        type="horror"
        :amount="horror"
      />
      <PoolItem
        v-if="clues && clues > 0"
        type="clue"
        :amount="clues"
      />
      <PoolItem
        v-if="resources && resources > 0"
        type="resource"
        :amount="resources"
      />
      <PoolItem
        v-if="doom && doom > 0"
        type="doom"
        :amount="doom"
      />
      <PoolItem v-if="evidence && evidence > 0" type="resource" tooltip="Evidence" :amount="evidence" />
      <Token v-for="(sealedToken, index) in treachery.sealedChaosTokens" :key="index" :token="sealedToken" :playerId="playerId" :game="game" @choose="choose" />
    </div>

    <template v-if="debug.active">
      <button @click="debug.send(game.id, {tag: 'Discard', contents: [null, { tag: 'GameSource' }, { tag: 'TreacheryTarget', contents: id}]})">Discard</button>
    </template>
  </div>
</template>


<style lang="scss" scoped>
.card {
  width: var(--card-width);
  max-width: var(--card-width);
  border-radius: 5px;
}

.treachery--can-interact {
  border: 2px solid $select;
  cursor:pointer;
}

.treachery {
  display: flex;
  flex-direction: column;
  position: relative;
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
    width: 20px;
    height: auto;
  }


  pointer-events: none;
}
</style>
