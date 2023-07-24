<script lang="ts" setup>
import { computed } from 'vue';
import { imgsrc } from '@/arkham/helpers';
import type { Game } from '@/arkham/types/Game';
import * as ArkhamGame from '@/arkham/types/Game';
import type { AbilityLabel, AbilityMessage, Message } from '@/arkham/types/Message';
import PoolItem from '@/arkham/components/PoolItem.vue';
import AbilityButton from '@/arkham/components/AbilityButton.vue'
import * as Arkham from '@/arkham/types/Treachery';
import { setUncaughtExceptionCaptureCallback } from 'process';

export interface Props {
  game: Game
  treachery: Arkham.Treachery
  investigatorId: string
  attached?: boolean
}

const props = withDefaults(defineProps<Props>(), { attached: false })

const image = computed(() => {
  return imgsrc(`cards/${props.treachery.cardCode.replace('c', '')}.jpg`)
})
const id = computed(() => props.treachery.id)
const choices = computed(() => ArkhamGame.choices(props.game, props.investigatorId))

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
        return [...acc, { contents: v, index: i }];
      }

      return acc;
    }, []);
})

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
        v-if="treachery.horror && treachery.horror > 0"
        type="horror"
        :amount="treachery.horror"
      />
      <PoolItem
        v-if="treachery.clues && treachery.clues > 0"
        type="clue"
        :amount="treachery.clues"
      />
      <PoolItem
        v-if="treachery.resources && treachery.resources > 0"
        type="resource"
        :amount="treachery.resources"
      />
      <PoolItem
        v-if="treachery.doom && treachery.doom > 0"
        type="doom"
        :amount="treachery.doom"
      />
    </div>
  </div>
</template>


<style lang="scss" scoped>
.card {
  width: $card-width;
  max-width: $card-width;
  border-radius: 5px;
}

.treachery--can-interact {
  border: 2px solid $select;
  cursor:pointer;
}

.treachery {
  display: flex;
  flex-direction: column;
}

.attached .card {
  object-fit: cover;
  object-position: left bottom;
  height: $card-width*0.6;
}
</style>
