<script lang="ts" setup>
import { computed } from 'vue';
import type { Game } from '@/arkham/types/Game';
import * as ArkhamGame from '@/arkham/types/Game';
import { MessageType } from '@/arkham/types/Message';
import StoryEntry from '@/arkham/components/StoryEntry.vue';

export interface Props {
  game: Game
  investigatorId: string
}

const props = defineProps<Props>()
const emit = defineEmits(['choose'])
const choices = computed(() => ArkhamGame.choices(props.game, props.investigatorId))

const choose = (idx: number) => emit('choose', idx)
</script>

<template>
  <template v-for="(choice, index) in choices" :key="index">
    <template
      v-if="choice.tag === MessageType.RUN
        && (choice.contents[0] && choice.contents[0].tag === MessageType.CONTINUE)"
      >
      <StoryEntry
        v-if="choice.contents[1].tag === MessageType.FLAVOR_TEXT"
        :choice="choice"
        :index="index"
        @choose="choose"
      />
    </template>
  </template>
</template>
