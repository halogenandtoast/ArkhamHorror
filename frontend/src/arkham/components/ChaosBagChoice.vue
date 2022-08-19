<script lang="ts" setup>
import { computed, inject } from 'vue';
import { Game } from '@/arkham/types/Game';
import { ChaosBagStep } from '@/arkham/types/ChaosBag';
import Token from '@/arkham/components/Token';

export interface Props {
  game: Game
  investigatorId: string
  choice: ChaosBagStep
}

const baseUrl = inject('baseUrl')

const props = defineProps<Props>()
const emit = defineEmits(['choose'])

const tokenChoices = computed(() => {
  switch (props.choice.tag) {
    case 'ChooseMatch': return props.choice.steps
    case 'Choose': return props.choice.steps
    default: return [props.choice]
  }
})

const choose = (idx: number) => emit('choose', idx)

</script>

<template>
  <div class="token-choices">
    <div v-for="(tokenChoice, idx) in tokenChoices" :key="idx">
      <template v-if="tokenChoice.tag ==='Resolved'">
        <Token v-for="(token, idx) in tokenChoice.tokens" :key="idx" :token="token" :game="game" :investigatorId="investigatorId" @choose="choose" />
      </template>
      <template v-else-if="tokenChoice.step && tokenChoice.step.tag === 'Draw'">
        <img :src="`${baseUrl}/img/arkham/ct_blank.png`" class="token" v-if="tokenChoice.tag === 'Decided'" />
        <img :src="`${baseUrl}/img/arkham/ct_blank.png`" class="token deciding" v-if="tokenChoice.tag === 'Deciding'" />
        <img :src="`${baseUrl}/img/arkham/ct_choose.png`" class="token" v-if="tokenChoice.tag === 'Undecided'" />
      </template>
      <template v-else-if="tokenChoice.step">
        <ChaosBagChoice :choice="tokenChoice.step" :game="game" :investigatorId="investigatorId" @choose="choose" />
      </template>
      <template v-else>
        <div class="error"> Token choice was unhandled, please report with: {{tokenChoice}}</div>
      </template>
    </div>
  </div>
</template>

<style lang="scss" scoped>
.token-choices {
  margin: 10px;
  padding: 10px;
  background: rgba(0, 0, 0, 0.2);
  border-radius: 20px;
  display: flex;
  flex-direction: row;
  .token {
    width: 150px;
  }
  border: 1px solid #000;
}

.deciding {
  border-radius: 50%;
  border: 4px solid green;
}
</style>
