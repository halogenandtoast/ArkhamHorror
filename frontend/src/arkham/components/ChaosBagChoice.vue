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
    case 'ChooseMatchChoice': return props.choice.steps
    case 'Choose': return props.choice.steps
    default: return [props.choice]
  }
})

const tokenGroups = computed(() => {
  switch (props.choice.tag) {
    case 'ChooseMatch': return props.choice.tokenGroups.flat()
    case 'ChooseMatchChoice': return props.choice.tokenGroups.flat()
    case 'Choose': return props.choice.tokenGroups.flat()
    default: return []
  }
})

const count = computed(() => {
  switch (props.choice.tag) {
    case 'ChooseMatch': return props.choice.amount
    case 'Choose': return props.choice.amount
    default: return null
  }
})

const isCancel = computed(() => {
  switch (props.choice.tag) {
    case 'ChooseMatch': return props.choice.tokenStrategy == "CancelChoice"
    case 'Choose': return props.choice.tokenStrategy == "CancelChoice"
    default: return false
  }
})

const isIgnore = computed(() => {
  switch (props.choice.tag) {
    case 'ChooseMatch': return props.choice.tokenStrategy == "IgnoreChoice"
    case 'Choose': return props.choice.tokenStrategy == "IgnoreChoice"
    default: return false
  }
})

const isResolve = computed(() => {
  switch (props.choice.tag) {
    case 'ChooseMatch': return props.choice.tokenStrategy == "ResolveChoice"
    case 'Choose': return props.choice.tokenStrategy == "ResolveChoice"
    default: return false
  }
})

const choose = (idx: number) => emit('choose', idx)

const allResolved = computed(() => {
  switch (props.choice.tag) {
    case 'ChooseMatch': return props.choice.steps.every((step) => step.tag === "Resolved")
    case 'Choose': return props.choice.steps.every((step) => step.tag === "Resolved")
    default: return false
  }
})

function pluralize(w, n) {
  return `${n} ${w}${n == 1 ? '' : 's'}`
}

</script>

<template>
  <div class="token-choices">

    <div class="token-prompt" v-if="isCancel && allResolved">
      Choose {{pluralize("token", count)}} to cancel
    </div>
    <div class="token-prompt" v-else-if="isIgnore && allResolved">
      Choose {{pluralize("token", count)}} to ignore
    </div>
    <div class="token-prompt" v-else-if="isResolve && allResolved">
      Choose {{pluralize("token", count)}} to resolve
    </div>

    <div class="token-choices-inner">
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
      <template v-if="allResolved">
        <div v-for="(token, idx) in tokenGroups" :key="idx">
          <Token :key="idx" :token="token" :game="game" :investigatorId="investigatorId" @choose="choose" :cancelled="isCancel || isIgnore" :selected="isResolve" />
        </div>
      </template>
    </div>
  </div>
</template>

<style lang="scss" scoped>
.token-choices {
  margin: 10px;
  background: rgba(0, 0, 0, 0.2);
  border-radius: 20px;
  display: flex;
  flex-direction: column;
  .token {
    width: 150px;
  }
  border: 1px solid rgba(255, 255, 255, 0.2);
  flex-grow: 0;
}

.deciding {
  border-radius: 50%;
  border: 4px solid green;
}

.token-choices-inner {
  display: flex;
  flex-direction: row;
  align-self: center;
  padding: 10px;
}

.token-prompt {
  color: #EEE;
  width: 100%;
  text-align: center;
  background: rgba(255, 255, 255, 0.2);
  border-top-left-radius: 20px;
  border-top-right-radius: 20px;
  font-size: 1.2em;
}
</style>
