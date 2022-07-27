<script lang="ts" setup>
import { computed, inject } from 'vue';
import { ChaosBagStep } from '@/arkham/types/ChaosBag';

export interface Props {
  choice: ChaosBagStep
}

const baseUrl = inject('baseUrl')

const props = defineProps<Props>()

const tokenChoices = computed(() => {
  switch (props.choice.contents?.tag) {
    case 'ChooseMatch': return props.choice.contents.contents[1]
    case 'Choose': return props.choice.contents.contents[1]
    default: return [props.choice]
  }
})

function imageFor(tokenFace: string) {
  switch (tokenFace) {
    case 'PlusOne':
      return `${baseUrl}/img/arkham/ct_plus1.png`;
    case 'Zero':
      return `${baseUrl}/img/arkham/ct_0.png`;
    case 'MinusOne':
      return `${baseUrl}/img/arkham/ct_minus1.png`;
    case 'MinusTwo':
      return `${baseUrl}/img/arkham/ct_minus2.png`;
    case 'MinusThree':
      return `${baseUrl}/img/arkham/ct_minus3.png`;
    case 'MinusFour':
      return `${baseUrl}/img/arkham/ct_minus4.png`;
    case 'MinusFive':
      return `${baseUrl}/img/arkham/ct_minus5.png`;
    case 'MinusSix':
      return `${baseUrl}/img/arkham/ct_minus6.png`;
    case 'MinusSeven':
      return `${baseUrl}/img/arkham/ct_minus7.png`;
    case 'MinusEight':
      return `${baseUrl}/img/arkham/ct_minus8.png`;
    case 'AutoFail':
      return `${baseUrl}/img/arkham/ct_autofail.png`;
    case 'ElderSign':
      return `${baseUrl}/img/arkham/ct_eldersign.png`;
    case 'Skull':
      return `${baseUrl}/img/arkham/ct_skull.png`;
    case 'Cultist':
      return `${baseUrl}/img/arkham/ct_cultist.png`;
    case 'Tablet':
      return `${baseUrl}/img/arkham/ct_tablet.png`;
    case 'ElderThing':
      return `${baseUrl}/img/arkham/ct_elderthing.png`;
    default:
      return `${baseUrl}/img/arkham/ct_blank.png`;
  }
}
</script>

<template>
  <div class="token-choices">
    <div v-for="(tokenChoice, idx) in tokenChoices" :key="idx">
      <template v-if="tokenChoice.tag =='Resolved'">
        <img v-for="(token, idx) in tokenChoice.contents" :key="idx"
          class="token"
          :src="imageFor(token.tokenFace)"
        />
      </template>
      <template v-else-if="tokenChoice.contents.tag =='Draw'">
        <img :src="`${baseUrl}/img/arkham/ct_blank.png`" class="token" v-if="tokenChoice.tag == 'Decided'" />
        <img :src="`${baseUrl}/img/arkham/ct_blank.png`" class="token deciding" v-if="tokenChoice.tag == 'Deciding'" />
        <img :src="`${baseUrl}/img/arkham/ct_choose.png`" class="token" v-if="tokenChoice.tag == 'Undecided'" />
      </template>
      <template v-else>
        <ChaosBagChoice :choice="tokenChoice" />
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
