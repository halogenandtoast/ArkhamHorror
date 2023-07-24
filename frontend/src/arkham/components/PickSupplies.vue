<script lang="ts" setup>
import { computed } from 'vue';
import { PickSupplies  } from '@/arkham/types/Question';
import { MessageType } from '@/arkham/types/Message';

const props = defineProps<{
  question: PickSupplies
}>()
const emit = defineEmits(['choose'])
const choose = (idx: number) => emit('choose', idx)

const pointsRemaining = computed(() => props.question.pointsRemaining)
const supplies = computed(() => props.question.choices.slice(1))
const chosenSupplies = computed(() => {
 return props.question.chosenSupplies.reduce((acc, e) => acc.set(e, (acc.get(e) || 0) + 1), new Map());
})

</script>
<template>
  <div class="pick-supplies">
    <h2>Pick Supplies ({{pointsRemaining}} points remaining)</h2>

    <div class="supply-choices">
      <template v-for="(choice, index) in supplies" :key="index">
        <template v-if="choice.tag === MessageType.TOOLTIP_LABEL">
          <button @click="choose(index + 1)" v-tooltip="choice.tooltip">{{choice.label}}</button>
        </template>
      </template>
    </div>

    <button class="done-button" @click="choose(0)">Done</button>
  </div>

  <div class="pick-supplies" v-if="chosenSupplies.size > 0">
    <h2>Chosen Supplies</h2>

    <ul>
      <template v-for="([supply, count], index) in chosenSupplies" :key="index">
        <li v-if="count > 1">{{supply}} ({{count}})</li>
        <li v-else>{{supply}}</li>
      </template>
    </ul>
  </div>
</template>

<style lang="scss" scoped>
.supply-choices {
  display: grid;
  grid-template-columns: repeat(4, minmax(150px, 1fr));
  row-gap: 20px;
}

button {
  border: 0;
  margin: 0 10px;
  padding: 10px;
  text-transform: uppercase;
  background-color: #532e61;
  font-weight: bold;
  border-radius: 0.6em;
  color: #EEE;
  font: Arial, sans-serif;
  &:hover {
    background-color: #311b3e;
  }
}

.pick-supplies {
  background: #DCD6D0;
  box-shadow: inset 0 0 170px rgba(0,0,0,0.5), 1px 1px 3px rgba(0,0,0,0.6);
  width: calc(100% - 20px);
  margin: 0 auto;
  margin-top: 20px;
  box-sizing: border-box;
  p {
    margin: 0;
    padding: 0;
  }
}

.done-button {
  margin: 0;
  width: 100%;
  border: 0;
  text-align: center;
  padding: 10px;
  box-sizing: border-box;
  text-transform: uppercase;
  border-radius: 0;
  background-color: #532e61;
  font-weight: bold;
  color: #EEE;
  font: Arial, sans-serif;
  margin-top: 20px;
  &:hover {
    background-color: #311b3e;
  }

  i {
    font-style: normal;
  }
}

h2 {
  text-align: center;
  text-transform: uppercase;
}

ul {
  list-style: none;
  margin: 0;
  padding: 0;
  margin-bottom: 10px;
}

ul li {
  text-transform: uppercase;
  background: rgba(0, 0, 0, 0.3);
  padding: 10px;
  margin: 0 10px;
  margin-bottom: 5px;
  border-radius: 5px;
  font-weight: 600;
}
</style>
