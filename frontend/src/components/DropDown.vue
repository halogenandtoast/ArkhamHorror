<script setup lang="ts">
import { ref } from 'vue';
interface Props {
  options: string[]
}

const choice = ref(null)

defineProps<Props>()
const emit = defineEmits(['choose'])

const submit = function() {
  const option = choice.value
  if (option !== null) {
    emit('choose', option)
  }
}
</script>

<template>
  <form @submit.prevent="submit">
    <select v-model="choice">
      <option value="Choose One" disabled>Choose One</option>
      <option v-for="(option, idx) in options" :value="idx" :key="idx">{{option}}</option>
    </select>
    <button type="submit">Choose</button>
  </form>
</template>
