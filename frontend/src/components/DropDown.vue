<script setup lang="ts">
import { ref } from 'vue';
import { handleI18n } from '@/arkham/i18n';
import { useI18n } from 'vue-i18n';

const { t } = useI18n()

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

const formatOption = function(option: string): string {
  return option.startsWith("$") ? handleI18n(option, t) : option
}
</script>

<template>
  <form @submit.prevent="submit">
    <select v-model="choice">
      <option value="Choose One" disabled>Choose One</option>
      <option v-for="(option, idx) in options" :value="idx" :key="idx">{{formatOption(option)}}</option>
    </select>
    <button type="submit">Choose</button>
  </form>
</template>
