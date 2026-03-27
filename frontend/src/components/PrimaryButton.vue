<script lang="ts" setup>
defineProps<{ label: string }>()
defineEmits(['click'])
</script>

<template>
  <button class="primary-btn" @click="$emit('click')">{{ label }}</button>
</template>

<style scoped>
.primary-btn {
  height: fit-content;
  align-self: center;
  background-color: var(--button-1);
  border-radius: 3px;
  outline: 0;
  padding: 10px 15px;
  text-transform: uppercase;
  color: white;
  border: 0;
  font-size: 1em;
  font-weight: bolder;
  cursor: pointer;
  transition: background-color 0.3s linear;

  &:hover {
    background: hsl(80, 35%, 32%);
  }
}
</style>
