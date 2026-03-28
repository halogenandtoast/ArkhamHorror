<script lang="ts" setup>
withDefaults(defineProps<{ label: string; danger?: boolean }>(), { danger: false })
defineEmits(['click'])
</script>

<template>
  <button class="primary-btn" :class="{ danger }" @click="$emit('click')">{{ label }}</button>
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
  @media (max-width: 768px) {
    padding: 6px 10px;
    font-size: 0.82em;
  }

  &:hover {
    background: hsl(80, 35%, 32%);
  }

  &.danger {
    background-color: #c0392b;
    &:hover { background-color: #a93226; }
  }
}
</style>
