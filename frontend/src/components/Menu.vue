<script setup lang="ts">
import { Menu, MenuButton, MenuItems } from '@headlessui/vue'
import { ChevronDownIcon } from '@heroicons/vue/20/solid'
</script>

<template>
  <Menu as="div">
    <MenuButton as="button">
      <slot></slot>
      <ChevronDownIcon aria-hidden="true" />
    </MenuButton>
    <transition name="appear" >
      <MenuItems class="menu-items">
        <slot name="items"></slot>
      </MenuItems>
    </transition>
  </Menu>
</template>

<style scoped>
.menu-items {
  background-color: var(--background-mid);
  border-bottom-left-radius: 5px;
  border-bottom-right-radius: 5px;
  position: absolute;
  z-index: 10;
  display: flex;
  flex-direction: column;
}

button {
  background: none;
  border: 0;
  display: flex;
  padding: 5px 10px;
  gap: 5px;
  align-items: center;
  &:hover {
    background: rgba(0,0,0,0.4);
  }
}

svg {
  width: 20px;
}

.appear-enter-active, .appear-leave-active {
  transition: opacity 0.2s ease-in-out;
}

.appear-enter-from, .appear-leave-to {
  opacity: 0;
}

.appear-enter-to, .appear-leave-from {
  opacity: 1;
}
</style>
