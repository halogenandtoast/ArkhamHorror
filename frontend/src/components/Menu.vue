<script setup>
import { Menu, MenuButton, MenuItem, MenuItems } from '@headlessui/vue'
import { ChevronDownIcon } from '@heroicons/vue/20/solid'
</script>

<template>
  <Menu as="div">
    <MenuButton as="button">
      <slot></slot>
      <ChevronDownIcon aria-hidden="true" />
    </MenuButton>
    <transition
            enter-active-class="transition duration-100 ease-out"
            enter-from-class="transform scale-95 opacity-0"
            enter-to-class="transform scale-100 opacity-100"
            leave-active-class="transition duration-75 ease-in"
            leave-from-class="transform scale-100 opacity-100"
            leave-to-class="transform scale-95 opacity-0"
          >
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
</style>
