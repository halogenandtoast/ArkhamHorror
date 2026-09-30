<script setup lang="ts">
/* Card copy, with the skills it names drawn as their icons: "test lore" prints
the lore symbol, while "you will suffer" keeps its verb. */
import { computed } from 'vue'
import Icon from '@/game/Icon.vue'
import { skillParts } from '@/game/util'

const props = defineProps<{ text: string }>()
const parts = computed(() => skillParts(props.text))
</script>

<template>
  <span
    ><template v-for="(p, i) in parts" :key="i"
      ><Icon v-if="'icon' in p" :name="p.icon" :title="p.word" /><template v-else>{{ p.text }}</template></template
    ></span
  >
</template>
