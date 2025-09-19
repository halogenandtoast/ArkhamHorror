<script lang="ts" setup>
import { computed } from 'vue';
import { type Investigator } from '@/arkham/types/Investigator'
import { useI18n } from 'vue-i18n';

const props = defineProps<{
  player: Investigator
}>()

const { t } = useI18n()

const supplies = computed(() =>
  props.player.supplies.reduce((acc, supply) => acc.set(supply, (acc.get(supply) || 0) + 1), new Map<string, number>())
)

const supplyLabel = (supply: string) => t(`theForgottenAge.supplies.${supply.charAt(0).toLowerCase() + supply.slice(1)}.name`)
</script>

<template>
  <div v-if="supplies.size > 0" class="supplies box">
    <slot name="heading" />
    <ul>
      <li v-for="[supply, count] in supplies.entries()" :key="supply">
        {{supplyLabel(supply)}} ({{count}})
      </li>
    </ul>
  </div>
</template>

<style scoped lang="scss">
.supplies {
  padding: 10px 20px;
  flex: 1;
}

ul {
  list-style-position: inside;
}

</style>
