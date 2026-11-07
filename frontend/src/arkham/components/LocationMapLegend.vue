<script lang="ts" setup>
import { useI18n } from 'vue-i18n'
import { useStorage } from '@vueuse/core'
import GameIcon from '@/arkham/components/GameIcon.vue'

/* What the symbols on the location plates mean. Open the first time the map is seen, then remembered. */
const { t } = useI18n()
const open = useStorage('arkham-map-legend', true)

const ENTRIES: [string, string][] = [
  ['●', 'revealed'], ['?', 'unrevealed'], ['★', 'here'], ['✦', 'targetable'], ['↔', 'move'],
  ['⚠', 'onEnter'], ['🔍', 'shroud'], ['clue', 'clues'], ['doom', 'doom'], ['ⓘ', 'details'],
]
</script>

<template>
  <span class="legend">
    <button class="view" type="button" :class="{ active: open }" :aria-expanded="open" @click.stop="open = !open">? {{ t('locationMap.legend.title') }}</button>
    <span v-if="open" class="panel" role="note" @click.stop>
      <span v-for="[icon, key] in ENTRIES" :key="key" class="row"><b aria-hidden="true"><GameIcon v-if="icon === 'clue' || icon === 'doom'" :type="icon" /><template v-else>{{ icon }}</template></b>{{ t(`locationMap.legend.${key}`) }}</span>
    </span>
  </span>
</template>

<style scoped>
.legend { position: relative; }
.view { padding: 3px 12px; font-size: 0.75rem; letter-spacing: 0.08em; text-transform: uppercase; background: transparent; border: 1px solid #75836b66; border-radius: 4px; color: #b4bdab; cursor: pointer; }
.view:hover { border-color: #e6c584; }
.view.active { border-color: #c6a873; color: #f0dfb5; }
.panel { position: absolute; top: calc(100% + 6px); right: 0; z-index: 90; width: 270px; display: grid; gap: 3px; padding: 10px 12px; background: #101913f5; border: 1px solid #c6a873; border-radius: 8px; box-shadow: 0 10px 30px #000c; font-size: 0.78rem; color: #e9e3d0; }
.row { display: flex; gap: 8px; align-items: baseline; }
.row b { width: 18px; text-align: center; color: #e6c584; }
</style>
