<script setup lang="ts">
import { computed, onMounted, ref, watch } from 'vue'
import { useI18n } from 'vue-i18n'
import { fetchCustomCards, updateGameRaw } from '@/arkham/api'
import type { CustomCard } from '@/arkham/customCards'
import type { Game } from '@/arkham/types/Game'

const props = defineProps<{ game: Game; playerId: string }>()
const emit = defineEmits<{ saving: [save: Promise<boolean>] }>()
const playerPool = computed(
  () =>
    props.game.settings.settingsCustomRandomWeaknessPools[props.playerId] ??
    ([false, []] as [boolean, string[]]),
)
const { t } = useI18n()
const customWeaknessesEnabled = ref(playerPool.value[0])
const customWeaknessSets = ref([...playerPool.value[1]])
const customWeaknessCards = ref<CustomCard[]>([])
const customWeaknessSaving = ref(false)
const customWeaknessError = ref('')
const customWeaknessSetOptions = computed(() => {
  const counts = new Map<string, number>()
  for (const { def } of customWeaknessCards.value) {
    if (def.cardSubType !== 'BasicWeakness') continue
    const name = typeof def.meta.set === 'string' ? def.meta.set : ''
    counts.set(name, (counts.get(name) ?? 0) + 1)
  }
  for (const name of customWeaknessSets.value) if (!counts.has(name)) counts.set(name, 0)
  return [...counts]
    .sort(([a], [b]) => a.localeCompare(b))
    .map(([name, count]) => ({ name, count }))
})
watch(
  () => [playerPool.value[0], playerPool.value[1]] as const,
  ([enabled, sets]) => {
    customWeaknessError.value = ''
    customWeaknessesEnabled.value = enabled
    customWeaknessSets.value = [...sets]
  },
)
let pendingSave: Promise<boolean> = Promise.resolve(true)
function saveCustomWeaknessPool(enabled: boolean, sets: string[]) {
  pendingSave = persistCustomWeaknessPool(enabled, sets)
  emit('saving', pendingSave)
  return pendingSave
}
async function persistCustomWeaknessPool(enabled: boolean, sets: string[]) {
  customWeaknessesEnabled.value = enabled
  customWeaknessSets.value = sets
  customWeaknessSaving.value = true
  customWeaknessError.value = ''
  try {
    await updateGameRaw(props.game.id, {
      tag: 'SetPlayerCustomRandomWeaknessPool',
      contents: [props.playerId, enabled, sets],
    })
    return true
  } catch {
    customWeaknessError.value = t('gameBar.settings.customWeaknessSaveFailed')
    customWeaknessesEnabled.value = playerPool.value[0]
    customWeaknessSets.value = [...playerPool.value[1]]
    return false
  } finally {
    customWeaknessSaving.value = false
  }
}
function toggleCustomWeaknessSet(name: string) {
  const selected = customWeaknessSets.value.includes(name)
    ? customWeaknessSets.value.filter((s) => s !== name)
    : [...customWeaknessSets.value, name]
  void saveCustomWeaknessPool(customWeaknessesEnabled.value, selected)
}
onMounted(async () => {
  try {
    customWeaknessCards.value = await fetchCustomCards(props.game.id)
  } catch {
    customWeaknessError.value = t('gameBar.settings.customWeaknessLoadFailed')
  }
})

// Deck submission must wait until the player’s pool has been saved.
defineExpose({ waitForSave: () => pendingSave })
</script>

<template>
  <div class="custom-weakness-pool">
    <div class="toggle-row">
      <div class="toggle-text">
        <div class="toggle-name">{{ t('gameBar.settings.customWeaknessTitle') }}</div>
      </div>
      <input
        type="checkbox"
        :aria-label="t('gameBar.settings.customWeaknessTitle')"
        :checked="customWeaknessesEnabled"
        :disabled="customWeaknessSaving"
        @change="saveCustomWeaknessPool(!customWeaknessesEnabled, customWeaknessSets)"
      />
    </div>
    <fieldset
      v-if="customWeaknessesEnabled"
      class="custom-weakness-sets"
      :disabled="customWeaknessSaving"
    >
      <legend>{{ t('gameBar.settings.customWeaknessSets') }}</legend>
      <label v-for="set in customWeaknessSetOptions" :key="set.name">
        <input
          type="checkbox"
          :checked="customWeaknessSets.includes(set.name)"
          @change="toggleCustomWeaknessSet(set.name)"
        />
        <span>{{ set.name || t('gameBar.settings.customWeaknessUnsorted') }}</span
        ><small>{{ set.count }}</small>
      </label>
      <p v-if="!customWeaknessSetOptions.length">{{ t('gameBar.settings.customWeaknessEmpty') }}</p>
      <p v-else-if="!customWeaknessSets.length">{{ t('gameBar.settings.customWeaknessSelect') }}</p>
    </fieldset>
    <p v-if="customWeaknessError" role="alert">{{ customWeaknessError }}</p>
  </div>
</template>

<style scoped>
.custom-weakness-pool { color: var(--weakness-pool-text, inherit); }
.custom-weakness-sets {
  margin: 0 0 12px;
  padding: 12px;
  border: 1px solid var(--weakness-pool-border, var(--box-border));
  label {
    display: flex;
    align-items: center;
    gap: 8px;
    padding: 6px 0;
    cursor: pointer;
  }
  small {
    margin-left: auto;
    opacity: 0.6;
  }
  p {
    margin: 8px 0 0;
    font-size: 0.8rem;
    opacity: 0.7;
  }
}

.toggle-row {
  display: grid;
  grid-template-columns: 1fr auto;
  gap: 16px;
  align-items: center;
  padding: 10px 14px;
  background: var(--weakness-pool-surface, var(--box-background));
  border: 1px solid var(--weakness-pool-border, var(--box-border));
  border-radius: 5px;
}
.toggle-text {
  min-width: 0;
}
.toggle-name {
  font-size: 14px;
  font-weight: 500;
  color: var(--weakness-pool-text, var(--text));
}
</style>
