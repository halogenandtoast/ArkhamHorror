import { ref } from 'vue'

const focusedSettingId = ref<string | null>(null)

export function useSettingsFocus() {
  return {
    focusedSettingId,
    focusSetting: (id: string) => {
      focusedSettingId.value = id
    },
    clearFocus: () => {
      focusedSettingId.value = null
    },
  }
}
