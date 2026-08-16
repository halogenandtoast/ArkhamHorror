import { onBeforeUnmount, onMounted, ref } from 'vue'

const KEY = 'arkhamSoundsDisabled'

/**
 * The global "Sounds" preference, kept live.
 *
 * Settings.vue writes it to localStorage and announces the change on the
 * `arkham-setting-change` window event; Game.vue reads it the same way. This
 * wraps that pattern so a third reader does not have to repeat the wiring.
 */
export function useSoundsDisabled() {
  const soundsDisabled = ref(localStorage.getItem(KEY) === 'true')

  function onSettingChange(event: Event) {
    const detail = (event as CustomEvent<{ key?: string; value?: string }>).detail
    if (detail?.key === KEY) soundsDisabled.value = detail.value === 'true'
  }

  onMounted(() => window.addEventListener('arkham-setting-change', onSettingChange))
  onBeforeUnmount(() => window.removeEventListener('arkham-setting-change', onSettingChange))

  return { soundsDisabled }
}
