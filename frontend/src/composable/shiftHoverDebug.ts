import { computed, onMounted, onUnmounted, ref } from 'vue'

/* Hold Shift over a panel and a Debug button shows in its corner; clicking it
 * turns that panel's controls on until clicked again. Independent of the
 * board's debug mode. */
export function useShiftHoverDebug() {
  const active = ref(false)
  const hovering = ref(false)
  const shiftHeld = ref(false)

  const showToggle = computed(() => active.value || (hovering.value && shiftHeld.value))

  const onKey = (event: KeyboardEvent) => {
    if (event.key === 'Shift') shiftHeld.value = event.type === 'keydown'
  }

  // A Shift released while another window has focus never sends its keyup here.
  const onBlur = () => { shiftHeld.value = false }

  onMounted(() => {
    window.addEventListener('keydown', onKey)
    window.addEventListener('keyup', onKey)
    window.addEventListener('blur', onBlur)
  })

  onUnmounted(() => {
    window.removeEventListener('keydown', onKey)
    window.removeEventListener('keyup', onKey)
    window.removeEventListener('blur', onBlur)
  })

  // Shift may already be down when the pointer arrives (pressed before the page had focus).
  const onEnter = (event: MouseEvent) => {
    hovering.value = true
    if (event.shiftKey) shiftHeld.value = true
  }

  const onLeave = () => { hovering.value = false }

  const toggle = () => { active.value = !active.value }

  return { active, showToggle, onEnter, onLeave, toggle }
}
