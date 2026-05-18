import { ref, onMounted, onUnmounted } from 'vue'
import useEmitter from './useEmitter'

export default function useHighlighter() {
  const emitter = useEmitter()
  const highlighted = ref<string | null>(null)
  let observer: MutationObserver | null = null
  let currentElement: HTMLElement | null = null

  function updateHighlight(id: string | null, el?: HTMLElement | null) {
    if (highlighted.value === id) return

    highlighted.value = id
    currentElement = el ?? null
    emitter.emit('highlight', id)

    // Reattach observer whenever the highlighted element changes
    if (observer) observer.disconnect()
    if (currentElement) {
      observer = new MutationObserver((mutations) => {
        for (const m of mutations) {
          for (const removed of m.removedNodes) {
            if (removed === currentElement || removed.contains?.(currentElement)) {
              updateHighlight(null)
              return
            }
          }
        }
      })
      observer.observe(document.body, {
        childList: true,
        subtree: true,
      })
    }
  }

  // Keep local state in sync across components
  function handleExternalHighlight(id: string | null) {
    highlighted.value = id
  }

  // Pointer move sets current highlight
  function handlePointerMove(e: PointerEvent) {
    const el = (e.target as HTMLElement)?.closest('[data-highlight-id]')
    const id = el?.getAttribute('data-highlight-id') ?? null
    updateHighlight(id, el)
  }

  onMounted(() => {
    emitter.on('highlight', handleExternalHighlight)
    window.addEventListener('pointermove', handlePointerMove, { passive: true })
  })

  onUnmounted(() => {
    emitter.off('highlight', handleExternalHighlight)
    window.removeEventListener('pointermove', handlePointerMove)
    observer?.disconnect()
    observer = null
  })

  return { highlighted }
}
