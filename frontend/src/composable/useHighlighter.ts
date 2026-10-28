import { ref, onMounted, onUnmounted } from 'vue'
import useEmitter from './useEmitter'

export default function useHighlighter() {
  const emitter = useEmitter()
  const highlighted = ref<string | null>(null)
  const sourceHighlighted = ref<string | null>(null)
  let observer: MutationObserver | null = null
  let currentElement: HTMLElement | null = null

  function updateHighlight(id: string | null, el?: HTMLElement | null, sourceId: string | null = null) {
    if (highlighted.value === id && sourceHighlighted.value === sourceId) return

    highlighted.value = id
    sourceHighlighted.value = sourceId
    currentElement = el ?? null
    emitter.emit('highlight', id)
    emitter.emit('source-highlight', sourceId)

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

  function handleExternalSourceHighlight(id: string | null) {
    sourceHighlighted.value = id
  }

  // Pointer move sets current highlight
  function handlePointerMove(e: PointerEvent) {
    const el = (e.target as HTMLElement)?.closest('[data-highlight-id], [data-source-highlight-id]')
    const id = el?.getAttribute('data-highlight-id') ?? null
    const sourceId = el?.getAttribute('data-source-highlight-id') ?? null
    updateHighlight(id, el instanceof HTMLElement ? el : null, sourceId)
  }

  onMounted(() => {
    emitter.on('highlight', handleExternalHighlight)
    emitter.on('source-highlight', handleExternalSourceHighlight)
    window.addEventListener('pointermove', handlePointerMove, { passive: true })
  })

  onUnmounted(() => {
    emitter.off('highlight', handleExternalHighlight)
    emitter.off('source-highlight', handleExternalSourceHighlight)
    window.removeEventListener('pointermove', handlePointerMove)
    observer?.disconnect()
    observer = null
  })

  return { highlighted, sourceHighlighted }
}
