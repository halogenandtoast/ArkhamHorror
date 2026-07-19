interface CardPosition {
  element?: HTMLElement
  rect: DOMRect
  recordedAt: number
}

export function createCardMovementTracker() {
  const positions = new Map<string, CardPosition>()

  function register(id: string, element: HTMLElement) {
    const previous = positions.get(id)
    const destination = element.getBoundingClientRect()

    positions.set(id, {
      element,
      rect: destination,
      recordedAt: performance.now(),
    })

    if (!previous || previous.element === element || performance.now() - previous.recordedAt > 1_000) return
    if (window.matchMedia('(prefers-reduced-motion: reduce)').matches) return

    const origin = previous.element?.isConnected
      ? previous.element.getBoundingClientRect()
      : previous.rect
    const deltaX = origin.left - destination.left
    const deltaY = origin.top - destination.top
    if (Math.abs(deltaX) < 1 && Math.abs(deltaY) < 1) return

    element.animate(
      [
        {
          transform: `translate(${deltaX}px, ${deltaY}px) scale(1.04)`,
          zIndex: 100,
          filter: 'drop-shadow(0 8px 12px rgba(0, 0, 0, 0.45))',
        },
        {
          transform: 'translate(0, 0) scale(1)',
          zIndex: 100,
          filter: 'drop-shadow(0 2px 3px rgba(0, 0, 0, 0.2))',
        },
      ],
      {
        duration: 420,
        easing: 'cubic-bezier(0.22, 1, 0.36, 1)',
      },
    )
  }

  function update(id: string, element: HTMLElement) {
    const current = positions.get(id)
    if (current?.element !== element) return
    positions.set(id, {
      element,
      rect: element.getBoundingClientRect(),
      recordedAt: performance.now(),
    })
  }

  function unregister(id: string, element: HTMLElement) {
    const current = positions.get(id)
    if (current?.element !== element) return
    positions.set(id, {
      rect: element.getBoundingClientRect(),
      recordedAt: performance.now(),
    })
  }

  return { register, update, unregister }
}

export const investigatorMovement = createCardMovementTracker()
export const enemyMovement = createCardMovementTracker()
