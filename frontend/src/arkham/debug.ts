import { reactive } from 'vue'
import { updateGameRaw } from '@/arkham/api'

const debug = reactive({
  active: false,
  toggle: () => {
    debug.active = !debug.active
  },
  send: (gameId: string, message: any) => updateGameRaw(gameId, message)
})

export function useDebug() {
  return debug
}
