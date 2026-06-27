import { defineStore } from 'pinia'
import { ref } from 'vue'
import { useWebSocket } from '@vueuse/core'
import { useUserStore } from '@/stores/user'
import * as Api from '@/arkham/api'
import {
  EventDetails,
  GroupDigest,
  SharedEventState,
  emptySharedState,
} from '@/arkham/types/EpicEvent'

// Store for the "Epic Multiplayer" event aggregate.
//
// Holds the live shared state + group digests for the organizer dashboard and
// the in-game organizer bar. Game.vue forwards `SharedStateUpdate` messages off a
// group's own game websocket into `applySharedState`, so the shared counters stay
// live while viewing/playing a group.
export const useEventStore = defineStore('event', () => {
  const eventId = ref<string | null>(null)
  const event = ref<EventDetails | null>(null)
  const sharedState = ref<SharedEventState>(emptySharedState())
  const groupDigests = ref<GroupDigest[]>([])
  // Flips true once any shared state has been seen (load or ws). Lets the
  // in-game panel stay hidden in ordinary, non-event games.
  const received = ref(false)
  const connected = ref(false)
  const socketError = ref(false)

  let closeFn: (() => void) | null = null

  // Apply a shared-state blob, guarding against out-of-order websocket delivery
  // via the monotonic sharedVersion.
  function applySharedState(next: SharedEventState) {
    if (received.value && next.sharedVersion < sharedState.value.sharedVersion) return
    sharedState.value = next
    received.value = true
  }

  function setEvent(details: EventDetails) {
    eventId.value = details.id
    event.value = details
    groupDigests.value = details.groups
    applySharedState(details.sharedState)
  }

  async function load(id: string): Promise<EventDetails> {
    const details = await Api.fetchEvent(id)
    setEvent(details)
    return details
  }

  // Connect (or reconnect) the event websocket for the organizer dashboard.
  function connect(id: string) {
    if (closeFn && eventId.value === id) return
    disconnect()
    eventId.value = id

    const userStore = useUserStore()
    const url = Api.eventWebsocketUrl(id, userStore.token)
    const { close } = useWebSocket(url, {
      autoReconnect: true,
      onConnected() {
        connected.value = true
        socketError.value = false
      },
      onDisconnected() {
        connected.value = false
      },
      onError() {
        socketError.value = true
      },
      onMessage(_ws, e) {
        try {
          const msg = JSON.parse(e.data)
          if (msg && msg.tag === 'SharedStateUpdate') {
            applySharedState(msg.contents as SharedEventState)
          }
        } catch (err) {
          console.error(err)
        }
      },
    })
    closeFn = close
  }

  function disconnect() {
    if (closeFn) {
      closeFn()
      closeFn = null
    }
    connected.value = false
  }

  return {
    eventId,
    event,
    sharedState,
    groupDigests,
    received,
    connected,
    socketError,
    applySharedState,
    setEvent,
    load,
    connect,
    disconnect,
  }
})
