<script setup lang="ts">
import { computed, ref } from 'vue'
import api from '@/api'
import Room from '@/components/admin/Room.vue'
import RoomDetails from '@/components/admin/RoomDetails.vue'
import type { RoomData, RoomDetails as RoomDetailsData } from '@/components/admin/roomTypes'

const request = await api.get<RoomData[]>('admin/rooms')
const data = computed(() => request.data)

/* Looked up by id rather than only openable from the list: the list is built
 * from the Redis tally, so a room a pod still holds but the tally has lost is
 * invisible there -- and that is exactly the room worth asking about. */
const lookupId = ref('')
const details = ref<RoomDetailsData | null>(null)
const error = ref<string | null>(null)
const loading = ref(false)

async function lookup(gameId: string) {
  const trimmed = gameId.trim()
  if (!trimmed) return
  loading.value = true
  error.value = null
  try {
    const response = await api.get<RoomDetailsData>(`admin/rooms/${trimmed}`)
    details.value = response.data
    lookupId.value = trimmed
  } catch (e) {
    details.value = null
    error.value = `Could not look up ${trimmed}: ${e}`
  } finally {
    loading.value = false
  }
}
</script>

<template>
  <section class="admin-block">
    <header class="section-header">
      <h2>Room Lookup</h2>
    </header>
    <form class="lookup" @submit.prevent="lookup(lookupId)">
      <input v-model="lookupId" type="text" placeholder="Game id" spellcheck="false" />
      <button type="submit" :disabled="loading">{{ loading ? 'Looking…' : 'Look up' }}</button>
    </form>
    <div v-if="error" class="empty box">{{ error }}</div>
    <RoomDetails v-if="details" :details="details" @update="details = $event" />
  </section>

  <section class="admin-block">
      <header class="section-header">
        <h2>Open Rooms</h2>
        <span class="count-badge" aria-label="Open rooms count">{{ data.length }}</span>
      </header>

      <div v-if="data.length === 0" class="empty box">No rooms.</div>
      <div v-else class="room-list">
        <Room
          v-for="room in data"
          :room="room"
          :key="room.roomArkhamGameId"
          @inspect="lookup(room.roomArkhamGameId)"
        />
      </div>
  </section>
</template>

<style scoped>
.admin-block,
.room-list {
  display: flex;
  flex-direction: column;
  gap: 10px;
}

.admin-block {
  background: color-mix(in srgb, var(--background-dark) 42%, transparent);
  border: 1px solid color-mix(in srgb, var(--box-border) 75%, transparent);
  border-radius: 6px;
  padding: 14px;
  box-shadow: 0 8px 20px rgba(0, 0, 0, 0.12);
}

.section-header {
  align-items: center;
  display: flex;
  gap: 12px;
}

.section-header h2 {
  color: var(--title);
  flex: 1;
  font-family: teutonic, sans-serif;
  font-size: 1.6rem;
  line-height: 1;
  margin: 0;
  text-transform: uppercase;
}

.count-badge {
  align-items: center;
  background: var(--background-dark);
  border: 1px solid var(--spooky-green);
  border-left-width: 4px;
  border-radius: 3px;
  color: color-mix(in srgb, var(--spooky-green) 78%, white);
  display: inline-flex;
  font-size: 0.78rem;
  font-weight: 800;
  justify-content: center;
  line-height: 1;
  min-width: 2.1em;
  padding: 5px 9px 5px 7px;
}

.empty {
  color: var(--title);
  opacity: 0.75;
}

.lookup {
  display: flex;
  gap: 8px;
}

.lookup input {
  background: var(--background-dark);
  border: 1px solid var(--box-border);
  border-radius: 3px;
  color: var(--title);
  flex: 1;
  font-family: monospace;
  min-width: 0;
  padding: 8px 10px;
}

.lookup button {
  background: var(--background-dark);
  border: 1px solid var(--box-border);
  border-radius: 3px;
  color: var(--title);
  font-size: 0.8rem;
  font-weight: 700;
  padding: 8px 12px;
  text-transform: uppercase;
}

.lookup button:hover:not(:disabled) {
  background: rgba(255, 255, 255, 0.06);
  color: white;
}
</style>
