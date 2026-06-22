<script setup lang="ts">
import { computed } from 'vue'
import api from '@/api'
import Room from '@/components/admin/Room.vue'

interface RoomData {
  roomClients: number
  roomLastUpdateAt: string | null
  roomArkhamGameId: string
}

const request = await api.get<RoomData[]>('admin/rooms')
const data = computed(() => request.data)
</script>

<template>
  <section class="admin-block">
      <header class="section-header">
        <h2>Open Rooms</h2>
        <span class="count-badge" aria-label="Open rooms count">{{ data.length }}</span>
      </header>

      <div v-if="data.length === 0" class="empty box">No rooms.</div>
      <div v-else class="room-list">
        <Room v-for="room in data" :room="room" :key="room.roomArkhamGameId" />
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
</style>
