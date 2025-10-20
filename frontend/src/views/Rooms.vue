<script setup lang="ts">
import { computed } from 'vue'
import api from '@/api'
import AdminUI from '@/arkham/components/Admin/UI.vue'
import Room from '@/components/admin/Room.vue'

interface RoomData {
  roomClients: number
  roomLastUpdateAt: string
  roomArkhamGameId: string
}

const request = await api.get<RoomData[]>('admin/rooms')
const data = computed(() => request.data)
</script>

<template>
  <AdminUI :selected="'rooms'">
    <header class="topbar">
      <button class="hamburger" @click="toggleSidebar" aria-label="Open menu">
        <svg viewBox="0 0 24 24"><path d="M3 6h18v2H3V6zm0 10h18v2H3v-2zm0-5h18v2H3v-2z" fill="currentColor"/></svg>
      </button>
      <h1>Rooms</h1>
    </header>

    <Room v-for="room in data" :room="room" :key="room.roomArkhamGameId" />
  </AdminUI>
</template>

<style scoped>
.room {
  padding: 20px;
  border-bottom: 1px solid var(--line);
  display: flex;
  *  {
    flex: 1;
  }
}

/* top bar */
.topbar {
  position: sticky; top: 0; z-index: 10;
  display: flex; align-items: center; gap: 12px;
  padding: 14px 20px;
  border-bottom: 1px solid var(--line);
  background: color-mix(in oklab, var(--bg) 85%, transparent);
  backdrop-filter: blur(6px);
}
.topbar h1 { font-size: 1rem; margin: 0; color: var(--text); font-weight: 700; letter-spacing: .02em }
.hamburger {
  display: none;
  width: 36px; height: 36px; border-radius: 10px;
  background: rgba(255,255,255,.04);
  border: 1px solid var(--line);
  color: var(--text);
}
.hamburger:hover { background: rgba(255,255,255,.07) }

@media (max-width: 960px) {
  .hamburger { display: grid; place-items: center }
}
</style>
