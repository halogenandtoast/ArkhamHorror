<script setup lang="ts">
import { computed } from 'vue'
import { useI18n } from 'vue-i18n'
import api from '@/api'

const { t } = useI18n()

interface RoomData {
  roomClients: number
  roomLastUpdatedAt: string | null
  roomArkhamGameId: string
}

const props = defineProps<{
  room: RoomData
}>()

/* No timestamp means there is no game row behind this room any more: the game was
 * deleted and the room outlived it, which is worth calling out on its own. */
const deleted = computed(() => !props.room.roomLastUpdatedAt)
const lastUpdated = computed(() => props.room.roomLastUpdatedAt ?? 'deleted')

/* Dropping a room evicts everyone on it -- the server sends them home and closes
 * their sockets -- so a room with people in it asks first. An orphaned one has
 * nobody to interrupt. */
async function deleteRoom() {
  if (props.room.roomClients > 0 && !confirm(t('admin.confirmDropRoom', props.room.roomClients))) {
    return
  }
  await api.delete(`admin/rooms/${props.room.roomArkhamGameId}`)
  window.location.reload()
}
</script>

<template>
  <div class="room box" :class="{ deleted }">
    <div class="room-stat">
      <span class="label">Clients</span>
      <strong>{{ room.roomClients }}</strong>
    </div>
    <div class="room-stat updated">
      <span class="label">Last Updated</span>
      <span>{{ lastUpdated }}</span>
    </div>
    <div class="room-actions">
      <router-link :to="`/admin/games/${room.roomArkhamGameId}`">View</router-link>
      <a href="#" class="delete" @click.prevent="deleteRoom">
        {{ room.roomClients > 0 ? t('admin.dropRoom') : t('admin.deleteRoom') }}
      </a>
    </div>
  </div>
</template>

<style scoped>
.room {
  align-items: center;
  display: grid;
  gap: 14px;
  grid-template-columns: minmax(80px, 0.5fr) minmax(180px, 1fr) auto;
}

.room.deleted {
  opacity: 0.82;
}

.room-stat {
  display: flex;
  flex-direction: column;
  gap: 2px;
  min-width: 0;
}

.label {
  color: color-mix(in srgb, var(--title) 62%, transparent);
  font-size: 0.72rem;
  font-weight: 700;
  letter-spacing: 0.08em;
  text-transform: uppercase;
}

strong {
  color: var(--title);
  font-size: 1.3rem;
}

.updated span:last-child {
  color: var(--title);
  overflow-wrap: anywhere;
}

.room-actions {
  display: flex;
  gap: 8px;
  justify-content: flex-end;
}

.room-actions a {
  background: var(--background-dark);
  border: 1px solid var(--box-border);
  border-radius: 3px;
  color: var(--title);
  font-size: 0.8rem;
  font-weight: 700;
  padding: 7px 10px;
  text-decoration: none;
  text-transform: uppercase;
}

.room-actions a:hover {
  background: rgba(255, 255, 255, 0.06);
  color: white;
}

.room-actions .delete {
  border-color: color-mix(in srgb, var(--delete) 65%, var(--box-border));
  color: color-mix(in srgb, var(--delete) 35%, white);
}

@media (max-width: 620px) {
  .room {
    align-items: stretch;
    grid-template-columns: 1fr;
  }

  .room-actions {
    justify-content: flex-start;
  }
}
</style>
