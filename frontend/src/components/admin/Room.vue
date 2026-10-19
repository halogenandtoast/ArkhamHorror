<script setup lang="ts">
import { computed } from 'vue'
import { useI18n } from 'vue-i18n'
import api from '@/api'
import type { RoomData } from './roomTypes'

const { t } = useI18n()

const props = defineProps<{
  room: RoomData
}>()

const emit = defineEmits<{ (e: 'inspect'): void }>()

/* No timestamp means there is no game row behind this room any more: the game was
 * deleted and the room outlived it, which is worth calling out on its own. */
const deleted = computed(() => !props.room.roomLastUpdatedAt)
const lastUpdated = computed(() => props.room.roomLastUpdatedAt ?? 'deleted')

/* All clients and no players: nothing further will happen on that board, so
 * the room is being held open by its watchers (or by a tally nobody is behind).
 * Either way it is a candidate for a sweep. */
const watchersOnly = computed(
  () => props.room.roomClients > 0 && props.room.roomWatchers >= props.room.roomClients,
)

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
  <div class="room box" :class="{ deleted, 'watchers-only': watchersOnly }">
    <div class="room-stat">
      <span class="label">Clients</span>
      <strong>{{ room.roomClients }}</strong>
    </div>
    <div class="room-stat">
      <span class="label">Watching</span>
      <strong>{{ room.roomWatchers }}</strong>
    </div>
    <div class="room-stat updated">
      <span class="label">Last Updated</span>
      <span>{{ lastUpdated }}</span>
    </div>
    <div class="room-actions">
      <a href="#" @click.prevent="emit('inspect')">{{ t('admin.inspectRoom') }}</a>
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
  grid-template-columns: minmax(70px, 0.4fr) minmax(70px, 0.4fr) minmax(180px, 1fr) auto;
}

.room.watchers-only {
  border-left: 3px solid color-mix(in srgb, var(--delete) 70%, transparent);
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
