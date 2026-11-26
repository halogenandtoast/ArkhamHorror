<script setup lang="ts">
/* The admin-written notifications, as a feed down the side of the games list
 * rather than a stack of banners across the top of it.
 *
 * Each entry is an AnnouncementCard, which is the same card the admin panel
 * previews when one is being written. This component owns only the panel and
 * the order they sit in; the server sends them newest first.
 *
 * An announcement cannot be dismissed: it stands until an admin takes it down.
 */
import type { AppNotification } from '@/arkham/api'
import AnnouncementCard from '@/arkham/components/AnnouncementCard.vue'
import FeedPanel from '@/components/FeedPanel.vue'

const props = defineProps<{ notifications: AppNotification[] }>()
</script>

<template>
  <FeedPanel :title="$t('home.announcements')" icon="bullhorn" :count="props.notifications.length">
    <ul>
      <li v-for="notification in props.notifications" :key="notification.id">
        <AnnouncementCard :notification="notification" />
      </li>
    </ul>
  </FeedPanel>
</template>

<style scoped>
ul {
  display: flex;
  flex-direction: column;
  gap: 12px;
  list-style: none;
  margin: 0;
  padding: 0;
}
</style>
