<script lang="ts" setup>
import type { EventListEntry } from '@/arkham/types/EpicEvent'

defineProps<{ event: EventListEntry }>()
</script>

<template>
  <RouterLink class="event-row" :to="`/events/${event.id}`">
    <span class="event-accent" aria-hidden="true" />
    <div class="event-main">
      <div class="event-titleline">
        <span class="event-chip">{{ $t('event.epicMultiplayer') }}</span>
        <span class="event-name">{{ event.name }}</span>
      </div>
    </div>
    <span class="event-role">{{ $t(`event.role.${event.role}`) }}</span>
    <span class="event-cta" aria-hidden="true">›</span>
  </RouterLink>
</template>

<style scoped>
/* Distinct "epic" frame so these stand out inline among regular game rows. */
.event-row {
  position: relative;
  display: flex;
  align-items: center;
  gap: 14px;
  color: var(--title);
  background:
    linear-gradient(120deg, rgba(110, 134, 64, 0.16), rgba(110, 134, 64, 0) 55%),
    var(--box-background);
  border: 1px solid color-mix(in srgb, var(--spooky-green) 45%, var(--box-border));
  border-radius: 6px;
  margin-bottom: 10px;
  padding: 16px 18px 16px 22px;
  overflow: hidden;
  text-decoration: none;
  box-shadow: 0 8px 22px rgba(0, 0, 0, 0.22);
  transition: border-color 0.2s linear, transform 0.12s ease, box-shadow 0.2s linear;
}

.event-row:hover {
  border-color: var(--spooky-green);
  transform: translateY(-1px);
  box-shadow: 0 12px 28px rgba(0, 0, 0, 0.3);
}

.event-accent {
  position: absolute;
  left: 0;
  top: 0;
  bottom: 0;
  width: 5px;
  background: var(--spooky-green);
}

.event-main {
  flex: 1;
  min-width: 0;
}

.event-titleline {
  display: flex;
  align-items: center;
  gap: 12px;
  min-width: 0;
}

.event-chip {
  flex-shrink: 0;
  padding: 3px 10px;
  border-radius: 999px;
  background: var(--spooky-green);
  color: #fff;
  font-size: 0.66em;
  font-weight: 700;
  letter-spacing: 0.07em;
  text-transform: uppercase;
}

.event-name {
  font-family: teutonic, sans-serif;
  font-size: 1.6em;
  overflow: hidden;
  text-overflow: ellipsis;
  white-space: nowrap;
}

.event-role {
  flex-shrink: 0;
  text-transform: uppercase;
  font-size: 0.8em;
  opacity: 0.7;
}

.event-cta {
  flex-shrink: 0;
  font-size: 1.6em;
  line-height: 1;
  opacity: 0.5;
}

.event-row:hover .event-cta {
  opacity: 0.9;
}
</style>
