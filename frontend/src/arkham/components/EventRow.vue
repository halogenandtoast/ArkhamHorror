<script lang="ts" setup>
import { onMounted, ref } from 'vue'
import { fetchEvent } from '@/arkham/api'
import Prompt from '@/components/Prompt.vue'
import { imgsrc } from '@/arkham/helpers'
import { useEpicHelpers } from '@/arkham/composables/useEpicHelpers'
import { useDbCardStore } from '@/stores/dbCards'
import type { EventListEntry, GroupDigest } from '@/arkham/types/EpicEvent'

const props = defineProps<{
  event: EventListEntry
  deleteEvent?: () => void
}>()
const groups = ref<GroupDigest[]>([])
const deleting = ref(false)
const { groupLabel } = useEpicHelpers()
const dbStore = useDbCardStore()

function bareCode(investigatorId: string): string {
  return investigatorId.replace(/^c/, '')
}

function portraitSrc(investigatorId: string): string {
  return imgsrc(`portraits/${bareCode(investigatorId)}.jpg`)
}

function investigatorClass(investigatorId: string): string {
  return dbStore.getDbCard(bareCode(investigatorId))?.faction_code ?? 'neutral'
}

function openSeatCount(group: GroupDigest): number {
  const openPlayers = group.players.filter((player) => !player.investigatorId).length
  return Math.max(openPlayers, group.seatCount - group.players.filter((player) => player.investigatorId).length)
}

function confirmDelete(event: MouseEvent) {
  event.preventDefault()
  event.stopPropagation()
  deleting.value = true
}

onMounted(async () => {
  void dbStore.initDbCards()
  try {
    const details = await fetchEvent(props.event.id)
    groups.value = details.groups
  } catch (e) {
    console.error(e)
  }
})
</script>

<template>
  <RouterLink custom :to="`/events/${event.id}`" v-slot="{ navigate }">
    <div
      class="event-row"
      role="link"
      tabindex="0"
      @click="navigate"
      @keydown.enter="() => navigate()"
      @keydown.space.prevent="() => navigate()"
    >
      <span class="event-accent" aria-hidden="true" />
      <div class="event-details">
      <div class="event-title">
        <div class="main-details">
          <span class="event-chip">{{ $t('event.epicMultiplayer') }}</span>
          <span class="event-name">{{ event.name }}</span>
        </div>
        <div class="extra-details">
          <span class="event-role">{{ $t(`event.role.${event.role}`) }}</span>
          <div v-if="deleteEvent && event.role === 'organizer'" class="event-delete">
            <a href="#delete" :aria-label="$t('event.deleteEvent')" @click="confirmDelete">
              <font-awesome-icon icon="trash" />
            </a>
          </div>
        </div>
      </div>
        <div v-if="groups.length" class="event-groups" aria-label="Epic multiplayer groups">
          <section v-for="group in groups" :key="group.ordinal" class="event-group-card">
            <h2>{{ groupLabel(group) }}</h2>
            <div class="investigators">
              <div
                v-for="(player, i) in group.players.filter((p) => p.investigatorId)"
                :key="`${player.username}-${i}`"
                class="investigator"
                :title="player.username"
              >
                <div :class="`investigator-portrait-container ${investigatorClass(player.investigatorId!)}`">
                  <img :src="portraitSrc(player.investigatorId!)" class="investigator-portrait" />
                </div>
              </div>
              <div
                v-for="seat in openSeatCount(group)"
                :key="`open-seat-${seat}`"
                class="investigator"
                :title="$t('event.openSeats')"
              >
                <div class="investigator-portrait-container neutral empty-seat">?</div>
              </div>
            </div>
          </section>
        </div>
      </div>
    </div>
  </RouterLink>
  <Prompt
    v-if="deleting && deleteEvent"
    :prompt="$t('event.confirmDelete')"
    :yes="deleteEvent"
    :no="() => deleting = false"
  />
</template>

<style scoped>
/* Epic rows keep the two-band GameRow shape, but with a mythos-green event rail. */
.event-row {
  position: relative;
  display: flex;
  color: var(--title);
  background:
    linear-gradient(120deg, rgba(110, 134, 64, 0.14), rgba(110, 134, 64, 0) 58%),
    var(--box-background);
  border: 1px solid color-mix(in srgb, var(--spooky-green) 40%, var(--box-border));
  border-radius: 3px;
  margin-bottom: 10px;
  overflow: hidden;
  text-decoration: none;
  cursor: pointer;
  box-shadow: 0 8px 22px rgba(0, 0, 0, 0.2);
  transition: border-color 0.2s linear, transform 0.12s ease, box-shadow 0.2s linear;
}

.event-row:hover {
  border-color: var(--spooky-green);
  transform: translateY(-1px);
  box-shadow: 0 12px 28px rgba(0, 0, 0, 0.28);
}

.event-row:focus-visible {
  outline: 2px solid var(--spooky-green);
  outline-offset: 2px;
}

.event-accent {
  position: absolute;
  inset: 0 auto 0 0;
  width: 5px;
  background:
    linear-gradient(180deg, rgba(210, 228, 158, 0.95), var(--spooky-green));
  box-shadow: 0 0 18px rgba(110, 134, 64, 0.55);
}

.event-details {
  flex: 1;
  min-width: 0;
}

.event-title {
  position: relative;
  display: flex;
  align-items: center;
  gap: 10px;
  padding: 10px 12px 10px 18px;
  border-bottom: 1px solid color-mix(in srgb, var(--spooky-green) 30%, var(--box-border));
}

.main-details,
.extra-details {
  display: flex;
  align-items: center;
  gap: 10px;
}

.main-details {
  flex: 1;
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
  flex: 1;
  min-width: 0;
  font-family: teutonic, sans-serif;
  font-size: 1.6em;
  overflow: hidden;
  text-overflow: ellipsis;
  white-space: nowrap;
}

.event-role {
  padding: 5px 15px;
  background: rgba(0, 0, 0, 0.5);
  border-radius: 10px;
  text-transform: uppercase;
  font-size: 0.8em;
  opacity: 0.82;
  white-space: nowrap;
}

.event-delete {
  transition: all 0.5s;
  position: relative;
  align-self: center;
  display: flex;

  a {
    font-size: 1.2em;
    color: var(--delete);

    &:hover {
      color: #990000;
    }
  }
}

.event-groups {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(170px, 1fr));
  gap: 10px;
  padding: 10px 12px 10px 18px;
  background: rgba(255, 255, 255, 0.02);
}

.event-group-card {
  min-width: 0;
  overflow: hidden;
  border: 1px solid rgba(110, 134, 64, 0.28);
  border-radius: 4px;
  background: rgba(0, 0, 0, 0.18);
}

.event-group-card h2 {
  margin: 0;
  padding: 2px 6px;
  background: var(--background-dark);
  color: var(--title);
  font-size: 0.8em;
  letter-spacing: 0.08em;
  text-transform: uppercase;
}

.investigators {
  display: flex;
  gap: 4px;
  min-height: 54px;
  padding: 8px;
  align-items: center;
  background: rgba(255, 255, 255, 0.02);
}

.investigator {
  display: inline;
  padding: 2px;
  border-radius: 10px;
}

.investigator-portrait-container {
  width: 42px;
  height: 42px;
  overflow: hidden;
  border-radius: 5px;
  box-shadow: 1px 1px 6px rgba(0, 0, 0, 0.45);
}

.investigator-portrait-container.survivor {
  border: 3px solid var(--survivor-extra-dark);
}

.investigator-portrait-container.guardian {
  border: 3px solid var(--guardian-extra-dark);
}

.investigator-portrait-container.mystic {
  border: 3px solid var(--mystic-extra-dark);
}

.investigator-portrait-container.seeker {
  border: 3px solid var(--seeker-extra-dark);
}

.investigator-portrait-container.rogue {
  border: 3px solid var(--rogue-extra-dark);
}

.investigator-portrait-container.neutral {
  border: 3px solid var(--neutral);
}

.investigator-portrait {
  width: 126px;
}

.investigator-portrait-container.empty-seat {
  display: flex;
  align-items: center;
  justify-content: center;
  border: 3px dashed var(--neutral);
  background: rgba(255, 255, 255, 0.04);
  color: color-mix(in srgb, var(--title) 58%, transparent);
  font-size: 1.4em;
  font-weight: 700;
}

@media (max-width: 600px) {
  .event-title {
    flex-direction: column;
    align-items: flex-start;
    font-size: 0.85em;
  }

  .extra-details {
    width: 100%;
  }

  .event-role {
    flex: 1;
  }

  .event-groups {
    grid-template-columns: 1fr;
  }
}
</style>
