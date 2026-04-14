<script lang="ts" setup>
import { ref, computed, onUnmounted } from 'vue'
import { useRouter } from 'vue-router'
import { fetchOpenSeats, claimSeat } from '@/arkham/api'
import { useClipboard } from '@vueuse/core'
import { imgsrc } from '@/arkham/helpers'
import type { Game } from '@/arkham/types/Game'
import InvestigatorRow from '@/arkham/components/InvestigatorRow.vue'
import LogIcons from '@/arkham/components/LogIcons.vue'

const props = defineProps<{
  gameId: string
  game: Game
  playerId: string | null
}>()

const router = useRouter()
const openSeats = ref<string[]>([])
const myClaimed = ref<string | null>(null)
const loading = ref(false)
const error = ref<string | null>(null)

const isHost = computed(() => localStorage.getItem(`gameHost_${props.gameId}`) === 'true')
const allClaimed = computed(() => openSeats.value.length === 0)
const hasPlayerClaimed = computed(() =>
  isHost.value || myClaimed.value !== null || (props.playerId !== null && props.playerId in props.game.investigators)
)
const investigators = computed(() => Object.values(props.game.investigators))

const claimSeatUrl = computed(() => {
  const resolved = router.resolve({ name: 'ClaimSeat', params: { gameId: props.gameId } })
  return window.location.origin + window.location.pathname + resolved.href
})
const { copy, copied } = useClipboard({ source: claimSeatUrl })

function isOpen(investigatorId: string) {
  return openSeats.value.includes(investigatorId)
}

async function poll() {
  try { openSeats.value = await fetchOpenSeats(props.gameId) } catch { /* ignore */ }
}

poll()
const interval = setInterval(poll, 3000)
onUnmounted(() => clearInterval(interval))

async function claim(investigatorId: string) {
  loading.value = true
  error.value = null
  const normalized = investigatorId.startsWith('c') ? investigatorId : `c${investigatorId}`
  try {
    await claimSeat(props.gameId, normalized)
    myClaimed.value = normalized
  } catch {
    error.value = 'Could not claim this seat. It may have already been taken.'
  } finally {
    loading.value = false
  }
}

function onContinue() {
  router.push(`/games/${props.gameId}`)
}
</script>

<template>
  <div class="lobby scroll-container">
    <LogIcons />

    <!-- Header: scenario / campaign info, or generic waiting box -->
    <div class="next-scenario">
      <div class="next-scenario-info">
        <div class="scenario-info">
          <h3>{{ game.scenario ? 'Scenario' : game.campaign ? 'Campaign' : 'Multiplayer' }}</h3>
          <h2>{{ game.scenario?.name.title ?? game.campaign?.name ?? 'Waiting for Players' }}</h2>
        </div>
        <div class="invite-section">
          <p class="invite-label">Invite others to join:</p>
          <div class="invite-link">
            <input type="text" :value="claimSeatUrl" readonly />
            <button type="button" @click="copy()">{{ copied ? '✓ Copied' : 'Copy' }}</button>
          </div>
        </div>
      </div>
      <div v-if="game.scenario" class="next-step-icon">
        <img :src="imgsrc(`sets/${game.scenario.id.replace(/^c/, '')}.png`)" />
      </div>
      <div v-else-if="game.campaign" class="next-step-icon">
        <img :src="imgsrc(`sets/${game.campaign.id}.png`)" />
      </div>
    </div>

    <!-- Investigator rows -->
    <div v-if="investigators.length > 0" class="investigators">
      <InvestigatorRow
        v-for="investigator in investigators"
        :key="investigator.id"
        :investigator="investigator"
        :game="game"
      >
        <template #back>
          <button
            v-if="isOpen(investigator.id) && !hasPlayerClaimed"
            class="claim-btn"
            :disabled="loading"
            @click="claim(investigator.id)"
            type="button"
          >Claim</button>
          <span v-else-if="investigator.id === myClaimed" class="status-pill joined">Claimed</span>
          <span v-else-if="isOpen(investigator.id)" class="status-pill open">Waiting</span>
          <span v-else class="status-pill joined">Joined</span>
        </template>
      </InvestigatorRow>
    </div>

    <p v-if="error" class="error-msg">{{ error }}</p>

    <div v-if="allClaimed" class="actions">
      <button class="continue-btn" @click="onContinue" type="button">Continue</button>
    </div>

  </div>
</template>

<style scoped lang="scss">
.lobby {
  margin-top: 5vh;
  margin-inline: auto;
  display: flex;
  flex-direction: column;
  gap: 20px;
  width: min(40vw, 700px);

  @media (max-width: 800px) { width: 90vw; }
}

/* ── Header box ── */
.next-scenario {
  display: flex;
  justify-content: space-between;
  gap: 10px;
  padding: 16px;
  color: #bebebe;
  border: 2px solid var(--line);
  border-radius: 8px;
  background: rgba(255, 255, 255, 0.1);

  @media (max-width: 800px) {
    flex-direction: column;
    align-items: center;
    text-align: center;
  }

  img { max-height: 150px; }
}

.next-scenario-info {
  display: flex;
  flex-direction: column;
  gap: 12px;
  flex: 1;
}

.scenario-info {
  h3 {
    margin: 0 0 4px;
    color: rgba(255, 255, 255, 0.55);
    font-size: 0.8em;
    text-transform: uppercase;
    letter-spacing: 0.08em;
  }
  h2 {
    margin: 0;
    color: white;
    font-family: "Teutonic", sans-serif;
    font-size: 1.8em;
  }
}

.next-step-icon {
  width: 150px;
  filter: invert(100%) brightness(60%);
  display: flex;
  align-items: center;
  justify-content: center;

  @media (max-width: 600px) { display: none; }
}

/* ── Invite link ── */
.invite-section {
  display: flex;
  flex-direction: column;
  gap: 6px;
}

.invite-label {
  margin: 0;
  font-size: 0.78em;
  text-transform: uppercase;
  letter-spacing: 0.06em;
  color: rgba(255, 255, 255, 0.45);
}

.invite-link {
  display: flex;
  gap: 6px;

  input {
    flex: 1;
    min-width: 0;
    background: rgba(0, 0, 0, 0.3);
    border: 1px solid rgba(255, 255, 255, 0.15);
    color: rgba(255, 255, 255, 0.7);
    padding: 6px 10px;
    border-radius: 6px;
    font-size: 0.8em;
    outline: none;
  }

  button {
    padding: 6px 12px;
    background: rgba(0, 0, 0, 0.35);
    border: 0;
    color: white;
    border-radius: 6px;
    cursor: pointer;
    font-size: 0.82em;
    white-space: nowrap;
    &:hover { background: rgba(0, 0, 0, 0.55); }
  }
}

/* ── Investigators panel ── */
.investigators {
  padding: 10px;
  background: rgba(255, 255, 255, 0.05);
  display: flex;
  gap: 10px;
  flex-direction: column;
  border-radius: 8px;
}

/* ── Claim button (shown in #back slot) ── */
.claim-btn {
  background: rgba(110, 134, 64, 0.85);
  border: 0;
  color: white;
  padding: 7px 18px;
  border-radius: 6px;
  cursor: pointer;
  font-size: 0.85em;
  font-weight: 600;
  white-space: nowrap;
  transition: background 0.15s;

  &:hover:not(:disabled) { background: rgba(110, 134, 64, 1); }
  &:disabled { opacity: 0.45; cursor: not-allowed; }
}

/* ── Status pills ── */
.status-pill {
  display: inline-block;
  padding: 5px 12px;
  border-radius: 999px;
  font-size: 0.72em;
  font-weight: 600;
  text-transform: uppercase;
  letter-spacing: 0.06em;
  white-space: nowrap;

  &.joined {
    background: rgba(110, 134, 64, 0.2);
    color: rgba(168, 208, 128, 0.9);
    border: 1px solid rgba(110, 134, 64, 0.35);
  }

  &.open {
    background: rgba(255, 255, 255, 0.06);
    color: rgba(255, 255, 255, 0.4);
    border: 1px solid rgba(255, 255, 255, 0.1);
  }
}

/* ── Continue ── */
.actions {
  display: flex;
  flex-direction: column;
}

.continue-btn {
  border: 0;
  background: rgba(0, 0, 0, 0.3);
  color: white;
  font-size: 1.2em;
  padding: 10px 20px;
  border-radius: 8px;
  width: 100%;
  cursor: pointer;

  &:hover { background: rgba(0, 0, 0, 0.5); }
}

.error-msg {
  color: #e05050;
  font-size: 0.9em;
  margin: 0;
}
</style>
