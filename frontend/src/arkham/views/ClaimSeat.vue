<script lang="ts" setup>
import { ref, onUnmounted } from 'vue'
import { useRouter } from 'vue-router'
import { fetchOpenSeats, claimSeat } from '@/arkham/api'
import { imgsrc } from '@/arkham/helpers'

export interface Props {
  gameId: string
}

const props = defineProps<Props>()
const router = useRouter()

const openSeats = ref<string[]>([])
const loading = ref(false)
const error = ref<string | null>(null)
const copied = ref(false)

const inviteUrl = window.location.href

async function copyInvite() {
  try {
    await navigator.clipboard.writeText(inviteUrl)
    copied.value = true
    setTimeout(() => copied.value = false, 2000)
  } catch {
    error.value = 'Could not copy invite link.'
  }
}

function loadOpenSeats(showError = false) {
  fetchOpenSeats(props.gameId).then(seats => {
    openSeats.value = seats
  }).catch(() => {
    if (showError) error.value = 'Could not load open seats.'
  })
}

loadOpenSeats(true)

const poll = setInterval(() => loadOpenSeats(), 3000)

onUnmounted(() => clearInterval(poll))

async function claim(investigatorId: string) {
  loading.value = true
  error.value = null
  const normalized = investigatorId.startsWith('c') ? investigatorId : `c${investigatorId}`
  try {
    await claimSeat(props.gameId, normalized)
    router.push(`/games/${props.gameId}`)
  } catch {
    error.value = 'Could not claim this seat. It may have already been taken.'
    loading.value = false
  }
}
</script>

<template>
  <div class="container">
    <div class="card">
      <header>
        <h2>Join Game</h2>
      </header>

      <div v-if="openSeats.length === 0" class="empty">
        <p>No open seats available in this game.</p>
        <router-link :to="`/games/${gameId}`">Go to game →</router-link>
      </div>

      <div v-else>
        <div class="invite-section">
          <p class="label">Send this link to other players:</p>
          <div class="invite-link">
            <input type="text" :value="inviteUrl" readonly />
            <button @click="copyInvite()" class="copy-btn">
              {{ copied ? '✓ Copied!' : 'Copy' }}
            </button>
          </div>
        </div>

        <p class="label">Choose your investigator:</p>
        <div class="seats-grid">
          <button
            v-for="iid in openSeats"
            :key="iid"
            class="seat-btn"
            :disabled="loading"
            @click="claim(iid)"
            type="button"
          >
            <img
              :src="imgsrc(`portraits/${iid.replace('c', '')}.jpg`)"
              :alt="iid"
              class="portrait"
            />
            <span class="iid">{{ iid }}</span>
          </button>
        </div>
      </div>

      <p v-if="error" class="error">{{ error }}</p>
    </div>
  </div>
</template>

<style scoped>
.container {
  width: 100%;
  max-width: 600px;
  margin: 40px auto 0;
  padding: 0 16px;
}

.card {
  background-color: var(--box-background);
  border: 1px solid var(--box-border);
  border-radius: 5px;
  padding: 20px;
  display: flex;
  flex-direction: column;
  gap: 16px;
}

h2 {
  color: var(--title);
  font-size: 2em;
  text-transform: uppercase;
  font-family: teutonic, sans-serif;
  margin: 0;
}

.label {
  color: var(--title);
  text-transform: uppercase;
  font-size: 0.85em;
  letter-spacing: 0.05em;
  margin: 0;
}

.seats-grid {
  display: flex;
  flex-wrap: wrap;
  gap: 12px;
  margin-top: 8px;
}

.seat-btn {
  display: flex;
  flex-direction: column;
  align-items: center;
  gap: 6px;
  background: none;
  border: 3px solid var(--box-border);
  border-radius: 6px;
  padding: 8px;
  cursor: pointer;
  transition: border-color 0.2s, transform 0.1s;
  color: var(--title);

  &:hover:not(:disabled) {
    border-color: var(--spooky-green);
    transform: scale(1.05);
  }

  &:disabled {
    opacity: 0.5;
    cursor: not-allowed;
  }
}

.portrait {
  width: 80px;
  height: 80px;
  object-fit: cover;
  object-position: top;
  border-radius: 3px;
}

.iid {
  font-size: 0.75em;
  opacity: 0.6;
}

.invite-section {
  display: flex;
  flex-direction: column;
  gap: 6px;
  padding-bottom: 16px;
  border-bottom: 1px solid var(--box-border);
  margin-bottom: 8px;
}

.copy-btn {
  padding: 6px 14px;
  background: var(--spooky-green);
  border: 0;
  color: white;
  border-radius: 3px;
  cursor: pointer;
  font-size: 0.85em;
  white-space: nowrap;
}

.copy-btn:hover {
  background: hsl(80, 35%, 32%);
}

.invite-link input {
  flex: 1;
  background: var(--background-dark);
  border: 1px solid var(--box-border);
  color: var(--title);
  padding: 6px 10px;
  border-radius: 3px;
  font-size: 0.85em;
}

.empty {
  display: flex;
  flex-direction: column;
  gap: 8px;
  color: var(--title);
}

.empty a {
  color: var(--spooky-green);
}

.error {
  color: #e05050;
  font-size: 0.9em;
  margin: 0;
}
</style>
