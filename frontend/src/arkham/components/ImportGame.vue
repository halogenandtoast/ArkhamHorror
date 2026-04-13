<script lang="ts" setup>
import { ref, computed } from 'vue'
import { useRouter } from 'vue-router'
import { importGame } from '@/arkham/api'
import { imgsrc } from '@/arkham/helpers'

const emit = defineEmits(['close'])
const router = useRouter()

const fileInput = ref<HTMLInputElement | null>(null)
const selectedFile = ref<File | null>(null)
const investigators = ref<string[]>([])
const isMultiplayer = ref(false)
const forceWithFriends = ref(false)
const selectedInvestigator = ref<string | null>(null)
const loading = ref(false)
const error = ref<string | null>(null)

const showInvestigatorPicker = computed(() =>
  (isMultiplayer.value || forceWithFriends.value) && investigators.value.length > 0
)

async function onFileChange(e: Event) {
  const file = (e.target as HTMLInputElement).files?.[0]
  if (!file) return
  selectedFile.value = file
  error.value = null
  investigators.value = []
  selectedInvestigator.value = null

  try {
    const text = await file.text()
    const json = JSON.parse(text)
    const variant = json.multiplayerVariant ?? json.campaignData?.multiplayerVariant ?? 'Solo'
    isMultiplayer.value = variant === 'WithFriends'
    // Prefer gamePlayerOrder (has proper 'c' prefix like 'c03004')
    // campaignPlayers from arkhamhorror.app uses short IDs ('03004') which breaks seat assignment
    const playerOrder: string[] = json.campaignData?.currentData?.gamePlayerOrder ?? []
    const fallbackPlayers: string[] = (json.campaignPlayers ?? []).map((id: string) =>
      id.startsWith('c') ? id : `c${id}`
    )
    investigators.value = playerOrder.length > 0 ? playerOrder : fallbackPlayers
  } catch {
    error.value = 'Could not parse the export file. Make sure it is a valid Arkham Horror export.'
    selectedFile.value = null
  }
}

async function submit() {
  if (!selectedFile.value) return
  loading.value = true
  error.value = null

  const formData = new FormData()
  formData.append('debugFile', selectedFile.value)

  if (isMultiplayer.value || forceWithFriends.value) {
    formData.append('multiplayerVariant', 'WithFriends')
  }
  if (selectedInvestigator.value) {
    formData.append('investigatorId', selectedInvestigator.value)
  }

  try {
    const game = await importGame(formData)
    emit('close')
    router.push(`/games/${game.id}`)
  } catch {
    error.value = 'Failed to import the game. Please try again.'
    loading.value = false
  }
}

const canSubmit = computed(() => {
  if (!selectedFile.value) return false
  if ((isMultiplayer.value || forceWithFriends.value) && !selectedInvestigator.value) return false
  return !loading.value
})
</script>

<template>
  <div class="import-game box">
    <header>
      <h2>Load Existing Game</h2>
    </header>

    <p class="description">
      Load a game exported via "Debug Export" to continue it here.
    </p>

    <div class="file-section">
      <label class="file-label">
        <span>{{ selectedFile ? selectedFile.name : 'Choose export file…' }}</span>
        <input
          type="file"
          accept="application/json"
          ref="fileInput"
          @change="onFileChange"
          class="hidden-input"
        />
      </label>
    </div>

    <!-- Force WithFriends toggle — shown only for Solo exports -->
    <label v-if="!isMultiplayer && investigators.length > 0" class="toggle-label">
      <input type="checkbox" v-model="forceWithFriends" @change="selectedInvestigator = null" />
      Import as multiplayer (WithFriends)
    </label>

    <!-- Investigator picker for multiplayer games -->
    <div v-if="showInvestigatorPicker" class="seat-picker">
      <p class="seat-label">Choose your investigator:</p>
      <div class="investigators-grid">
        <button
          v-for="iid in investigators"
          :key="iid"
          class="investigator-btn"
          :class="{ selected: selectedInvestigator === iid }"
          @click="selectedInvestigator = iid"
          type="button"
        >
          <img
            :src="imgsrc(`portraits/${iid.replace('c', '')}.jpg`)"
            :alt="iid"
            class="investigator-portrait"
          />
        </button>
      </div>
      <p v-if="isMultiplayer && !selectedInvestigator" class="hint">
        Select your investigator to continue
      </p>
    </div>

    <p v-if="error" class="error">{{ error }}</p>

    <div class="actions">
      <button @click="emit('close')" class="btn-cancel" type="button">Cancel</button>
      <button
        @click="submit"
        :disabled="!canSubmit"
        class="btn-submit"
        type="button"
      >
        {{ loading ? 'Loading…' : 'Load Game' }}
      </button>
    </div>
  </div>
</template>

<style scoped>
.import-game {
  display: flex;
  flex-direction: column;
  gap: 12px;
}

h2 {
  color: var(--title);
  font-size: 1.6em;
  text-transform: uppercase;
  font-family: teutonic, sans-serif;
  margin: 0;
}

.description {
  color: var(--title);
  margin: 0;
  font-size: 0.95em;
  opacity: 0.8;
}

.file-label {
  display: flex;
  align-items: center;
  gap: 10px;
  padding: 10px 14px;
  background: var(--background-dark);
  border: 1px dashed var(--box-border);
  border-radius: 4px;
  cursor: pointer;
  color: var(--title);
  font-size: 0.9em;
  transition: border-color 0.2s;

  &:hover {
    border-color: var(--spooky-green);
  }
}

.hidden-input {
  display: none;
}

.seat-label {
  color: var(--title);
  text-transform: uppercase;
  font-size: 0.85em;
  letter-spacing: 0.05em;
  margin: 0;
}

.investigators-grid {
  display: flex;
  flex-wrap: wrap;
  gap: 8px;
}

.investigator-btn {
  background: none;
  border: 3px solid transparent;
  border-radius: 6px;
  padding: 0;
  cursor: pointer;
  transition: border-color 0.2s, transform 0.1s;

  &:hover {
    border-color: rgba(255, 255, 255, 0.4);
    transform: scale(1.05);
  }

  &.selected {
    border-color: var(--spooky-green);
    transform: scale(1.05);
  }
}

.investigator-portrait {
  display: block;
  width: 64px;
  height: 64px;
  object-fit: cover;
  object-position: top;
  border-radius: 3px;
}

.hint {
  color: var(--title);
  opacity: 0.5;
  font-size: 0.85em;
  margin: 0;
}

.toggle-label {
  display: flex;
  align-items: center;
  gap: 8px;
  color: var(--title);
  font-size: 0.9em;
  cursor: pointer;
  user-select: none;

  input[type="checkbox"] {
    accent-color: var(--spooky-green);
    width: 16px;
    height: 16px;
    cursor: pointer;
  }
}

.error {
  color: #e05050;
  font-size: 0.9em;
  margin: 0;
}

.actions {
  display: flex;
  gap: 8px;
  justify-content: flex-end;
  margin-top: 4px;
}

.btn-cancel {
  padding: 8px 18px;
  background: transparent;
  border: 1px solid var(--box-border);
  color: var(--title);
  border-radius: 3px;
  cursor: pointer;
  text-transform: uppercase;
  font-size: 0.9em;

  &:hover {
    background: rgba(255, 255, 255, 0.05);
  }
}

.btn-submit {
  padding: 8px 18px;
  background: var(--spooky-green);
  border: 0;
  color: white;
  border-radius: 3px;
  cursor: pointer;
  text-transform: uppercase;
  font-size: 0.9em;
  font-weight: bold;
  transition: background 0.2s;

  &:hover:not(:disabled) {
    background: hsl(80, 35%, 32%);
  }

  &:disabled {
    background: #555;
    cursor: not-allowed;
  }
}
</style>
