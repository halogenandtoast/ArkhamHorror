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
const importMode = ref<'Solo' | 'WithFriends'>('Solo')
const selectedInvestigator = ref<string | null>(null)
const loading = ref(false)
const error = ref<string | null>(null)

const showModePicker = computed(() =>
  !isMultiplayer.value && investigators.value.length > 0
)

const showInvestigatorPicker = computed(() =>
  (isMultiplayer.value || importMode.value === 'WithFriends') && investigators.value.length > 1
)

async function onFileChange(e: Event) {
  const file = (e.target as HTMLInputElement).files?.[0]
  if (!file) return
  selectedFile.value = file
  error.value = null
  investigators.value = []
  selectedInvestigator.value = null
  importMode.value = 'Solo'

  try {
    const text = await file.text()
    const json = JSON.parse(text)
    const variant = json.multiplayerVariant ?? json.campaignData?.multiplayerVariant ?? 'Solo'
    isMultiplayer.value = variant === 'WithFriends'
    if (isMultiplayer.value) importMode.value = 'WithFriends'
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

function onModeChange() {
  selectedInvestigator.value = null
}

async function submit() {
  if (!selectedFile.value) return
  loading.value = true
  error.value = null

  const formData = new FormData()
  formData.append('debugFile', selectedFile.value)

  const multiplayer = isMultiplayer.value || importMode.value === 'WithFriends'

  if (multiplayer) {
    formData.append('multiplayerVariant', 'WithFriends')
    const investigatorId = selectedInvestigator.value ?? (investigators.value.length === 1 ? investigators.value[0] : null)
    if (investigatorId) {
      formData.append('investigatorId', investigatorId)
    }
  }

  try {
    const game = await importGame(formData)
    emit('close')
    if (multiplayer) {
      localStorage.setItem(`gameHost_${game.id}`, 'true')
    }
    router.push(`/games/${game.id}`)
  } catch {
    error.value = 'Failed to import the game. Please try again.'
    loading.value = false
  }
}

const canSubmit = computed(() => {
  if (!selectedFile.value) return false
  const multiplayer = isMultiplayer.value || importMode.value === 'WithFriends'
  if (multiplayer && investigators.value.length > 1 && !selectedInvestigator.value) return false
  return !loading.value
})
</script>

<template>
  <div class="import-game box">
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

    <!-- Import mode picker — shown only for Solo exports -->
    <div v-if="showModePicker" class="mode-section">
      <div class="mode-picker">
        <input type="radio" v-model="importMode" value="Solo" id="mode-solo" @change="onModeChange" />
        <label for="mode-solo">Solo</label>
        <input type="radio" v-model="importMode" value="WithFriends" id="mode-multi" @change="onModeChange" />
        <label for="mode-multi">Multiplayer</label>
      </div>
      <p class="mode-description">
        <template v-if="importMode === 'Solo'">You will control all investigators.</template>
        <template v-else>You will receive an invite link so others can join and claim their investigators.</template>
      </p>
    </div>

    <!-- Investigator picker for multiplayer games -->
    <div v-if="showInvestigatorPicker" class="seat-picker">
      <div class="investigator-rows">
        <div
          v-for="iid in investigators"
          :key="iid"
          class="investigator-row"
          :class="{ selected: selectedInvestigator === iid }"
        >
          <div class="portrait-wrap">
            <img
              :src="imgsrc(`portraits/${iid.replace('c', '')}.jpg`)"
              :alt="iid"
              class="investigator-portrait"
            />
          </div>
          <button
            class="claim-btn"
            :class="{ claimed: selectedInvestigator === iid }"
            @click="selectedInvestigator = iid"
            type="button"
          >{{ selectedInvestigator === iid ? 'Selected' : 'Select' }}</button>
        </div>
      </div>
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

/* ── File picker ── */
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

/* ── Mode pill toggle ── */
.mode-section {
  display: flex;
  flex-direction: column;
  gap: 8px;
}

.mode-description {
  font-size: 0.85em;
  color: var(--title);
  opacity: 0.65;
  margin: 0;
}

.mode-picker {
  display: flex;
  border-radius: 4px;
  overflow: hidden;
  border: 1px solid var(--box-border);
}

.mode-picker input[type="radio"] {
  display: none;
}

.mode-picker label {
  flex: 1;
  text-align: center;
  padding: 8px 12px;
  font-size: 0.85em;
  text-transform: uppercase;
  letter-spacing: 0.06em;
  color: var(--title);
  opacity: 0.55;
  cursor: pointer;
  user-select: none;
  transition: background 0.15s, opacity 0.15s;

  &:first-of-type {
    border-right: 1px solid var(--box-border);
  }

  &:hover {
    opacity: 0.85;
    background: rgba(255, 255, 255, 0.04);
  }
}

.mode-picker input[type="radio"]:checked + label {
  background: var(--spooky-green);
  color: #fff;
  opacity: 1;
}

/* ── Investigator picker ── */
.seat-picker {
  display: flex;
  flex-direction: column;
  gap: 10px;
}

.seat-label {
  color: var(--title);
  text-transform: uppercase;
  font-size: 0.8em;
  letter-spacing: 0.08em;
  opacity: 0.7;
  margin: 0;
}

/* ── Investigator select rows ── */
.investigator-rows {
  display: flex;
  flex-direction: column;
  gap: 6px;
}

.investigator-row {
  display: flex;
  align-items: center;
  gap: 12px;
  padding: 8px 10px;
  border-radius: 8px;
  background: rgba(255, 255, 255, 0.04);
  border: 1px solid rgba(255, 255, 255, 0.07);
  transition: background 0.15s, border-color 0.15s;

  &.selected {
    background: rgba(110, 134, 64, 0.12);
    border-color: rgba(110, 134, 64, 0.45);
  }
}

.portrait-wrap {
  width: 52px;
  height: 52px;
  border-radius: 6px;
  overflow: hidden;
  flex-shrink: 0;
}

.investigator-portrait {
  width: 150px;
  display: block;
  margin-left: -18px;
  margin-top: -4px;
}

.claim-btn {
  margin-left: auto;
  padding: 7px 16px;
  background: rgba(0, 0, 0, 0.3);
  border: 1px solid rgba(255, 255, 255, 0.12);
  color: var(--title);
  border-radius: 6px;
  cursor: pointer;
  font-size: 0.85em;
  white-space: nowrap;
  transition: background 0.15s, border-color 0.15s, color 0.15s;

  &:hover {
    background: rgba(255, 255, 255, 0.06);
  }

  &.claimed {
    background: rgba(110, 134, 64, 0.85);
    border-color: rgba(110, 134, 64, 0.6);
    color: white;
  }
}

/* ── Error & actions ── */
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
