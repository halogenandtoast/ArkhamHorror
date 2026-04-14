<script lang="ts" setup>
import { ref, computed } from 'vue'
import { useRouter } from 'vue-router'
import { importGame } from '@/arkham/api'
import type { Investigator } from '@/arkham/types/Investigator'
import type { Game } from '@/arkham/types/Game'
import InvestigatorRow from '@/arkham/components/InvestigatorRow.vue'
import LogIcons from '@/arkham/components/LogIcons.vue'
import { imgsrc } from '@/arkham/helpers'

const router = useRouter()

const fileInput = ref<HTMLInputElement | null>(null)
const selectedFile = ref<File | null>(null)
type GamePreview = {
  name: string
  scenarioId: string | null
  scenarioTitle: string | null
  campaignId: string | null
  campaignName: string | null
  difficulty: string | null
}

const investigators = ref<Investigator[]>([])
const isMultiplayer = ref(false)
const importMode = ref<'Solo' | 'WithFriends'>('Solo')
const selectedInvestigator = ref<string | null>(null)
const gamePreview = ref<GamePreview | null>(null)
const gameStub = { campaign: null, scenario: null } as Partial<Game>
const loading = ref(false)
const error = ref<string | null>(null)

const showModePicker = computed(() => investigators.value.length > 1)

const showInvestigatorPicker = computed(() =>
  importMode.value === 'WithFriends' && investigators.value.length > 1
)

async function onFileChange(e: Event) {
  const file = (e.target as HTMLInputElement).files?.[0]
  if (!file) return
  selectedFile.value = file
  error.value = null
  investigators.value = []
  selectedInvestigator.value = null
  gamePreview.value = null
  importMode.value = 'Solo'

  try {
    const text = await file.text()
    const json = JSON.parse(text)
    const variant = json.multiplayerVariant ?? json.campaignData?.multiplayerVariant ?? 'Solo'
    isMultiplayer.value = variant === 'WithFriends'
    if (isMultiplayer.value) importMode.value = 'WithFriends'
    const invMap: Record<string, Investigator> = json.campaignData?.currentData?.gameEntities?.investigators ?? {}
    const playerOrder: string[] = json.campaignData?.currentData?.gamePlayerOrder ?? []
    const fallbackIds: string[] = (json.campaignPlayers ?? []).map((id: string) =>
      id.startsWith('c') ? id : `c${id}`
    )
    const orderedIds = playerOrder.length > 0 ? playerOrder : fallbackIds
    const ordered = orderedIds.map(id => invMap[id]).filter(Boolean) as Investigator[]
    investigators.value = ordered.length > 0 ? ordered : Object.values(invMap)

    const gameMode = json.campaignData?.currentData?.gameMode ?? {}
    const scenario = gameMode.That ?? null
    const campaign = gameMode.This ?? null
    gamePreview.value = {
      name: json.campaignData?.name ?? null,
      scenarioId: scenario?.id ?? null,
      scenarioTitle: scenario?.name?.title ?? null,
      campaignId: campaign?.id ?? null,
      campaignName: typeof campaign?.name === 'string' ? campaign.name : (campaign?.name?.title ?? null),
      difficulty: scenario?.difficulty ?? campaign?.difficulty ?? null,
    }
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

  if (importMode.value === 'WithFriends') {
    const investigatorId = selectedInvestigator.value ?? (investigators.value.length === 1 ? investigators.value[0].id : null)
    if (investigatorId) {
      formData.append('investigatorId', investigatorId)
    }
  }

  try {
    const game = await importGame(formData, importMode.value)
    if (importMode.value === 'WithFriends' && investigators.value.length > 1) {
      localStorage.setItem(`gameHost_${game.id}`, 'true')
      router.push(`/games/${game.id}/claim-seat`)
    } else {
      router.push(`/games/${game.id}`)
    }
  } catch {
    error.value = 'Failed to import the game. Please try again.'
    loading.value = false
  }
}

const canSubmit = computed(() => {
  if (!selectedFile.value) return false
  if (importMode.value === 'WithFriends' && investigators.value.length > 1 && !selectedInvestigator.value) return false
  return !loading.value
})
</script>

<template>
  <LogIcons />
  <div class="import-game box">
    <div class="file-section">
      <label class="file-label">
        <span>{{ selectedFile ? selectedFile.name : 'Click to choose export file…' }}</span>
        <input
          type="file"
          accept="application/json"
          ref="fileInput"
          @change="onFileChange"
          class="hidden-input"
        />
      </label>
    </div>

    <div v-if="gamePreview" class="game-preview">
      <div v-if="gamePreview.scenarioId || gamePreview.campaignId" class="preview-icon">
        <img v-if="gamePreview.scenarioId" :src="imgsrc(`sets/${gamePreview.scenarioId.replace('c', '')}.png`)" />
        <img v-else-if="gamePreview.campaignId" :src="imgsrc(`sets/${gamePreview.campaignId}.png`)" />
      </div>
      <div class="preview-info">
        <div class="preview-name">{{ gamePreview.name }}</div>
        <div class="preview-meta">
          <span v-if="gamePreview.scenarioTitle && gamePreview.scenarioTitle !== gamePreview.name" class="preview-scenario">{{ gamePreview.scenarioTitle }}</span>
          <span v-if="gamePreview.difficulty" class="preview-difficulty">{{ gamePreview.difficulty }}</span>
        </div>
      </div>
    </div>

    <div v-if="showModePicker" class="mode-section">
      <div class="mode-picker">
        <input type="radio" v-model="importMode" value="Solo" id="mode-solo" />
        <label for="mode-solo">Solo</label>
        <input type="radio" v-model="importMode" value="WithFriends" id="mode-multi" />
        <label for="mode-multi">Multiplayer</label>
      </div>
      <p class="mode-description">
        <template v-if="importMode === 'Solo'">You will control all investigators.</template>
        <template v-else>You will receive an invite link so others can join and claim their investigators.</template>
      </p>
    </div>

    <div v-if="investigators.length > 0" class="investigator-rows">
      <InvestigatorRow
        v-for="investigator in investigators"
        :key="investigator.id"
        :investigator="investigator"
        :game="gameStub"
        :show-expand="false"
      >
        <template #back>
          <button
            v-if="showInvestigatorPicker"
            class="select-btn"
            :class="{ selected: selectedInvestigator === investigator.id }"
            @click="selectedInvestigator = investigator.id"
            type="button"
          >{{ selectedInvestigator === investigator.id ? 'Selected' : 'Select' }}</button>
        </template>
      </InvestigatorRow>
    </div>

    <p v-if="error" class="error">{{ error }}</p>

    <div v-if="selectedFile" class="actions">
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

.game-preview {
  display: flex;
  align-items: center;
  gap: 12px;
  padding: 10px 12px;
  background: rgba(255, 255, 255, 0.04);
  border: 1px solid rgba(255, 255, 255, 0.08);
  border-radius: 6px;
}

.preview-icon {
  flex-shrink: 0;
  width: 28px;
  filter: invert(100%) brightness(60%);

  img { width: 100%; display: block; }
}

.preview-info {
  display: flex;
  flex-direction: column;
  gap: 4px;
  min-width: 0;
}

.preview-name {
  font-family: teutonic, sans-serif;
  font-size: 1.1em;
  color: var(--title);
  white-space: nowrap;
  overflow: hidden;
  text-overflow: ellipsis;
}

.preview-meta {
  display: flex;
  gap: 8px;
  align-items: center;
  flex-wrap: wrap;
}

.preview-scenario {
  font-size: 0.8em;
  color: rgba(255, 255, 255, 0.5);
}

.preview-difficulty {
  font-size: 0.75em;
  text-transform: uppercase;
  letter-spacing: 0.06em;
  color: rgba(255, 255, 255, 0.4);
  background: rgba(255, 255, 255, 0.07);
  padding: 2px 7px;
  border-radius: 999px;
}

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

.investigator-rows {
  display: flex;
  flex-direction: column;
  gap: 6px;
}

.select-btn {
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

  &.selected {
    background: rgba(110, 134, 64, 0.85);
    border-color: rgba(110, 134, 64, 0.6);
    color: white;
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

.hidden { display: none; }
</style>
