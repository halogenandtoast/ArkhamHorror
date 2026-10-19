<script lang="ts" setup>
import { ref, computed } from 'vue'
import { useI18n } from 'vue-i18n'
import { useRouter } from 'vue-router'
import { importGame } from '@/arkham/api'
import type { Investigator } from '@/arkham/types/Investigator'
import type { Game } from '@/arkham/types/Game'
import InvestigatorRow from '@/arkham/components/InvestigatorRow.vue'
import LogIcons from '@/arkham/components/LogIcons.vue'
import { campaignSetIcon, scenarioSetIcon } from '@/arkham/helpers'
import { setGameLocalStorageItem } from '@/arkham/localStorage'

const router = useRouter()
const { t } = useI18n()

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
const gameStub = { campaign: null, scenario: null, investigators: {} } as unknown as Game
const loading = ref(false)
const error = ref<string | null>(null)
/* The file actually sent: compressed, and trimmed when the untrimmed one is
 * bigger than the server will take. Built when the file is chosen so the
 * trimming can be announced before the upload rather than after it. */
const preparedUpload = ref<File | null>(null)
const stepsTotal = ref(0)
const stepsKept = ref<number | null>(null)

const showModePicker = computed(() => investigators.value.length > 1)

const showInvestigatorPicker = computed(() =>
  importMode.value === 'WithFriends' && investigators.value.length > 1
)

const K = 'importGame.'

function formatBytes(bytes: number): string {
  if (bytes < 1024) return `${bytes} B`
  if (bytes < 1024 * 1024) return `${(bytes / 1024).toFixed(0)} KB`
  return `${(bytes / (1024 * 1024)).toFixed(1)} MB`
}

const GZIP_MAGIC = [0x1f, 0x8b]

async function isGzipped(file: File): Promise<boolean> {
  const head = new Uint8Array(await file.slice(0, 2).arrayBuffer())
  return head.length === 2 && head[0] === GZIP_MAGIC[0] && head[1] === GZIP_MAGIC[1]
}

/* The export the game offers admins is already gzipped, and the import handler
 * sniffs the magic bytes either way, so a .gz file has to be readable here too
 * -- otherwise the one file small enough to upload is the one we refuse. */
class UnreadableFile extends Error {}

async function readAsText(file: File): Promise<string> {
  if (!(await isGzipped(file))) return file.text()
  if (typeof DecompressionStream === 'undefined') {
    throw new UnreadableFile(t(`${K}errors.noDecompression`))
  }
  try {
    const stream = file.stream().pipeThrough(new DecompressionStream('gzip'))
    return await new Response(stream).text()
  } catch {
    throw new UnreadableFile(t(`${K}errors.badGzip`))
  }
}

/* Compressing before the upload is what keeps a long campaign importable at
 * all: the proxy caps the request body well below what an uncompressed export
 * of a finished campaign weighs, and JSON this repetitive gives back an order
 * of magnitude. Returns the original file when compression is unavailable. */
async function compressedForUpload(file: File): Promise<File> {
  if (typeof CompressionStream === 'undefined') return file
  if (await isGzipped(file)) return file
  try {
    const stream = file.stream().pipeThrough(new CompressionStream('gzip'))
    const blob = await new Response(stream).blob()
    if (blob.size === 0 || blob.size >= file.size) return file
    return new File([blob], `${file.name}.gz`, { type: 'application/gzip' })
  } catch {
    return file
  }
}

/* Yesod answers `invalidArgs` with {message, errors}; other handlers use
 * errorMsg, and a proxy that rejects the body outright answers with HTML or
 * nothing at all. Pull out whichever one is actually there. */
function serverReason(e: unknown): string | null {
  const data = (e as { response?: { data?: unknown } })?.response?.data
  if (typeof data === 'string') {
    const trimmed = data.trim()
    // An nginx or proxy error page is not a reason worth showing verbatim.
    if (trimmed === '' || trimmed.startsWith('<')) return null
    return trimmed.slice(0, 500)
  }
  if (data && typeof data === 'object') {
    const d = data as { errors?: unknown; message?: unknown; errorMsg?: unknown }
    if (Array.isArray(d.errors) && d.errors.length > 0) {
      return d.errors.filter(x => typeof x === 'string').join('; ').slice(0, 500)
    }
    if (typeof d.errorMsg === 'string' && d.errorMsg !== '') return d.errorMsg.slice(0, 500)
    if (typeof d.message === 'string' && d.message !== '') return d.message.slice(0, 500)
  }
  return null
}

function importErrorMessage(e: unknown, uploadedBytes: number): string {
  const status = (e as { response?: { status?: number } })?.response?.status
  const reason = serverReason(e)

  // 413 is the proxy, and so is a connection dropped mid-upload: nginx closes
  // the socket on an oversized body, which axios surfaces as a network error
  // with no response at all.
  if (status === 413) {
    return t(`${K}errors.tooLarge`, { size: formatBytes(uploadedBytes) })
  }
  if (status === undefined) {
    return t(`${K}errors.unreachable`, { size: formatBytes(uploadedBytes) })
  }
  if (status === 401 || status === 403) return t(`${K}errors.notAuthorized`)
  if (status === 503) return t(`${K}errors.maintenance`)
  if (reason) return t(`${K}errors.rejected`, { reason })
  return t(`${K}errors.statusOnly`, { status })
}

/* What the game's own basic export keeps, so a trimmed upload is exactly a
 * minimal export of the same game rather than some third thing. */
const TRIMMED_STEPS = 30

/* Comfortably under the 5 MB the production proxy accepts (prod.nginxconf), so
 * the trim happens before the upload rather than after a rejected one. Being
 * wrong either way only costs a retry: the ladder in `submit` trims further
 * when the server refuses anyway, and an untrimmed file under this never gets
 * trimmed speculatively. */
const UPLOAD_SOFT_LIMIT = 4 * 1024 * 1024

/* Only production sits behind a proxy that caps the request body: prod.nginxconf
 * allows 5 MB, while local nginx allows 100 MB and `npm run dev` proxies
 * straight to the API. Trimming in dev would throw away undo history that would
 * have imported perfectly well, so dev uploads whatever it was given and lets a
 * real failure speak for itself. */
const TRIMS_LARGE_UPLOADS = import.meta.env.PROD

/* Keeping the highest `keep` steps is exactly what the server does for a basic
 * export, so a trimmed file is a smaller export of the same game. Sorted by
 * `step` rather than sliced off the front: the export happens to write them
 * newest-first, and keeping the wrong end would leave the game with ancient
 * history and no recent undo at all.
 *
 * `currentData` is untouched either way, so the imported game resumes at the
 * same point -- all that is lost is how far back undo can reach. */
function trimSteps(json: any, keep: number): number {
  const steps = json?.campaignData?.steps
  if (!Array.isArray(steps)) return 0
  if (steps.length <= keep) return 0
  const newest = [...steps]
    .sort((a, b) => (b?.step ?? 0) - (a?.step ?? 0))
    .slice(0, keep)
  json.campaignData.steps = newest
  return steps.length - keep
}

async function uploadFor(json: any): Promise<File> {
  return compressedForUpload(
    new File([JSON.stringify(json)], 'arkham-import.json', { type: 'application/json' })
  )
}

/* A proxy that refuses an oversized body answers 413, but it may also just
 * close the connection, which axios reports as a failure with no response at
 * all. Both are worth one more try with less history in the file. */
function mightBeTooLarge(e: unknown): boolean {
  const status = (e as { response?: { status?: number } })?.response?.status
  return status === 413 || status === undefined
}

async function onFileChange(e: Event) {
  const file = (e.target as HTMLInputElement).files?.[0]
  if (!file) return
  selectedFile.value = file
  error.value = null
  investigators.value = []
  selectedInvestigator.value = null
  gamePreview.value = null
  importMode.value = 'Solo'
  preparedUpload.value = null
  stepsTotal.value = 0
  stepsKept.value = null

  if (file.size === 0) {
    error.value = t(`${K}errors.empty`)
    selectedFile.value = null
    return
  }

  let json: any
  try {
    json = JSON.parse(await readAsText(file))
  } catch (e) {
    error.value = e instanceof UnreadableFile ? e.message : t(`${K}errors.notJson`)
    selectedFile.value = null
    return
  }

  if (!json || typeof json !== 'object' || !json.campaignData) {
    error.value = t(`${K}errors.notAnExport`)
    selectedFile.value = null
    return
  }

  try {
    const variant = json.multiplayerVariant ?? json.campaignData?.multiplayerVariant ?? 'Solo'
    const invMap: Record<string, Investigator> = json.campaignData?.currentData?.gameEntities?.investigators ?? {}
    const playerOrder: string[] = json.campaignData?.currentData?.gamePlayerOrder ?? []
    const fallbackIds: string[] = (json.campaignPlayers ?? []).map((id: string) =>
      id.startsWith('c') ? id : `c${id}`
    )
    const orderedIds = playerOrder.length > 0 ? playerOrder : fallbackIds
    const ordered = orderedIds.map(id => invMap[id]).filter(Boolean) as Investigator[]
    investigators.value = ordered.length > 0 ? ordered : Object.values(invMap)
    isMultiplayer.value = variant === 'WithFriends' && investigators.value.length > 1
    if (isMultiplayer.value) importMode.value = 'WithFriends'

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
    error.value = t(`${K}errors.notAnExport`)
    selectedFile.value = null
    return
  }

  const steps = json.campaignData?.steps
  stepsTotal.value = Array.isArray(steps) ? steps.length : 0

  try {
    let upload = await compressedForUpload(file)
    if (TRIMS_LARGE_UPLOADS && upload.size > UPLOAD_SOFT_LIMIT && stepsTotal.value > TRIMMED_STEPS) {
      trimSteps(json, TRIMMED_STEPS)
      upload = await uploadFor(json)
      stepsKept.value = TRIMMED_STEPS
    }
    preparedUpload.value = upload
  } catch {
    // Preparing the upload is an optimisation; the raw file still works.
    preparedUpload.value = file
    stepsKept.value = null
  }
}

async function send(upload: File): Promise<Game> {
  const formData = new FormData()
  formData.append('debugFile', upload)

  if (importMode.value === 'WithFriends') {
    const investigatorId = selectedInvestigator.value ?? (investigators.value.length === 1 ? investigators.value[0].id : null)
    if (investigatorId) {
      formData.append('investigatorId', investigatorId)
    }
  }

  return importGame(formData, importMode.value)
}

async function submit() {
  if (!selectedFile.value) return
  const file = selectedFile.value
  loading.value = true
  error.value = null

  /* What was prepared when the file was chosen, then -- only where a proxy caps
   * the body -- progressively less undo history. The pre-trim guesses the limit
   * from a constant, so a rejection it did not predict is still worth a retry
   * with a smaller file before giving up. */
  const ladder: (number | null)[] = [null]
  if (TRIMS_LARGE_UPLOADS) {
    for (const keep of [TRIMMED_STEPS, 0]) {
      if ((stepsKept.value ?? stepsTotal.value) > keep) ladder.push(keep)
    }
  }

  let upload = preparedUpload.value ?? (await compressedForUpload(file))
  let lastError: unknown = null

  for (const keep of ladder) {
    if (keep !== null) {
      try {
        const json = JSON.parse(await readAsText(file))
        trimSteps(json, keep)
        upload = await uploadFor(json)
        stepsKept.value = keep
      } catch {
        break
      }
    }

    try {
      const game = await send(upload)
      if (importMode.value === 'WithFriends' && investigators.value.length > 1) {
        setGameLocalStorageItem(game.id, 'host', 'true')
        router.push(`/games/${game.id}/claim-seat`)
      } else {
        router.push(`/games/${game.id}`)
      }
      return
    } catch (e) {
      lastError = e
      if (!TRIMS_LARGE_UPLOADS || !mightBeTooLarge(e)) break
    }
  }

  error.value = importErrorMessage(lastError, upload.size)
  loading.value = false
}

const canSubmit = computed(() => {
  if (!selectedFile.value) return false
  if (importMode.value === 'WithFriends' && investigators.value.length > 1 && !selectedInvestigator.value) return false
  return !loading.value
})

defineExpose({
  canSubmit,
  loading,
  selectedFile,
  submit,
})
</script>

<template>
  <LogIcons />
  <div class="import-game box">
    <div class="file-section">
      <label class="file-label">
        <span>{{ selectedFile ? selectedFile.name : $t('importGame.chooseFile') }}</span>
        <input
          type="file"
          accept=".json,.gz,application/json,application/gzip"
          ref="fileInput"
          @change="onFileChange"
          class="hidden-input"
        />
      </label>
    </div>

    <div v-if="gamePreview" class="game-preview">
      <div v-if="gamePreview.scenarioId || gamePreview.campaignId" class="preview-icon">
        <img v-if="gamePreview.scenarioId" :src="scenarioSetIcon(gamePreview.scenarioId)" />
        <img v-else-if="gamePreview.campaignId" :src="campaignSetIcon(gamePreview.campaignId)" />
      </div>
      <div class="preview-info">
        <div class="preview-name">{{ gamePreview.name }}</div>
        <div class="preview-meta">
          <span v-if="gamePreview.scenarioTitle && gamePreview.scenarioTitle !== gamePreview.name" class="preview-scenario">{{ gamePreview.scenarioTitle }}</span>
          <span v-if="gamePreview.difficulty" class="preview-difficulty">{{ gamePreview.difficulty }}</span>
        </div>
      </div>
    </div>

    <!-- Said before the import, not after it: the dropped history is gone by
         the time the game opens, and this is the only chance to mention it. -->
    <p v-if="stepsKept !== null" class="notice">
      <template v-if="stepsKept > 0">{{ $t('importGame.trimmed', { kept: stepsKept, total: stepsTotal }) }}</template>
      <template v-else>{{ $t('importGame.trimmedAll', { total: stepsTotal }) }}</template>
    </p>

    <div v-if="showModePicker" class="mode-section">
      <div class="mode-picker">
        <input type="radio" v-model="importMode" value="Solo" id="mode-solo" />
        <label for="mode-solo">{{ $t('importGame.solo') }}</label>
        <input type="radio" v-model="importMode" value="WithFriends" id="mode-multi" />
        <label for="mode-multi">{{ $t('importGame.multiplayer') }}</label>
      </div>
      <p class="mode-description">
        <template v-if="importMode === 'Solo'">{{ $t('importGame.soloDescription') }}</template>
        <template v-else>{{ $t('importGame.multiplayerDescription') }}</template>
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

.notice {
  color: #d8c48a;
  font-size: 0.85em;
  line-height: 1.4;
  margin: 0;
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
    background: var(--button);
    cursor: not-allowed;
  }
}

.hidden { display: none; }
</style>
