<script setup lang="ts">
import { ref } from 'vue'
import * as api from '@/api'
import { useGame } from '@/game/context'
import { mainSiteUrl } from '@/session'

const ctx = useGame()
const dialog = ref<HTMLDialogElement | null>(null)
const title = ref('')
const description = ref('')
const busy = ref(false)
const filed = ref<number | null>(null)
const reportUrl = (number: number) => `${mainSiteUrl}/#/reports/${number}`

function download(data: unknown) {
  const url = URL.createObjectURL(new Blob([JSON.stringify(data)], { type: 'application/json' }))
  const a = document.createElement('a')
  a.href = url
  a.download = `ah3e-debug-${ctx.tableId}.json`
  a.click()
  URL.revokeObjectURL(url)
}

async function save() {
  try {
    download(await api.exportTable(ctx.tableId))
  } catch (e) {
    ctx.error.value = api.errorText(e)
  }
}

async function file() {
  busy.value = true
  try {
    const { url } = await api.fileBug(ctx.tableId)
    // the title says which game, since the tracker is shared with Arkham
    const { report } = await api.createReport({
      kind: 'bug',
      title: `[3e] ${title.value}`,
      body: `Arkham Horror 3e\n\n${description.value}\n\ntable: ${window.location.href}\nfile: ${url}`,
    })
    title.value = ''
    description.value = ''
    filed.value = report.number
    dialog.value?.close()
  } catch (e) {
    ctx.error.value = api.errorText(e)
  } finally {
    busy.value = false
  }
}
</script>

<template>
  <button id="debugExport" class="undo-btn" title="Download this table as a debug file" @click="save">Export</button>
  <button id="fileBug" class="undo-btn" title="Report a bug with this game attached" @click="dialog?.showModal()">
    Bug
  </button>
  <dialog ref="dialog" class="bug-dialog">
    <form @submit.prevent="file">
      <h2>File a bug</h2>
      <p>The game state is uploaded with the report, so the bug can be reproduced.</p>
      <p class="waiting">Replies come back on the tracker on the main site.</p>
      <input v-model="title" required placeholder="What went wrong, in a line" />
      <textarea v-model="description" required rows="5" placeholder="What you did, and what you expected"></textarea>
      <div class="bug-buttons">
        <button class="primary" type="submit" :disabled="busy">{{ busy ? 'Uploading…' : 'Submit' }}</button>
        <button type="button" @click="dialog?.close()">Cancel</button>
      </div>
    </form>
  </dialog>
  <dialog :open="filed !== null" class="bug-dialog">
    <h2>Bug filed</h2>
    <p>Filed as report #{{ filed }}.</p>
    <div class="bug-buttons">
      <a v-if="filed !== null" class="button-like primary" :href="reportUrl(filed)" target="_blank" rel="noreferrer">
        Open the report
      </a>
      <button type="button" @click="filed = null">Close</button>
    </div>
  </dialog>
</template>
