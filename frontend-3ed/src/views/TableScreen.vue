<script setup lang="ts">
import { onMounted, ref } from 'vue'
import { errorText } from '@/api'
import { catalog, loadCatalog } from '@/session'
import TableRoom from '@/views/TableRoom.vue'

defineProps<{ id: string }>()
const error = ref('')
async function load() {
  error.value = ''
  try {
    await loadCatalog()
  } catch (e) {
    error.value = errorText(e)
  }
}
onMounted(load)
</script>

<template>
  <TableRoom v-if="catalog" :id="id" :key="id" :catalog="catalog" />
  <div v-else class="screen">
    <template v-if="error">
      <p class="err">{{ error }}</p>
      <button @click="load">Retry</button>
    </template>
    <p v-else class="waiting">Loading…</p>
  </div>
</template>
