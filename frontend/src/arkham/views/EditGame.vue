<template>
  <div id="edit-game" class="edit-game" v-if="ready">
    <v-jsoneditor v-model="json" :plus="false" height="70vh"></v-jsoneditor>
    <button @click="save">Save</button>
  </div>
</template>

<script lang="ts">
import { defineComponent, ref } from 'vue'
import { fetchGameRaw, updateGameRaw } from '@/arkham/api'
import VJsoneditor from 'v-jsoneditor'

export default defineComponent({
  components: { VJsoneditor },
  props: { gameId: { type: String, required: true } },
  setup(props) {
    const ready = ref(false);
    const socket = ref<WebSocket | null>(null);
    const json = ref<string | null>(null);

    fetchGameRaw(props.gameId).then(({ game }) => {
      json.value = game.currentData;
      const baseURL = `${window.location.protocol}//${window.location.hostname}${window.location.port ? `:${window.location.port}` : ''}`;
      socket.value = new WebSocket(`${baseURL}/api/v1/arkham/games/${props.gameId}`.replace(/https/, 'wss').replace(/http/, 'ws'));
      socket.value.addEventListener('message', (event: MessageEvent) => {
        json.value = JSON.parse(event.data).currentData;
      });
      ready.value = true;
    })

    async function save() {
      if (json.value) {
        updateGameRaw(props.gameId, json.value);
      }
    }

    return { save, ready, json }
  }
})
</script>

<style scoped>
.action { border: 5px solid #FF00FF; border-radius: 15px; }

.game {
  width: 100vw;
  height: calc(100vh - 40px);
  display: grid;
  grid-template-rows: min-content 1fr min-content;
}
</style>
