<template>
  <div id="edit-game" class="edit-game" v-if="ready">
    <v-jsoneditor v-model="json" :plus="false" height="70vh"></v-jsoneditor>
    <button @click="save">Save</button>
  </div>
</template>

<script lang="ts">
import { Component, Prop, Vue } from 'vue-property-decorator';
import api from '@/api';
import { fetchGameRaw, updateGameRaw } from '@/arkham/api';
import VJsoneditor from 'v-jsoneditor';

@Component({
  components: { VJsoneditor },
})
export default class EditGame extends Vue {
  @Prop(String) readonly gameId!: string;

  private ready = false;
  private socket: WebSocket | null = null;
  private json: string | null = null;

  async mounted() {
    fetchGameRaw(this.gameId).then(({ game }) => {
      this.json = game.currentData;
      this.ready = true;
      this.socket = new WebSocket(`${api.defaults.baseURL}/arkham/games/${this.gameId}`.replace(/https?/, 'ws'));
      this.socket.addEventListener('message', (event) => {
        this.json = JSON.parse(event.data).currentData;
      });
    });
  }

  save() {
    if (this.json) {
      updateGameRaw(this.gameId, this.json);
    }
  }
}
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
