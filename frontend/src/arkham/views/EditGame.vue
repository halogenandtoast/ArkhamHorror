<template>
  <div id="edit-game" class="edit-game">
    <textarea v-model="rawGameMessage"></textarea><br />
    <button @click="save">Save</button>
    <div>{{rawGameMessage}}</div>
  </div>
</template>

<script lang="ts">
import { defineComponent, ref } from 'vue'
import { updateGameRaw } from '@/arkham/api'

export default defineComponent({
  props: { gameId: { type: String, required: true } },
  setup(props) {
    const rawGameMessage = ref("")

    async function save() {
      console.log(rawGameMessage.value)
      const json = JSON.parse(rawGameMessage.value)
      updateGameRaw(props.gameId, json)
    }

    return { rawGameMessage, save }
  }
})
</script>

<style scoped lang="scss">
.action { border: 5px solid #FF00FF; border-radius: 15px; }

.edit-game {
  font-size: 2em;
}

textarea {
  width: 1000px;
  font-size: 2em;
}

button {
  font-size: 2em;
}
</style>
