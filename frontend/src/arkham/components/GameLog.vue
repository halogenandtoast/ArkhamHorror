<template>
  <div class="game-log" ref="messages">
    <ul>
      <li v-for="(msg, i) in gameLog" :key="i">{{msg}}</li>
    </ul>
  </div>
</template>

<script lang="ts">
import { defineComponent, watch, ref, toRefs } from 'vue';

export default defineComponent({
  props: {
    gameLog: { type: Array as string[], required: true },
  },
  setup(props) {
    const messages = ref(null)
    const { gameLog } = toRefs(props)

    watch(gameLog, () => {
      console.log("HERE")
      if (messages.value !== null) {
        messages.value.scrollIntoView({ behavior: 'smooth', block: 'end' })
      }
    })

    return {}
  }
})
</script>

<style scoped lang="scss">
.game-log {
  background: white;
  padding: 10px 0;
  height: 200px;
  overflow-y: auto;
}
</style>
