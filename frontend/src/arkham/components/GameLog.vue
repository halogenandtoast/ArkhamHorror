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
    gameLog: { type: Array as () => string[], required: true },
  },
  setup(props) {
    const messages = ref<Element | null>(null)
    const { gameLog } = toRefs(props)

    watch(gameLog, () => {
      const el = messages.value
      if (el !== null) {
        el.scrollTop = el.scrollHeight
      }
    }, { deep: true })

    return { messages }
  }
})
</script>

<style scoped lang="scss">
.game-log {
  background: white;
  padding: 10px 0;
  max-width: 500px;
  width: 25vw;
  height: calc(100vh - 40px);
  overflow-y: auto;
  overflow-x: hidden;
  display: flex;
  flex-direction: column;
  box-sizing: border-box;
}
</style>
