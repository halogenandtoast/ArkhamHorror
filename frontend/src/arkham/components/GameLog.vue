<template>
  <div class="game-log" ref="messages">
    <ul>
      <li class="log-entry" v-for="(msg, i) in gameLog" :key="i"><GameMessage :game="game" :msg="msg" /></li>
    </ul>
  </div>
</template>

<script lang="ts">
import { defineComponent, watch, ref, toRefs, nextTick, onMounted } from 'vue';
import { Game } from '@/arkham/types/Game';
import GameMessage from '@/arkham/components/GameMessage.vue';

export default defineComponent({
  components: { GameMessage },
  props: {
    game: { type: Object as () => Game, required: true },
    gameLog: { type: Array as () => string[], required: true },
  },
  setup(props) {
    const messages = ref<Element | null>(null)
    const { gameLog } = toRefs(props)

    onMounted(async () => {
      const el = messages.value
      if (el) {
        const child = el.lastElementChild
        if (child) {
          await nextTick()
          child.scrollIntoView(false);
          el.scrollTop = el.scrollTop + 100;
        }
      }
    })

    watch(gameLog, async () => {
      const el = messages.value
      if (el) {
        const child = el.lastElementChild
        if (child) {
          await nextTick()
          child.scrollIntoView(false);
          el.scrollTop = el.scrollTop + 100;
        }
      }
    }, { deep: true })

    return { messages }
  }
})
</script>

<style scoped lang="scss">
.game-log {
  background: #5e7b73;
  width: calc(100% - 20px);
  border-radius: 5px;
  margin: 10px;
  height: 100%;
  padding: 10px 10px;
  overflow-y: auto;
  overflow-x: hidden;
  box-sizing: border-box;
  flex: 1 1 50%;
  ul {
    list-style: none;
    margin: 0;
    padding: 0;
  }
}

.log-entry {
  background: rgba(0, 0, 0, 0.3);
  padding: 10px;
  margin: 10px 0;
  border-radius: 5px;
  color: white;
  font-weight: 700;
}
</style>
