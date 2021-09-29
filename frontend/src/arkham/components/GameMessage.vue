<script lang="ts">
import { defineComponent, h } from 'vue';
import { Game } from '@/arkham/types/Game';

export default defineComponent({
  props: {
    game: { type: Object as () => Game, required: true },
    msg: { type: String, required: true },
  },
  render() {
    const splits = this.msg.split(/({[^}]+})/)
    const els = splits.map(split => {
      if (/{card:"((?:[^"]|\\.)+)":"([^"]+)":"([^"]+)"}/.test(split)) {
        const found = split.match(/{card:"((?:[^"]|\\.)+)":"([^"]+)":"([^"]+)"}/)
        if (found) {
          const [, cardName, cardId] = found
          if (cardName && cardId) {
            return h('span', { 'data-image-id': cardId }, cardName.replace(/\\"/g, "\""))
          }
        }
      } else if (/{investigator:"((?:[^"]|\\.)+)":"([^"]+)"}/.test(split)) {
        const found = split.match(/{investigator:"((?:[^"]|\\.)+)":"([^"]+)"}/)
        if (found) {
          const [, name, investigatorId ] = found
          if (investigatorId) {
            return name ? h('span', { 'data-image-id': investigatorId }, name.replace(/\\"/g, "\"")) : split
          }
        }
      }
      return split
    })

    return h('div', { className: 'message-body' }, els)
  },
})
</script>

<style scoped lang="scss">
span[data-image-id] {
  color: #BBB;
  cursor: pointer;
}
</style>
