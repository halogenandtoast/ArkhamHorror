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
      if (/{card:"([^"]+)":"([^"]+)":"([^"]+)"}/.test(split)) {
        const found = split.match(/{card:"([^"]+)":"([^"]+)":"([^"]+)"}/)
        return h('span', { 'data-image-id': found[2] }, found[1])
      } else if (/{investigator:"([^"]+)"}/.test(split)) {
        const found = split.match(/{investigator:"([^"]+)"}/)
        return h('span', { 'data-image-id': found[1] }, this.game.currentData.investigators[found[1]].contents.name)
      } else {
        return split
      }
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
