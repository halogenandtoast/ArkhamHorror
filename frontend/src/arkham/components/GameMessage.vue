<script lang="ts">
import { defineComponent, h } from 'vue';
import { cardArt } from '@/arkham/cardImages';
import { Game } from '@/arkham/types/Game';
import { handleEmbeddedI18n } from '@/arkham/i18n';
import { chaosTokenImage } from '@/arkham/types/ChaosToken';

export default defineComponent({
  props: {
    game: { type: Object as () => Game, required: true },
    msg: { type: String, required: true },
  },
  render() {
    const msg = handleEmbeddedI18n(this.msg, this.$t)
      // Logs written before custom token formatting was fixed contain the
      // Haskell constructor and an extra pair of quotes. Keep saved logs
      // renderable while new entries use the canonical homebrew slug.
      .replace(/\{token:"CustomToken "([^"]+)""\}/g, '{token:"$1"}')
    const splits = msg.split(/({[^}]+})/)
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
            return name ? h('span', { 'data-image-id': investigatorId, 'class': 'card--sideways' }, name.replace(/\\"/g, "\"")) : split
          }
        }
      } else if (/{enemy:"((?:[^"]|\\.)+)":(.+):"([^"]+)"}/.test(split)) {
        const found = split.match(/{enemy:"((?:[^"]|\\.)+)":(.+):"([^"]+)"}/)
        if (found) {
          const [, name, , cardCode ] = found
          if (cardCode) {
            return name ? h('span', { 'data-image-id': cardCode }, name.replace(/\\"/g, "\"")) : split
          }
        }
      } else if (/{location:"((?:[^"]|\\.)+)":(.+):"([^"]+)"}/.test(split)) {
        const found = split.match(/{location:"((?:[^"]|\\.)+)":(.+):"([^"]+)"}/)
        if (found) {
          const [, name, locationId, cardCode ] = found
          const location = this.game.locations[locationId]

          if (location) {
            const actualCardCode = cardArt(location.cardCode, location.revealed ? '' : 'b')
            return name ? h('span', { 'data-image-id': actualCardCode }, name.replace(/\\"/g, "\"")) : split
          }

          if (cardCode) {
            return name ? h('span', { 'data-image-id': cardCode }, name.replace(/\\"/g, "\"")) : split
          }

          return name ? h('span', { 'data-image-id': cardCode }, name.replace(/\\"/g, "\"")) : split
        }
      } else if (/{location:"((?:[^"]|\\.)+)":(.+)}/.test(split)) {
        const found = split.match(/{location:"((?:[^"]|\\.)+)":(.+)}/)
        if (found) {
          const [, name, locationId ] = found
          if (locationId) {
            return name ? h('span', { 'data-image-id': locationId }, name.replace(/\\"/g, "\"")) : split
          }
        }
      } else if (/{token:"([^"]+)"}/.test(split)) {
        const found = split.match(/{token:"([^"]+)"}/)
        if (found) {
          const [, token] = found
          if (token) {
            return h('img', { 'src': chaosTokenImage(token), 'width': '23', 'class': 'chaos-token' })
          }
        }
      }
      return split
    })

    return h('div', { className: 'message-body' }, els)
  },
})
</script>

<style scoped>
span[data-image-id] {
  color: #BBB;
  cursor: pointer;
}

img.chaos-token {
  display: inline-block;
  vertical-align: text-top;
}
</style>
