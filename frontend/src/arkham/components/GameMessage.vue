<script lang="ts">
import { defineComponent, h } from 'vue';
import { imgsrc } from '@/arkham/helpers';
import { cardArt } from '@/arkham/cardImages';
import { Game } from '@/arkham/types/Game';
import { handleEmbeddedI18n } from '@/arkham/i18n';

function imageFor(tokenFace: string) {
  switch (tokenFace) {
    case 'PlusOne':
      return imgsrc("chaos-tokens/ct_plus1.png");
    case 'Zero':
      return imgsrc("chaos-tokens/ct_0.png");
    case 'MinusOne':
      return imgsrc("chaos-tokens/ct_minus1.png");
    case 'MinusTwo':
      return imgsrc("chaos-tokens/ct_minus2.png");
    case 'MinusThree':
      return imgsrc("chaos-tokens/ct_minus3.png");
    case 'MinusFour':
      return imgsrc("chaos-tokens/ct_minus4.png");
    case 'MinusFive':
      return imgsrc("chaos-tokens/ct_minus5.png");
    case 'MinusSix':
      return imgsrc("chaos-tokens/ct_minus6.png");
    case 'MinusSeven':
      return imgsrc("chaos-tokens/ct_minus7.png");
    case 'MinusEight':
      return imgsrc("chaos-tokens/ct_minus8.png");
    case 'AutoFail':
      return imgsrc("chaos-tokens/ct_autofail.png");
    case 'ElderSign':
      return imgsrc("chaos-tokens/ct_eldersign.png");
    case 'Skull':
      return imgsrc("chaos-tokens/ct_skull.png");
    case 'Cultist':
      return imgsrc("chaos-tokens/ct_cultist.png");
    case 'Tablet':
      return imgsrc("chaos-tokens/ct_tablet.png");
    case 'ElderThing':
      return imgsrc("chaos-tokens/ct_elderthing.png");
    case 'BlessToken':
      return imgsrc("chaos-tokens/ct_bless.png");
    case 'CurseToken':
      return imgsrc("chaos-tokens/ct_curse.png");
    case 'FrostToken':
      return imgsrc("chaos-tokens/ct_frost.png");
    default: {
      if (tokenFace.includes(':')) {
        const [, campaign, key] = tokenFace.split(':')
        if (campaign && key) return imgsrc(`homebrew/${campaign}/chaos-tokens/${key}.png`)
      }
      return imgsrc("chaos-tokens/ct_blank.png");
    }
  }
}

export default defineComponent({
  props: {
    game: { type: Object as () => Game, required: true },
    msg: { type: String, required: true },
  },
  render() {
    const msg = handleEmbeddedI18n(this.msg, this.$t);
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
            return h('img', { 'src': imageFor(token), 'width': '23', 'class': 'chaos-token' })
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
