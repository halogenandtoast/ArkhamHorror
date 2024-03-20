<script lang="ts">
import { defineComponent, h } from 'vue';
import { imgsrc } from '@/arkham/helpers';
import { Game } from '@/arkham/types/Game';

function imageFor(tokenFace: string) {
  switch (tokenFace) {
    case 'PlusOne':
      return imgsrc("ct_plus1.png");
    case 'Zero':
      return imgsrc("ct_0.png");
    case 'MinusOne':
      return imgsrc("ct_minus1.png");
    case 'MinusTwo':
      return imgsrc("ct_minus2.png");
    case 'MinusThree':
      return imgsrc("ct_minus3.png");
    case 'MinusFour':
      return imgsrc("ct_minus4.png");
    case 'MinusFive':
      return imgsrc("ct_minus5.png");
    case 'MinusSix':
      return imgsrc("ct_minus6.png");
    case 'MinusSeven':
      return imgsrc("ct_minus7.png");
    case 'MinusEight':
      return imgsrc("ct_minus8.png");
    case 'AutoFail':
      return imgsrc("ct_autofail.png");
    case 'ElderSign':
      return imgsrc("ct_eldersign.png");
    case 'Skull':
      return imgsrc("ct_skull.png");
    case 'Cultist':
      return imgsrc("ct_cultist.png");
    case 'Tablet':
      return imgsrc("ct_tablet.png");
    case 'ElderThing':
      return imgsrc("ct_elderthing.png");
    case 'BlessToken':
      return imgsrc("ct_bless.png");
    case 'CurseToken':
      return imgsrc("ct_curse.png");
    default:
      return imgsrc("ct_blank.png");
  }
}

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

<style scoped lang="scss">
span[data-image-id] {
  color: #BBB;
  cursor: pointer;
}

img.chaos-token {
  display: inline-block;
  vertical-align: text-top;
}
</style>
