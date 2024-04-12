<script lang="ts" setup>
import { computed, inject, Ref } from 'vue'
import { CardContents, type Card } from '@/arkham/types/Card'
import type { Game } from '@/arkham/types/Game'
import type { AbilityLabel, AbilityMessage, Message } from '@/arkham/types/Message'
import { MessageType} from '@/arkham/types/Message'
import { imgsrc } from '@/arkham/helpers'
import AbilityButton from '@/arkham/components/AbilityButton.vue'
import * as ArkhamGame from '@/arkham/types/Game'

export interface Props {
  game: Game
  card: Card
  playerId: string
  ownerId: string
}

const props = defineProps<Props>()

const investigator = computed(() => Object.values(props.game.investigators).find((i) => i.playerId === props.playerId))
const investigatorId = computed(() => investigator.value?.id)

const cardContents = computed<CardContents>(() =>
  props.card.tag == 'VengeanceCard' ? props.card.contents.contents : props.card.contents)

const id = computed(() => cardContents.value.id)
const choices = computed(() => ArkhamGame.choices(props.game, props.playerId))

const revealed = computed(() => {
  const meta = investigator.value?.meta
  if ("revealedCards" in meta) {
    return Object.values(meta.revealedCards).some((v) => (v as string[]).includes(id.value))
  }

  return false
})

const cardAction = computed(() => {
  return choices.value.findIndex((choice) => {
    if (choice.tag === MessageType.TARGET_LABEL) {
      return choice.target.contents === id.value
    }

    return false
  })
})

const solo = inject<Ref<boolean>>('solo')

function isAbility(v: Message): v is AbilityLabel {
  if (v.tag !== 'AbilityLabel') {
    return false
  }

  const { source } = v.ability;

  if (source.sourceTag === 'ProxySource') {
    if ("contents" in source.source) {
      return source.source.contents === id.value
    }
  } else if (source.tag === 'CardIdSource') {
    return source.contents === id.value
  } else if (source.tag === 'EventSource') {
    return source.contents === id.value
  } else if (source.tag === 'AssetSource') {
    return source.contents === id.value
  } else if (source.tag === 'SkillSource') {
    return source.contents === id.value
  }

  return false
}

const abilities = computed(() => {
  return choices
    .value
    .reduce<AbilityMessage[]>((acc, v, i) => {
      if (isAbility(v)) {
        return [...acc, { contents: v, displayAsAction: false, index: i}];
      }

      return acc;
    }, []);
})

const classObject = computed(() => {
  return {
    'card--can-interact': cardAction.value !== -1,
    // 'card--committed': uncommitCardAction.value !== -1,
  }
})

const cardBack = computed(() => {
  return imgsrc("player_back.jpg")
})

const image = computed(() => {
  const { cardCode } = cardContents.value;
  return imgsrc(`cards/${cardCode.replace('c', '')}.jpg`);
})

/*
const painted = computed(() => {
  return true
})

const canvas = ref<HTMLCanvasElement | null>(null)



watch(canvas, (painted) => {
  let c = canvas.value
  if (c === null || c === undefined) {
    return
  }
  if (painted) {
    const ctx = c.getContext('2d') as CanvasRenderingContext2D
    const img = new Image()
    img.addEventListener('load', () => {
      c.width = img.width
      c.height = img.height
      ctx.drawImage(img, 0, 0, c.width, c.height)
      oilPaintEffect(c, 4, 55);
    })
    img.src = image.value
  }
})



function oilPaintEffect(canvas, radius, intensity) {
    const ctx = canvas.getContext('2d') as CanvasRenderingContext2D
    var width = canvas.width,
        height = canvas.height,
        imgData = ctx.getImageData(0, 0, width, height),
        pixData = imgData.data,
        pixelIntensityCount = [];

    var centerX = canvas.width / 2,
        centerY = canvas.height / 4;

    var intensityLUT = [],
        rgbLUT = [];

    for (var y = 0; y < height; y++) {
        intensityLUT[y] = [];
        rgbLUT[y] = [];
        for (var x = 0; x < width; x++) {
            var idx = (y * width + x) * 4,
                r = pixData[idx],
                g = pixData[idx + 1],
                b = pixData[idx + 2],
                avg = (r + g + b) / 3;

            intensityLUT[y][x] = Math.round((avg * intensity) / 255);
            rgbLUT[y][x] = {
                r: r,
                g: g,
                b: b
            };
        }
    }


    for (y = 0; y < height; y++) {
        for (x = 0; x < width; x++) {
            pixelIntensityCount = [];

            // Find intensities of nearest pixels within radius.
            for (var yy = -radius; yy <= radius; yy++) {
                for (var xx = -radius; xx <= radius; xx++) {
                    if (y + yy > 0 && y + yy < height && x + xx > 0 && x + xx < width) {
                        var intensityVal = intensityLUT[y + yy][x + xx];

                        if (!pixelIntensityCount[intensityVal]) {
                            pixelIntensityCount[intensityVal] = {
                                val: 1,
                                r: rgbLUT[y + yy][x + xx].r,
                                g: rgbLUT[y + yy][x + xx].g,
                                b: rgbLUT[y + yy][x + xx].b
                            }
                        } else {
                            pixelIntensityCount[intensityVal].val++;
                            pixelIntensityCount[intensityVal].r += rgbLUT[y + yy][x + xx].r;
                            pixelIntensityCount[intensityVal].g += rgbLUT[y + yy][x + xx].g;
                            pixelIntensityCount[intensityVal].b += rgbLUT[y + yy][x + xx].b;
                        }
                    }
                }
            }

            pixelIntensityCount.sort(function (a, b) {
                return b.val - a.val;
            });

            var curMax = pixelIntensityCount[0].val,
                dIdx = (y * width + x) * 4;


            const ry = canvas.width / 3;
            const rx = canvas.height / 3.7;

            if ((Math.pow(x - centerX, 2)/Math.pow(rx, 2)) + (Math.pow(y - centerY, 2)/Math.pow(ry, 2)) <= 1) {
              pixData[dIdx] = ~~ (pixelIntensityCount[0].r / curMax);
              pixData[dIdx + 1] = ~~ (pixelIntensityCount[0].g / curMax);
              pixData[dIdx + 2] = ~~ (pixelIntensityCount[0].b / curMax);
              pixData[dIdx + 3] = 255;
            }

        }
    }

    // change this to ctx to instead put the data on the original canvas
    ctx.putImageData(imgData, 0, 0);
}

    <canvas v-show="painted" ref="canvas" class="card" :data-index="id" :data-card-code="cardContents.cardCode" :data-image="image">
    </canvas>
*/

</script>

<template>
  <div class="card-container" :data-index="id" v-if="solo || (investigatorId == ownerId) || revealed">
    <img
      :class="classObject"
      class="card"
      :src="image"
      @click="$emit('choose', cardAction)"
    />

    <AbilityButton
      v-for="ability in abilities"
      :key="ability.index"
      :ability="ability.contents"
      :data-image="image"
      @click="$emit('choose', ability.index)"
      />

  </div>
  <div class="card-container" v-else>
    <img class="card" :src="cardBack" />
  </div>
</template>

<style scoped lang="scss">

.card {
  width: $card-width;
  min-width: $card-width;
  border-radius: 7px;
  box-shadow: 0 3px 6px rgba(0,0,0,0.23), 0 3px 6px rgba(0,0,0,0.53);
  border-radius: 6px;
  display: flex;
  border: 2px solid rgba(0, 0, 0, 0);

  &--can-interact {
    border: 2px solid $select;
    cursor: pointer;
  }

  &--committed {
    margin-top: -10px;
  }
}
</style>
