<script lang="ts" setup>
import { ref, watch } from 'vue';
import { PickDestiny, DestinyDrawing  } from '@/arkham/types/Question';
import { Game } from '@/arkham/types/Game';
import { imgsrc } from '@/arkham/helpers';
import { useI18n } from 'vue-i18n';
import { tarotCardImage } from '@/arkham/types/TarotCard';
import { setDestiny } from '@/arkham/api';

const props = defineProps<{
  game: Game,
  question: PickDestiny,
  playerId: string
}>()
const { t } = useI18n()
const valid = ref(false)
const drawings = ref(props.question.drawings)

const submit = async () => await setDestiny(props.game.id, drawings.value)

const rotate = (scenario: string) => {
  drawings.value = drawings.value.map((d: DestinyDrawing) => {
    if (d.scenario === scenario) {
      return { ...d, tarot: { ...d.tarot, facing: d.tarot.facing == 'Upright' ? 'Reversed' : 'Upright' } }
    } 

    return d
  })
}

watch(drawings, () => {
  const totalReversed = drawings.value.filter((d : DestinyDrawing) => d.tarot.facing === 'Reversed').length
  valid.value = totalReversed === Math.ceil(drawings.value.length / 2)
})

</script>
<template>
  <div id="pick-destiny-container">
    <h1 class="title">{{t('pickDestiny.title')}}</h1>
    <div id="pick-destiny">
      <p>{{t('pickDestiny.instructions')}}</p>

      <div class="tarot-rows">
        <div v-for="drawing in drawings" :key="drawing.scenario" class="tarot-row">
          <h2>{{t(`destiny.${drawing.scenario}`)}}</h2>
          {{t(`tarot.${drawing.tarot.arcana}`)}}
          <img :src="imgsrc(`tarot/${tarotCardImage(drawing.tarot)}`)" @click="rotate(drawing.scenario)" class="tarot-card" :class="{ [drawing.tarot.facing]: true}" />
        </div>
      </div>

      <button :disabled="!valid" class="done-button" @click="submit">{{t('label.done')}}</button>
    </div>
  </div>
</template>

<style scoped>
#pick-destiny-container {
  box-shadow: inset 0 0 170px rgba(0, 0, 0, 0.5), 1px 1px 3px rgba(0, 0, 0, 0.6);
  border-radius: 5px;
}

#pick-destiny {
  width: 100%;
  display: flex;
  flex-direction: column;
  gap: 10px;
  border-bottom-left-radius: 5px;
  border-bottom-right-radius: 5px;
  background: #DCD6D0;
  padding: 20px;
  box-sizing: border-box;
}

.tarot-rows {
  display: grid;
  grid-template-columns: repeat(4, minmax(calc(25% - 20px), 1fr));
  row-gap: 20px;
  column-gap: 10px;
  @media (max-width: 800px) and (orientation: portrait) {
    grid-template-columns: repeat(3, minmax(calc(34% - 20px), 1fr));
  }
}

h1 {
  border-top-left-radius: 5px;
  border-top-right-radius: 5px;
  padding: 10px;
  color: var(--title);
  font-size: 2em;
  text-transform: uppercase;
  font-family: Teutonic;
  background-color: var(--background-dark);
}


.tarot-row {
  display: flex;
  flex-direction: column;
  align-items: center;
  justify-content: space-between;
  text-align: center;
  gap: 5px;
  font-family: Teutonic, serif;

  h2 {
    text-transform: uppercase;
  }


  img {
    cursor: alias;
    width: auto;
    height: auto;
    aspect-ratio: var(--tarot-aspect);
    max-width: 300px;
    max-height: 30vh;
    border-radius: 10px;
    box-shadow: inset 0 0 170px rgba(0,0,0,0.5), 1px 1px 3px rgba(0,0,0,0.6);
  }
}

button {
  border: 0;
  margin: 0 10px;
  padding: 10px;
  text-transform: uppercase;
  background-color: #532e61;
  font-weight: bold;
  border-radius: 0.6em;
  color: #EEE;
  font: Arial, sans-serif;
  &:hover {
    background-color: #311b3e;
  }
  @media (max-width: 800px) and (orientation: portrait) {
    font-size: smaller;
    margin:0 5px;
  }
}

.done-button {
  margin: 0;
  width: 100%;
  border: 0;
  text-align: center;
  padding: 10px;
  text-transform: uppercase;
  border-radius: 0;
  background-color: #532e61;
  font-weight: bold;
  color: #EEE;
  font: Arial, sans-serif;
  margin-top: 20px;
  &:hover {
    background-color: #311b3e;
  }

  i {
    font-style: normal;
  }
}

h2 {
  text-align: center;
  text-transform: uppercase;
}

.tarot-card {
  transition: all 0.3s ease-in;
  &.Reversed {
    transform: rotateZ(180deg);
    transform-origin: center;
    animation-fill-mode: forwards;
  }
}

button[disabled], button[disabled]:hover {
  background-color: grey;
  cursor: not-allowed;
}
</style>
