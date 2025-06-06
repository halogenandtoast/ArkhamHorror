<script lang="ts" setup>
import { computed } from 'vue';
import { PickSupplies  } from '@/arkham/types/Question';
import { MessageType } from '@/arkham/types/Message';
import { imgsrc } from '@/arkham/helpers';
import { useI18n } from 'vue-i18n';

const props = defineProps<{
  game: Game,
  question: PickSupplies,
  playerId: string
}>()
const emit = defineEmits(['choose'])
const choose = (idx: number) => emit('choose', idx)
const { t } = useI18n()

const investigatorId = computed(() => Object.values(props.game.investigators).find((i) => i.playerId === props.playerId)?.id)
const pointsRemaining = computed(() => props.question.pointsRemaining)
const supplies = computed(() => props.question.choices.slice(1))
const chosenSupplies = computed(() => {
 return props.question.chosenSupplies.reduce((acc, e) => acc.set(e, (acc.get(e) || 0) + 1), new Map());
})

const portrait = (investigatorId: string) => {
  const player = props.game.investigators[investigatorId]

  if (player.form.tag === "YithianForm") {
    return imgsrc(`portraits/${investigatorId.replace('c', '')}.jpg`)
  }

  if (player.form.tag === "HomunculusForm") {
    return imgsrc(`portraits/${investigatorId.replace('c', '')}.jpg`)
  }

  return imgsrc(`portraits/${player.cardCode.replace('c', '')}.jpg`)
}

</script>
<template>
  <div id="pick-supplies">
    <div class="pick-supplies-top">
      <div class="pick-supplies-portrait">
        <img :src="portrait(investigatorId)" alt="Investigator Portrait" class="portrait" />
      </div>
      <div class="pick-supplies-contents"><ul><li v-html="t('theForgottenAge.prologue.pickSupplies')"></li></ul></div>
    </div>
    <div class="pick-supplies">
      <h2>Pick Supplies ({{pointsRemaining}} points remaining)</h2>

      <div class="supply-choices">
        <template v-for="(choice, index) in supplies" :key="index">
          <template v-if="choice.tag === MessageType.TOOLTIP_LABEL">
            <button @click="choose(index + 1)" v-tooltip="choice.tooltip">{{choice.label}}</button>
          </template>
        </template>
      </div>

      <button class="done-button" @click="choose(0)">Done</button>
    </div>

    <div class="pick-supplies" v-if="chosenSupplies.size > 0">
      <h2>Chosen Supplies</h2>

      <ul>
        <template v-for="([supply, count], index) in chosenSupplies" :key="index">
          <li v-if="count > 1">{{supply}} ({{count}})</li>
          <li v-else>{{supply}}</li>
        </template>
      </ul>
    </div>
  </div>
</template>

<style lang="scss" scoped>
#pick-supplies {
  width: 100%;
  display: flex;
  flex-direction: column;
  gap: 10px;
}

.supply-choices {
  display: grid;
  grid-template-columns: repeat(4, minmax(calc(25% - 20px), 1fr));
  row-gap: 20px;
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
}

.pick-supplies-container {
  background: #DCD6D0;
  box-shadow: inset 0 0 170px rgba(0,0,0,0.5), 1px 1px 3px rgba(0,0,0,0.6);
  width: calc(100% - 20px);
  margin: 0 auto;
  gap: 10px;
  p {
    margin: 0;
    padding: 0;
  }
}

.pick-supplies {
  background: #DCD6D0;
  box-shadow: inset 0 0 170px rgba(0,0,0,0.5), 1px 1px 3px rgba(0,0,0,0.6);
  width: calc(100% - 20px);
  margin: 0 auto;
  border-radius: 10px;
  gap: 10px;
  p {
    margin: 0;
    padding: 0;
  }
  button {
    border-bottom-left-radius: 10px;
    border-bottom-right-radius: 10px;
  }
  h2 {
    margin-block: 10px;
    font-weight: bold;
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

ul {
  list-style: none;
  margin: 0;
  padding: 0;
  margin-bottom: 10px;
}

ul li {
  text-transform: uppercase;
  background: rgba(0, 0, 0, 0.3);
  padding: 10px;
  margin: 0 10px;
  margin-bottom: 5px;
  border-radius: 5px;
  font-weight: 600;
}

.portrait {
  width: 25vw;
  max-width: 200px;
}

.pick-supplies-contents {
  display: flex;
  align-items: center;
  background: #DCD6D0;
  border-radius: 10px;
  box-shadow: inset 0 0 170px rgba(0, 0, 0, 0.5), 1px 1px 3px rgba(0, 0, 0, 0.6);
}

.pick-supplies-portrait {
  align-self: center;
  min-width: fit-content;

  img {
    border-radius: 10px;
    box-shadow: inset 0 0 170px rgba(0, 0, 0, 0.5), 1px 1px 3px rgba(0, 0, 0, 0.6);
  }
}

.pick-supplies-top {
  display: flex;
  gap: 10px;
  border-radius: 10px;
  margin-inline: 10px;
  container-type: inline-size;
  @container (width < 500px) {
    .portrait {
      object-fit: contain;
      max-width: 10vw;
      min-width: 10vw;
      margin: 20px;
    }
  }
  ul {
    list-style-type: "\0059";
    padding-inline: 20px;
    li {
      background: unset;
      text-transform: none;
      font-weight: normal;
      padding-left: 10px;
      margin-left: 10px;
      margin-bottom: 5px;
      text-align: left;

      ul, :deep(ul) {
        margin-block: 10px;
        list-style-type: "\e91a";
        li::marker {
          font-family: "ArkhamIcons";
        }
      }

      :deep(li) {
        padding-left: 10px;
        margin-bottom: 10px;
      }
    }

    li::marker, :deep(li::marker) {
      font-family: "ArkhamSlim";
      color: var(--spooky-green-dark);
      padding-left: 10px;
      margin-left: 10px;
    }
  }
}
</style>
