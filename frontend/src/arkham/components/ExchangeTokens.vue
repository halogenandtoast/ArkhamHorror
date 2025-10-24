<script setup lang="ts">
import { ref, computed } from 'vue'
import { Token } from '@/arkham/types/Token'
import { Source } from '@/arkham/types/Source'
import type { Game } from '@/arkham/types/Game';
import { imgsrc } from '@/arkham/helpers';
import PoolItem from '@/arkham/components/PoolItem.vue';
import { useI18n } from 'vue-i18n';
import { exchangeTokens } from '@/arkham/api';

const props = defineProps<{
  game: Game
  investigator1: string
  investigator1Amount: number
  investigator2: string
  investigator2Amount: number
  token: Token
  source: Source
}>()

const { t } = useI18n()
const amount = ref(0)
const amount1 = computed(() => props.investigator1Amount - amount.value)
const amount2 = computed(() => props.investigator2Amount + amount.value)

const portraitLabelImage = (investigatorId: string) => {
  const player = props.game.investigators[investigatorId]

  if (player.form.tag == "YithianForm") {
    return imgsrc(`portraits/${investigatorId.replace('c', '')}.jpg`)
  }

  if (player.form.tag == "HomunculusForm") {
    return imgsrc(`portraits/${investigatorId.replace('c', '')}.jpg`)
  }

  return imgsrc(`portraits/${player.cardCode.replace('c', '')}.jpg`)
}

async function submit() {
  await exchangeTokens(props.game.id, props.source, props.investigator1, props.investigator2, props.token, amount.value)
}

async function adjustAmount(delta: number) {
  const newAmount = amount.value + delta

  if (props.investigator1Amount - newAmount < 0) return
  if (props.investigator2Amount + newAmount < 0) return

  amount.value = newAmount
}

</script>

<template>
  <div class="exchange-tokens">
    <section class='main'>
      <div class="investigator">
        <img class="portrait" :src="portraitLabelImage(investigator1)"/>
        <span class="amount">{{amount1}}</span>
      </div>
      <div class='controls'>
        <button id="btn__back" @click="adjustAmount(-1)">&larr;</button>
        <div class="item">
          <PoolItem :type="token.toLowerCase()" />
        </div>
        <button id="btn__forward" @click="adjustAmount(1)">&rarr;</button>
      </div>
      <div class="investigator">
        <img class="portrait" :src="portraitLabelImage(investigator2)"/>
        <span class="amount">{{amount2}}</span>
      </div>
    </section>
    <section class='actions'>
      <button class="button close" @click="submit">{{ t('exchange') }}</button>
    </section>
  </div>
</template>

<style scoped>
.exchange-tokens {
  display: flex;
  flex-direction: column;
  align-items: center;

  section {
    display: flex;
    align-items: center;
    justify-content: center;
    gap: 1em;
    width: 100%;
  }

  section.main {
    padding: 2em;
    background: #424242;
  }

  section.actions {
    button {
      width: 100%;
    }
  }
}

.investigator {
  display: flex;
  flex-direction: column;
  align-items: center;
  gap: 0.5em;
  color: whitesmoke;
  font-size: 1.2em;
  font-weight: bold;
  font-family: 'Arno', sans-serif;
  flex-shrink: 0;
  position: relative;
  isolation: isolate;
  padding-bottom: 20px;
  img {
    border: 2px solid transparent;
    background: linear-gradient(#999 0%, #444 100%) border-box;
    width: 60px;
    height: 60px;
    border-radius: 50%;
    object-fit: cover;
    object-position: top;
    /* drop shadow */
    box-shadow: 0 0 10px rgba(0, 0, 0, 0.8);
  }

  .amount {
    box-shadow: 0 0 5px rgba(0, 0, 0, 1);
    z-index: -1;
    position: absolute;
    bottom: 0px;
    color: #4a5844;
    font-family: "Teutonic";
    letter-spacing: 0.1em;
    transform: translateY(calc(100% - 38px));
    background: #96a78d; /*rgba(255, 0, 255, 0.4);*/
    padding: 20px 8px 2px 8px;
    border-radius: 2px;
  }
}

.controls {
  display: flex;
  align-items: center;
  flex-direction: row;
  width: 100%;
  justify-content: center;
  position: relative;
  margin-inline: 2em;
  button {
    position: absolute;
    font-size: 1.1em;
    background: #333;
    color: #555;
    border: none;
    padding: 2px 10px;
    cursor: pointer;
    &:first-of-type {
      border-top-left-radius: 4px;
      border-bottom-left-radius: 4px;
      right: 0;
      transform: translateX(-100%);
    }
    &:last-of-type {
      border-top-right-radius: 4px;
      border-bottom-right-radius: 4px;
      left: 0;
      transform: translateX(100%);
    }
    &:hover {
      color: #CCC;
    }
  }
}

.actions {
  button {
    padding: 0.8em;
    font-size: 1em;
    font-weight: bold;
    background-color: #222;
    color: #CCC;
    border: none;
    border-radius: 0.6em;
    border-top-left-radius: 0;
    border-top-right-radius: 0;
    text-transform: uppercase;
    width: 100%;
    transition: all 0.3s ease;

    &:hover {
      color: white;
    }
  }
}

.item {
  padding: 10px;
  background-color: #333;
  border-radius: 100vw;
  width: fit-content;
  height: auto;
  aspect-ratio: 1 / 1;
  :deep(img) {
    padding: 2px;
  }
}
</style>
