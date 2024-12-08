<script lang="ts" setup>
import * as Arkham from '@/arkham/types/Game'
import { LogContents, logContentsDecoder } from '@/arkham/types/Log';
import { computed, ref, onMounted, watch } from 'vue'
import { fetchCard } from '@/arkham/api';
import type { CardDef } from '@/arkham/types/CardDef'
import type { Name } from '@/arkham/types/Name'
import { scenarioToI18n, type Remembered } from '@/arkham/types/Scenario'
import Supplies from '@/arkham/components/Supplies.vue';
import XpBreakdown from '@/arkham/components/XpBreakdown.vue';
import { toCapitalizedWords } from '@/arkham/helpers';
import { useI18n } from 'vue-i18n';

export interface Props {
  game: Arkham.Game
  cards: CardDef[]
  playerId: string
}

const props = defineProps<Props>()

const { t } = useI18n()
const mainLog = props.game.campaign?.log || props.game.scenario?.standaloneCampaignLog || { recorded: [], recordedSets: [], recordedCounts: [] }

const remembered = computed(() => {
  const log = props.game.scenario?.log
  if (!log) return []
  if (!props.game.scenario) return []
  const prefix = scenarioToI18n(props.game.scenario)
  return log.map((record: Remembered) => {
    if (record.tag == 'YouOweBiancaResources') {
      console.log(record);
      return `You owe Bianca resources (${record.contents})`
    } else {
      return t(`${prefix}.remembered.${record.tag.charAt(0).toLowerCase() + record.tag.slice(1)}`)
    }
  })
})

const otherLog = ref<LogContents | null>(null)

if (props.game.campaign?.meta?.otherCampaignAttrs?.log) {
  logContentsDecoder.decodeToPromise(props.game.campaign?.meta?.otherCampaignAttrs?.log).then(res => otherLog.value = res)
}

const breakdowns =
  props.game.campaign?.xpBreakdown ||
    (props.game.scenario && props.game.scenario.xpBreakdown ? [[{ "tag": "ScenarioStep", "contents": props.game.scenario.id }, props.game.scenario.xpBreakdown]] : undefined) ||
    []

const logTitle = props.game.campaign?.meta?.currentCampaignMode ?
  (props.game.campaign.meta.currentCampaignMode === 'TheDreamQuest' ? "The Dream-Quest" : "The Web of Dreams") : null


const otherLogTitle = logTitle ?
  (logTitle === 'The Dream-Quest' ? 'The Web of Dreams' : 'The Dream-Quest') : null

const logTitles = logTitle && otherLogTitle ? [logTitle, otherLogTitle].sort() : null


const campaignLog = ref(mainLog)

const recorded = computed(() => campaignLog.value.recorded)
const recordedSets = computed(() => campaignLog.value.recordedSets)
const recordedCounts = computed(() => campaignLog.value.recordedCounts)
const partners = computed(() => campaignLog.value.partners)
const hasSupplies = computed(() => Object.values(props.game.investigators).some((i) => i.supplies.length > 0))

const loadedCards = ref<CardDef[]>([]);

// Function to load missing cards
async function loadMissingCards() {
  const nonCardKeys = ['MementosDiscovered', 'MemoriesRecovered', 'PossibleSuspects', 'PossibleHideouts', 'SuppliesRecovered'];
  const missingCardCodes = new Set();
  for (const [key, setValue] of Object.entries(recordedSets.value)) {
    if (nonCardKeys.includes(key)) continue;
    for (const val of setValue) { // Assuming setValue is an entry [setKey, setValues]
      const cardCode = val.contents; // Assuming this is how you get the card code
      if (!findCard(cardCode)) {
        missingCardCodes.add(cardCode);
      }
    }
  }

  const missingCardsPromises = Array.from(missingCardCodes).map(code => fetchCard(code.replace(/^c/, '')));
  const fetchedCards = await Promise.all(missingCardsPromises);

  loadedCards.value.push(...fetchedCards);
}

// Invoke the loading function when the component mounts or recordedSets changes
onMounted(loadMissingCards);
watch(recordedSets, loadMissingCards, { deep: true });

const findCard = (cardCode: string): CardDef | undefined => {
  return props.cards.find((c) => c.cardCode == cardCode) || loadedCards.value.find(c => c.cardCode == cardCode);
}

const displayRecordValue = (key: string, value: SomeRecordable): string => {
  if (key === 'MementosDiscovered') {
    const contents = value.contents || value.recordVal?.contents
    return toCapitalizedWords(contents)
  }

  if (key === 'MemoriesRecovered') {
    const contents = value.contents || value.recordVal?.contents
    const memory = contents.charAt(0).toLowerCase() + contents.slice(1)
    return t(`theInnsmouthConspiracy.memoriesRecovered.${memory}`)
  }

  if (key === 'PossibleSuspects') {
    const contents = value.contents || value.recordVal?.contents
    const suspect = contents.charAt(0).toLowerCase() + contents.slice(1)
    return t(`theInnsmouthConspiracy.possibleSuspects.${suspect}`, suspect)
  }

  if (key === 'PossibleHideouts') {
    const contents = value.contents || value.recordVal?.contents
    const hideout = contents.charAt(0).toLowerCase() + contents.slice(1)
    return t(`theInnsmouthConspiracy.possibleHideouts.${hideout}`, hideout)
  }

  if (key === 'SuppliesRecovered') {
    const contents = value.contents || value.recordVal?.contents
    const supply = contents.charAt(0).toLowerCase() + contents.slice(1)
    return t(`edgeOfTheEarth.suppliesRecovered.${supply}`, supply)
  }

  const code = value.contents || value.recordVal?.contents
  return cardCodeToTitle(code)
}

const cardCodeToTitle = (cardCode: string): string => {
  const card = findCard(cardCode)

  if (card) {
    return fullName(card.name)
  }

  if(cardCode == "c01121b") {
    return "The Masked Hunter"
  }

  if(cardCode == "c50026b") {
    return "Narōgath"
  }

  return "unknown"
}

const fullName = (name: Name): string => {
  const subtitle = name.subtitle
  if (subtitle) {
    return `${name.title}: ${subtitle}`
  }

  return name.title
}

const emptyLog = computed(() => {
  if ((logTitles ?? []).length > 0) return false;
  if (hasSupplies.value) return false;
  if (recorded.value.length > 0) return false;
  if (remembered.value.length > 0) return false;
  if (Object.entries(recordedSets.value).length > 0) return false;
  return true;
})

</script>

<template>
  <div class="content column">
    <div class="campaign-log column">
      <h1>Campaign Log: {{game.name}}</h1>
      <div v-if="emptyLog" class="box">
        No entries yet.
      </div>
      <div v-if="remembered.length > 0" class="remembered box">
        <h3 class="title">Remembered</h3>
        <ul>
          <li v-for="record in remembered" :key="record">{{record}}.</li>
        </ul>
      </div>
      <div class="log-categories">
        <div v-if="logTitles" class="options">
          <template v-for="title in logTitles" :key="title">
            <input
              type="radio"
              v-model="campaignLog"
              :value="title === logTitle ? mainLog : otherLog"
              :checked="title === logTitle"
              :id="`log${title}`"
            />
            <label :for="`log${title}`">{{title}}</label>
          </template>
        </div>
        <div v-if="hasSupplies" class="supplies-container">
          <h2>Supplies</h2>
          <div class="supplies-content">
            <Supplies v-for="i in game.investigators" :key="i.id" :player="i">
              <template #heading>
                <h3>{{i.name.title}}</h3>
              </template>
            </Supplies>
          </div>
        </div>
        <div v-if="recorded.length > 0" class="box">
          Campaign Notes
          <ul>
            <li v-for="record in recorded" :key="record">{{t(record)}}.</li>
            <template v-for="i in game.investigators" :key="i.id">
              <li v-for="record in i.log.recorded" :key="`${i.id}${record}`">{{fullName(i.name)}} {{toCapitalizedWords(record).toLowerCase()}}.</li>
            </template>
          </ul>
        </div>
        <ul>
          <li v-for="[setKey, setValues] in Object.entries(recordedSets)" :key="setKey">{{t(setKey)}}
            <ul>
              <li v-for="setValue in setValues" :key="setValue" :class="{ 'crossed-out': setValue.tag === 'CrossedOut', 'circled': setValue.circled }">{{displayRecordValue(setKey, setValue)}}</li>
            </ul>
          </li>
        </ul>
        <ul>
          <li v-for="[key, value] in recordedCounts" :key="key">{{toCapitalizedWords(key)}}: {{value}}.</li>
        </ul>
        <div v-if="Object.values(partners).length > 0" class="partners box">
          <h3 class="title">Expedition Team</h3>
          <div class="partners-content">
            <table>
              <thead>
                <tr>
                  <th></th>
                  <th>Damage</th>
                  <th>Horror</th>
                </tr>
              </thead>
              <tbody>
                <tr v-for="[cCode, partner] in Object.entries(partners)" :key="cCode" class="partner" :class="{ [partner.status]: true }">
                  <td class="partner-name"><span class="name">{{cardCodeToTitle(cCode)}}</span><span class="status-mia" v-if="partner.status === 'Mia'">MIA</span></td>
                  <td>{{partner.damage}}</td>
                  <td>{{partner.horror}}</td>
                </tr>
              </tbody>
            </table>
          </div>
        </div>
      </div>
    </div>

    <div v-for="([step, entries], idx) in breakdowns" :key="idx" class="breakdowns">
      <XpBreakdown :game="game" :step="step" :entries="entries" :playerId="playerId" />
    </div>
  </div>
</template>

<style lang="scss" scoped>
h1 {
  font-family: teutonic, sans-serif;
  margin: 0;
  padding: 0;
  color: var(--title);
  margin-bottom: 10px;
}

.campaign-log {
  width: 80%;
  margin-inline: auto;
  margin-block: 20px;
  font-size: 1.8em;
}

.breakdowns {
  width: 80%;
  margin: 0 auto;
}

.crossed-out {
  text-decoration: line-through;
}

.options {
  display: flex;
  justify-content: space-around;
}

.log-categories {
  display: flex;
  flex-direction: column;
  gap: 10px;
  margin: 0;
  padding: 0;
}

ul {
  display: flex;
  flex-direction: column;
  gap: 10px;
  margin: 0;
  padding: 0;
}

li {
  display: flex;
  flex-direction: column;
  gap: 10px;
  background: var(--box-background);
  border: 1px solid var(--box-border);
  border-radius: 5px;
  padding: 10px;
  color: var(--title);
  margin: 0;

  & ul li {
    background: rgba(255, 255, 255, 0.1);
  }
}

.box {
  & ul li {
    background: rgba(255, 255, 255, 0.1);
  }
}

.content {
  overflow: auto;
  width: 100%;
  padding-bottom: 50px;
}

.supplies-container {
  display: flex;
  flex-direction: column;
  color: var(--title);
}

.supplies-content {
  color: var(--title);
  display: flex;
  flex-direction: row;
  gap: 10px;
}

.circled {
  background: var(--rogue-dark);
}

h3.title {
  color: var(--title);
  font-family: teutonic, sans-serif;
  margin-bottom: 10px;
}

table {
  width: 100%;
  display: grid;
  grid-template-columns: repeat(3, auto);
  gap: 5px;

  thead, tbody, tr {
    display: contents;
  }
}

.partner td {
  background: var(--box-background);
  border: 1px solid var(--box-border);
  border-radius: 5px;
  padding: 10px;
  color: var(--title);
}

tr td:not(:first-child) {
  text-align: right;
}

.Eliminated td {
  text-decoration: line-through;
  background: darkred;
}

.Mia td {
  background: darkgoldenrod;
}

.partner-name {
  display: flex;
  gap: 10px;

  .name {
    flex: 1;
  }
}

.status-mia {
  background: rgba(0, 0, 0, 0.5);
  padding-inline: 5px;
  border-radius: 2px;
}

</style>
