<script lang="ts" setup>
import * as Arkham from '@/arkham/types/Game'
import { LogContents, formatKey, logContentsDecoder } from '@/arkham/types/Log'
import { imgsrc, toCapitalizedWords } from '@/arkham/helpers'
import { computed, ref, onMounted, watch } from 'vue'
import { fetchCard } from '@/arkham/api'
import type { CardDef } from '@/arkham/types/CardDef'
import { type Name, simpleName } from '@/arkham/types/Name'
import { scenarioToI18n, type Remembered } from '@/arkham/types/Scenario'
import LogIcons from '@/arkham/components/LogIcons.vue'
import Calendar from '@/arkham/components/TheScarletKeys/Calendar.vue'
import KeysStatus from '@/arkham/components/TheScarletKeys/KeysStatus.vue'
import WorldMap from '@/arkham/components/TheScarletKeys/WorldMap.vue'
import Supplies from '@/arkham/components/Supplies.vue'
import XpBreakdown from '@/arkham/components/XpBreakdown.vue'
import InvestigatorRow from '@/arkham/components/InvestigatorRow.vue'
import { useI18n } from 'vue-i18n'
import { Seal } from '@/arkham/types/Seal'
import { useDbCardStore } from '@/stores/dbCards'

export interface Props {
  game: Arkham.Game
  cards: CardDef[]
  playerId: string
}

const props = defineProps<Props>()
const store = useDbCardStore()
const { t } = useI18n()


// --- Utilities -----------------------------------------------------------------
const EMPTY_LOG: LogContents = { recorded: [], recordedSets: [], recordedCounts: [], partners: {} as any }

const fullName = (name: Name): string => (name.subtitle ? `${name.title}: ${name.subtitle}` : name.title)

const isSeal = (key: string): boolean => ['edgeOfTheEarth.key.sealsRecovered', 'edgeOfTheEarth.key.sealsPlaced'].includes(key)

const sealImage = (seal: Seal): string => {
  const revealed = seal.sealActive ? 'active' : 'dormant'
  switch (seal.sealKind) {
    case 'SealA': return imgsrc(`seals/seal-a-${revealed}.png`)
    case 'SealB': return imgsrc(`seals/seal-b-${revealed}.png`)
    case 'SealC': return imgsrc(`seals/seal-c-${revealed}.png`)
    case 'SealD': return imgsrc(`seals/seal-d-${revealed}.png`)
    case 'SealE': return imgsrc(`seals/seal-e-${revealed}.png`)
  }
}

const time = computed(() => selectedLog.value.recordedCounts.find((r) => {
  return (r[0].tag === 'TheScarletKeysKey' && r[0].contents === 'Time')
}))

const theta = computed(() => props.game.campaign?.meta?.theta)
const scarletKeys = computed(() => props.game.campaign?.meta?.keyStatus)

const setClass = (key: string): string => key.split('.').pop() || ''

// --- Determine available logs & titles -----------------------------------------
const mainLog = computed<LogContents>(() => props.game.campaign?.log || props.game.scenario?.standaloneCampaignLog || EMPTY_LOG)

const dreamModeTitle = computed(() => {
  const mode = props.game.campaign?.meta?.currentCampaignMode
  if (!mode) return null
  return mode === 'TheDreamQuest' ? 'The Dream-Quest' : 'The Web of Dreams'
})

const otherModeTitle = computed(() => {
  const title = dreamModeTitle.value
  if (!title) return null
  return title === 'The Dream-Quest' ? 'The Web of Dreams' : 'The Dream-Quest'
})

// decode the counterpart log if present (Dream Eaters A/B split)
const otherLog = ref<LogContents | null>(null)
if (props.game.campaign?.meta?.otherCampaignAttrs?.log) {
  logContentsDecoder
    .decodePromise(props.game.campaign.meta.otherCampaignAttrs.log)
    .then(res => { otherLog.value = res })
    .catch(() => { otherLog.value = null })
}

// A mapping of title → LogContents. When there is no split, we expose just the main one.
const logMap = computed<Record<string, LogContents>>(() => {
  const title = dreamModeTitle.value
  const other = otherModeTitle.value
  if (title && other) {
    return {
      [title]: mainLog.value,
      [other]: otherLog.value ?? EMPTY_LOG,
    }
  }
  // Fallback single-title mapping (label with campaign/scenario name)
  const singleTitle = props.game.name || 'Campaign Log'
  return { [singleTitle]: mainLog.value }
})

const logTitles = computed(() => Object.keys(logMap.value).sort())

// Selected title drives which log we show. We NEVER compare log objects!
const selectedTitle = ref<string>(logTitles.value[0] ?? '')
watch(logTitles, (titles) => { if (!titles.includes(selectedTitle.value)) selectedTitle.value = titles[0] ?? '' })

const selectedLog = computed<LogContents>(() => logMap.value[selectedTitle.value] ?? mainLog.value)

// --- Investigators shown depend on which half is selected -----------------------
const investigators = computed(() => {
  // If the selected title corresponds to the main log's title, show main investigators; else others
  const mainTitle = dreamModeTitle.value ?? logTitles.value[0]
  const showingMain = selectedTitle.value === mainTitle
  return showingMain ? Object.values(props.game.investigators) : Object.values(props.game.otherInvestigators)
})

// --- Remembered (scenario-only) -------------------------------------------------
const remembered = computed(() => {
  const log = props.game.scenario?.log
  if (!log || !props.game.scenario) return [] as string[]
  const prefix = scenarioToI18n(props.game.scenario)
  return log.map((record: Remembered) => {
    if (record.tag == 'YouOweBiancaResources') return `You owe Bianca resources (${record.contents})`
    if (record.tag === 'RememberedName') {
      return t(`${prefix}.remembered.${record.actualTag.charAt(0).toLowerCase() + record.actualTag.slice(1)}`, { name: simpleName(record.name) })
    }
    return t(`${prefix}.remembered.${record.tag.charAt(0).toLowerCase() + record.tag.slice(1)}`)
  })
})

// --- XP breakdowns --------------------------------------------------------------
const breakdowns =
  props.game.campaign?.xpBreakdown ||
  (props.game.scenario && props.game.scenario.xpBreakdown ? [[{ tag: 'ScenarioStep', contents: props.game.scenario.id } as any, props.game.scenario.xpBreakdown]] : undefined) ||
  []

// --- Derived fields from the currently selected log -----------------------------
const recorded = computed(() => selectedLog.value.recorded.filter(r => !['Teachings1', 'Teachings2', 'Teachings3'].includes(r.tag)).map(formatKey))
const recordedSets = computed(() => selectedLog.value.recordedSets)
const recordedCounts = computed(() => selectedLog.value.recordedCounts.filter((r) => {
  return (r[0].tag !== 'TheScarletKeysKey' && r[0].contents !== 'Time')
}))
const partners = computed(() => (selectedLog.value as any).partners ?? {})
const hasSupplies = computed(() => Object.values(investigators.value).some(i => i.supplies.length > 0))

// --- Card loading for recordedSets ---------------------------------------------
const loadedCards = ref<CardDef[]>([])
const NON_CARD_KEYS = new Set([
  'theCircleUndone.key.mementosDiscovered',
  'theInnsmouthConspiracy.key.memoriesRecovered',
  'theInnsmouthConspiracy.key.possibleSuspects',
  'theInnsmouthConspiracy.key.possibleHideouts',
  'theInnsmouthConspiracy.key.outForBlood',
  'edgeOfTheEarth.key.suppliesRecovered',
  'edgeOfTheEarth.key.sealsPlaced',
  'edgeOfTheEarth.key.sealsRecovered',
])

const findCard = (cardCode: string): CardDef | undefined => props.cards.find(c => c.cardCode === cardCode) || loadedCards.value.find(c => c.cardCode === cardCode)

async function loadMissingCards() {
  const missing = new Set<string>()
  for (const [key, setValues] of Object.entries(recordedSets.value)) {
    if (NON_CARD_KEYS.has(key)) continue
    for (const val of setValues as any[]) {
      const code = (val as any).contents
      if (code && !findCard(code)) missing.add(code)
    }
  }
  if (missing.size === 0) return
  const fetched = await Promise.all(Array.from(missing).map(code => fetchCard(code.replace(/^c/, ''))))
  loadedCards.value.push(...fetched)
}

onMounted(loadMissingCards)
watch([recordedSets, selectedTitle], loadMissingCards)

// --- Display helpers ------------------------------------------------------------
const displayRecordValue = (key: string, value: any): string => {
  const contents: string | undefined = value.contents || value.recordVal?.contents

  if (key === 'theCircleUndone.key.mementosDiscovered') return contents ? toCapitalizedWords(contents) : ''

  if (key === 'theInnsmouthConspiracy.key.memoriesRecovered' && contents) {
    const memory = contents.charAt(0).toLowerCase() + contents.slice(1)
    return t(`theInnsmouthConspiracy.memoriesRecovered.${memory}`)
  }

  if (key === 'theInnsmouthConspiracy.key.outForBlood' && contents) {
    const suspect = contents.charAt(0).toLowerCase() + contents.slice(1)
    return t(`theInnsmouthConspiracy.possibleSuspects.${suspect}`, suspect)
  }

  if (key === 'theInnsmouthConspiracy.key.possibleSuspects' && contents) {
    const suspect = contents.charAt(0).toLowerCase() + contents.slice(1)
    return t(`theInnsmouthConspiracy.possibleSuspects.${suspect}`, suspect)
  }

  if (key === 'theInnsmouthConspiracy.key.possibleHideouts' && contents) {
    const hideout = contents.charAt(0).toLowerCase() + contents.slice(1)
    return t(`theInnsmouthConspiracy.possibleHideouts.${hideout}`, hideout)
  }

  if (key === 'edgeOfTheEarth.key.suppliesRecovered' && contents) {
    const supply = contents.charAt(0).toLowerCase() + contents.slice(1)
    return t(`edgeOfTheEarth.suppliesRecovered.${supply}`, supply)
  }

  if (isSeal(key)) return ''

  const code = contents
  return code ? cardCodeToTitle(code) : ''
}

const cardCodeToShortTitle = (cardCode: string): string => {
  const title = cardCodeToTitle(cardCode)
  return title.includes(':') ? title.split(':')[0] : title
}

const cardCodeToTitle = (cardCode: string): string => {
  const card = findCard(cardCode)
  const language = localStorage.getItem('language') || 'en'

  if (language !== 'en') {
    const code = card ? card.art : cardCode.replace(/^c/, '')
    const dbCard = store.getDbCard(code)
    if (dbCard) return dbCard.subname ? `${dbCard.name}: ${dbCard.subname}` : dbCard.name
  }

  if (card) return fullName(card.name)
  if (cardCode === 'c01121b') return 'The Masked Hunter'
  if (cardCode === 'c50026b') return 'Narōgath'
  return 'unknown'
}

// --- UI helpers -----------------------------------------------------------------
const emptyLog = computed(() => {
  if (logTitles.value.length > 0) return false
  if (hasSupplies.value) return false
  if (recorded.value.length > 0) return false
  if (remembered.value.length > 0) return false
  if (Object.entries(recordedSets.value).length > 0) return false
  return true
})

const bonusXp = computed(() => props.game.campaign?.meta?.bonusXp ?? null)

const mapData = computed(() => {
  const current = props.game.campaign?.meta?.currentLocation || 'London'
  const unlocked = props.game.campaign?.meta?.unlockedLocations || []
  return { current, hasTicket: false, available: unlocked, locations: [
    ['Alexandria', { unlocked: false }],
    ['Anchorage', { unlocked: false }],
    ['Arkham', { unlocked: false }],
    ['Bermuda', { unlocked: false }],
    ['BermudaTriangle', { unlocked: false }],
    ['Bombay', { unlocked: false }],
    ['BuenosAires', { unlocked: false }],
    ['Cairo', { unlocked: false }],
    ['Constantinople', { unlocked: false }],
    ['Havana', { unlocked: false }],
    ['HongKong', { unlocked: false }],
    ['Kabul', { unlocked: false }],
    ['Kathmandu', { unlocked: false }],
    ['KualaLumpur', { unlocked: false }],
    ['Lagos', { unlocked: false }],
    ['London', { unlocked: false }],
    ['Manokwari', { unlocked: false }],
    ['Marrakesh', { unlocked: false }],
    ['MonteCarlo', { unlocked: false }],
    ['Moscow', { unlocked: false }],
    ['Nairobi', { unlocked: false }],
    ['NewOrleans', { unlocked: false }],
    ['Perth', { unlocked: false }],
    ['Quito', { unlocked: false }],
    ['Reykjavik', { unlocked: false }],
    ['RioDeJaneiro', { unlocked: false }],
    ['Rome', { unlocked: false }],
    ['SanFrancisco', { unlocked: false }],
    ['SanJuan', { unlocked: false }],
    ['Shanghai', { unlocked: false }],
    ['Stockholm', { unlocked: false }],
    ['Sydney', { unlocked: false }],
    ['Tokyo', { unlocked: false }],
    ['Tunguska', { unlocked: false }],
    ['Venice', { unlocked: false }],
    ['YborCity', { unlocked: false }],
  ]}
})
</script>


<template>
  <LogIcons />
  <div class="content column">
    <div class="investigators-log">
      <InvestigatorRow v-for="investigator in investigators" :key="investigator.id" :investigator="investigator" :game="game" :bonus-xp="bonusXp && bonusXp[investigator.id]" />
    </div>
    <div v-if="time || scarletKeys" class="column">
      <div class="world-map-container">
        <WorldMap :game="game" :playerId="playerId" :mapData="mapData" :embark="false" />
      </div>
      <div class="scarlet-keys">
        <Calendar v-if="time" :time="time" :theta="theta" />
        <KeysStatus v-if="scarletKeys" :keys="scarletKeys" :toTitle="cardCodeToShortTitle" />
      </div>
    </div>

    <div class="campaign-log column">
      <h1>Campaign Log: {{ game.name }}</h1>

      <div v-if="emptyLog" class="box">No entries yet.</div>


      <div v-if="remembered.length > 0" class="remembered box">
        <h3 class="title">Remembered</h3>
        <ul>
          <li v-for="record in remembered" :key="record">{{ record }}</li>
        </ul>
      </div>

      <div class="log-categories">
        <div v-if="logTitles.length > 1" class="options">
          <div v-for="title in logTitles" :key="title" class="log-title-option" :class="{ checked: title === selectedTitle }">
            <input
              name="log"
              type="radio"
              v-model="selectedTitle"
              :value="title"
              :id="`log${title}`"
            />
            <label :for="`log${title}`">{{ title }}</label>
          </div>
        </div>

        <div v-if="hasSupplies" class="supplies-container">
          <h2>{{ t('theForgottenAge.supplies.title') }}</h2>
          <div class="supplies-content">
            <Supplies v-for="i in investigators" :key="i.id" :player="i">
              <template #heading>
                <h3>{{ i.name.title }}</h3>
              </template>
            </Supplies>
          </div>
        </div>

        <div v-if="recorded.length > 0" class="box">
          Campaign Notes
          <ul>
            <li v-for="record in recorded" :key="record">{{ t(record) }}</li>
            <template v-for="i in investigators" :key="i.id">
              <li v-for="record in i.log.recorded" :key="`${i.id}${record}`">{{ fullName(i.name) }} {{ t(formatKey(record)) }}.</li>
            </template>
          </ul>
        </div>

        <ul>
          <li v-for="[setKey, setValues] in Object.entries(recordedSets)" :key="setKey">{{ t(setKey) }}
            <ul :class="setClass(setKey)">
              <li v-if="isSeal(setKey)" v-for="setValue in setValues"><img :src="sealImage(setValue.contents)" class="seal"/></li>
              <li v-else v-for="setValue in setValues" :key="setValue" :class="{ 'crossed-out': setValue.tag === 'CrossedOut', 'circled': setValue.circled }">{{ displayRecordValue(setKey, setValue) }}</li>
            </ul>
          </li>
        </ul>

        <ul>
          <li v-for="[key, value] in recordedCounts" :key="key">{{ t(formatKey(key)) }}: {{ value }}.</li>
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
                  <td v-if="partner.status === 'TheEntity'" class="partner-name"><span class="name"><s>{{ cardCodeToTitle(cCode) }}</s></span><span class='name'>The Entity</span></td>
                  <td v-else class="partner-name"><span class="name">{{ cardCodeToTitle(cCode) }}</span><span class="status-mia" v-if="partner.status === 'Mia'">MIA</span></td>
                  <td>{{ partner.damage }}</td>
                  <td>{{ partner.horror }}</td>
                </tr>
              </tbody>
            </table>
          </div>
        </div>
      </div>
    </div>


    <div v-for="([step, entries], idx) in breakdowns" :key="idx" class="breakdowns">
      <XpBreakdown :game="game" :step="step" :entries="entries" :playerId="playerId" :showAll="true" :investigators="investigators" />
    </div>
  </div>
</template>

<style scoped>
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

.seal {
  max-width: 45px;
}

.sealsPlaced, .sealsRecovered {
  display: flex;
  flex-direction: row;
  gap: 10px;
}

.investigators-log {
  display: flex;
  flex-direction: column;
  gap: 10px;
  place-items: center;
  margin: 20px;
}

.hidden {
  display: none;
}

.log-categories {
  .options {
    gap: 10px;
  }
}

.log-title-option {
  color: white;
  background-color: rgba(255, 255, 255, 0.3);
  padding: 10px;
  flex: 1;
  border-radius: 10px;
  display: flex;
  gap: 10px;
  label {
    flex: 1;
  }
  &.checked {
    background-color: rgba(255, 255, 255, 0.6);
  }
}

.scarlet-keys {
  display: flex;
  max-width: 90vw;
  margin: 0 auto;
  flex-direction: row;
  gap: 20px;
  @media (max-width: 1500px) {
    flex-direction: column;
    align-items: center;
  }
}

.world-map-container {
  margin: 0 auto;
  max-width: 60vw;
}

</style>
