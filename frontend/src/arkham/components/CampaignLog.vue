<script lang="ts" setup>
import * as Arkham from '@/arkham/types/Game'
import { LogContents, LogKey, formatKey, logContentsDecoder } from '@/arkham/types/Log'
import { toCapitalizedWords } from '@/arkham/helpers'
import { computed, ref, onMounted, watch, type Component } from 'vue'
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
import CampaignLogSection from '@/arkham/components/CampaignLogSection.vue'
import CampaignLogRecordedSets from '@/arkham/components/CampaignLogRecordedSets.vue'
import CampaignLogInvestigatorSection from '@/arkham/components/CampaignLogInvestigatorSection.vue'
import CampaignLogPartners from '@/arkham/components/CampaignLogPartners.vue'
import { useI18n } from 'vue-i18n'
import { useDbCardStore } from '@/stores/dbCards'

import ResidentNotes from '@/arkham/components/TheFeastOfHemlockVale/ResidentNotes.vue'
import AreasSurveyed from '@/arkham/components/TheFeastOfHemlockVale/AreasSurveyed.vue'

export interface Props {
  game: Arkham.Game
  cards: CardDef[]
  playerId: string
}

const props = defineProps<Props>()
const store = useDbCardStore()
const { t } = useI18n()

const sectionComponentById: Record<string, Component> = {
  motherRachelNotes: ResidentNotes,
  leahAtwoodNotes: ResidentNotes,
  simeonAtwoodNotes: ResidentNotes,
  williamHemlockNotes: ResidentNotes,
  riverHawthorneNotes: ResidentNotes,
  gideonMizrahNotes: ResidentNotes,
  judithParkNotes: ResidentNotes,
  theoPetersNotes: ResidentNotes,
  areasSurveyed: AreasSurveyed,
}

// --- Utilities -----------------------------------------------------------------
const EMPTY_LOG: LogContents = { recorded: [], recordedSets: {} as any, recordedCounts: [], partners: {} as any }

const fullName = (name: Name): string => (name.subtitle ? `${name.title}: ${name.subtitle}` : name.title)


const time = computed(() =>
  selectedLog.value.recordedCounts.find((r) => r[0].tag === 'TheScarletKeysKey' && r[0].contents === 'Time')
)

const theta = computed(() => props.game.campaign?.meta?.theta)
const delta = computed(() => props.game.campaign?.meta?.delta)
const psi = computed(() => props.game.campaign?.meta?.psi)
const scarletKeys = computed(() => props.game.campaign?.meta?.keyStatus)

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
  const singleTitle = props.game.name || 'Campaign Log'
  return { [singleTitle]: mainLog.value }
})

const logTitles = computed(() => Object.keys(logMap.value).sort())

// Selected title drives which log we show. We NEVER compare log objects!
const selectedTitle = ref<string>(logTitles.value[0] ?? '')
watch(logTitles, (titles) => {
  if (!titles.includes(selectedTitle.value)) selectedTitle.value = titles[0] ?? ''
})

const selectedLog = computed<LogContents>(() => logMap.value[selectedTitle.value] ?? mainLog.value)

// --- Investigators shown depend on which half is selected -----------------------
const investigators = computed(() => {
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
      return t(`${prefix}.remembered.${record.actualTag.charAt(0).toLowerCase() + record.actualTag.slice(1)}`, {
        name: simpleName(record.name),
      })
    }
    return t(`${prefix}.remembered.${record.tag.charAt(0).toLowerCase() + record.tag.slice(1)}`)
  })
})

const breakdowns =
  props.game.campaign?.xpBreakdown ||
  (props.game.scenario && props.game.scenario.xpBreakdown
    ? [[{ tag: 'ScenarioStep', contents: props.game.scenario.id } as any, props.game.scenario.xpBreakdown]]
    : undefined) ||
  []

const isRecord = (v: unknown): v is Record<string, unknown> => typeof v === 'object' && v !== null

type SectionContents = { tag: string; contents: string }
type SectionLogKey = { tag: string; contents: SectionContents }

const isSection = (r: LogKey): r is SectionLogKey => {
  if (!('contents' in r)) return false
  if (!isRecord(r.contents)) return false
  return typeof r.contents.tag === 'string' && typeof r.contents.contents === 'string'
}

const lowerFirst = (s: string) => s.slice(0, 1).toLowerCase() + s.slice(1)
const clamp6 = (n: unknown) => Math.max(0, Math.min(6, Math.floor(Number(n) || 0)))

const recorded = computed(() => {
  return selectedLog.value.recorded
    .filter(r => !['Teachings1', 'Teachings2', 'Teachings3'].includes(r.tag))
    .filter((c) => !isSection(c))
    .map(formatKey)
})

type SectionModel = {
  key: string
  id: string
  titleKey: string
  orderKey: string
  records: string[]
  relationshipLevel: number
  component?: Component
}

const relationshipLevelBySectionId = computed<Record<string, number>>(() => {
  const m: Record<string, number> = {}

  for (const [k, value] of selectedLog.value.recordedCounts) {
    const sectionTag = k?.contents?.tag
    const leafTag = k?.contents?.contents
    if (!sectionTag || !leafTag) continue

    const sectionId = lowerFirst(sectionTag)
    if (!/RelationshipLevel$/.test(leafTag)) continue

    m[sectionId] = clamp6(value)
  }

  return m
})

const sectionsFromCounts = computed<Record<string, { baseKey: string; id: string; titleKey: string; orderKey: string }>>(
  () => {
    const m: Record<string, { baseKey: string; id: string; titleKey: string; orderKey: string }> = {}

    for (const [k] of selectedLog.value.recordedCounts) {
      const sectionTag = k?.contents?.tag
      if (!sectionTag) continue

      const baseKey = lowerFirst(k.tag.replace(/Key$/, ''))
      const sectionId = lowerFirst(sectionTag)
      const key = `${baseKey}:${sectionId}`

      if (!m[key]) {
        m[key] = {
          baseKey,
          id: sectionId,
          titleKey: t(`${baseKey}.key['[${sectionId}]'].title`),
          orderKey: t(`${baseKey}.key['[${sectionId}]'].orderKey`),
        }
      }
    }

    return m
  }
)

const sections = computed<SectionModel[]>(() => {
  const byKey: Record<string, SectionModel> = {}

  for (const record of selectedLog.value.recorded.filter(isSection)) {
    const baseKey = lowerFirst(record.tag.replace(/Key$/, ''))
    const sectionId = lowerFirst(record.contents.tag)
    const leaf = lowerFirst(record.contents.contents)
    const recordKey = `${baseKey}.key['[${sectionId}]'].${leaf}`

    const titleKey = t(`${baseKey}.key['[${sectionId}]'].title`)
    const orderKey = t(`${baseKey}.key['[${sectionId}]'].orderKey`)
    const key = `${baseKey}:${sectionId}`

    const existing = byKey[key]
    if (existing) {
      existing.records.push(recordKey)
      continue
    }

    byKey[key] = {
      key,
      id: sectionId,
      titleKey,
      orderKey,
      records: [recordKey],
      relationshipLevel: relationshipLevelBySectionId.value[sectionId] ?? 0,
      component: sectionComponentById[sectionId],
    }
  }

  for (const [key, meta] of Object.entries(sectionsFromCounts.value)) {
    const existing = byKey[key]
    if (existing) {
      existing.relationshipLevel = relationshipLevelBySectionId.value[existing.id] ?? existing.relationshipLevel
      continue
    }

    byKey[key] = {
      key,
      id: meta.id,
      titleKey: meta.titleKey,
      orderKey: meta.orderKey,
      records: [],
      relationshipLevel: relationshipLevelBySectionId.value[meta.id] ?? 0,
      component: sectionComponentById[meta.id],
    }
  }

  return Object.values(byKey).sort((a, b) => a.orderKey.localeCompare(b.orderKey))
})

const recordedSets = computed(() => selectedLog.value.recordedSets as any)
const recordedCounts = computed(() =>
  selectedLog.value.recordedCounts.filter((r) => {
    return (r[0].tag !== 'TheScarletKeysKey' && r[0].contents !== 'Time') && !isSection(r[0])
  })
)

const partners = computed(() => (selectedLog.value as any).partners ?? {})
const hasSupplies = computed(() => Object.values(investigators.value).some(i => i.supplies.length > 0))

// --- Investigator log sections --------------------------------------------------
const isLogEmpty = (log: LogContents | null | undefined): boolean => {
  const l = log ?? EMPTY_LOG
  const hasRecorded = (l.recorded?.length ?? 0) > 0
  const hasCounts = (l.recordedCounts?.length ?? 0) > 0
  const hasSets = Object.entries((l.recordedSets as any) ?? {}).length > 0
  const hasPartners = Object.keys((l as any).partners ?? {}).length > 0
  return !(hasRecorded || hasCounts || hasSets || hasPartners)
}

const investigatorRecorded = (log: LogContents) =>
  (log.recorded ?? [])
    .filter(r => !['Teachings1', 'Teachings2', 'Teachings3'].includes(r.tag))
    .filter(r => !isSection(r))
    .map(formatKey)

const investigatorRecordedCounts = (log: LogContents) =>
  (log.recordedCounts ?? []).filter(([k]) => !isSection(k))

const investigatorRecordedSetsEntries = (log: LogContents) =>
  Object.entries(((log.recordedSets as any) ?? {}) as Record<string, any[]>)

const investigatorLogSections = computed(() => {
  return investigators.value
    .map((i) => {
      const log = i.log ?? EMPTY_LOG
      return {
        investigator: i,
        log,
        recorded: investigatorRecorded(log),
        recordedCounts: investigatorRecordedCounts(log),
        recordedSetsEntries: investigatorRecordedSetsEntries(log),
        empty: isLogEmpty(log),
      }
    })
    .filter(m => !m.empty)
})

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

const findCard = (cardCode: string): CardDef | undefined =>
  props.cards.find(c => c.cardCode === cardCode) || loadedCards.value.find(c => c.cardCode === cardCode)

async function loadMissingCards() {
  const missing = new Set<string>()

  const scanRecordedSets = (sets: any) => {
    for (const [key, setValues] of Object.entries(sets ?? {})) {
      if (NON_CARD_KEYS.has(key)) continue
      for (const val of (setValues as any[]) ?? []) {
        const code = (val as any).contents
        if (code && typeof code === 'string' && !findCard(code)) missing.add(code)
      }
    }
  }

  // campaign-selected log
  scanRecordedSets(recordedSets.value)

  // investigator logs (whichever half is selected)
  for (const i of investigators.value) {
    scanRecordedSets(i.log?.recordedSets)
  }

  if (missing.size === 0) return

  const fetched = await Promise.all(Array.from(missing).map(code => fetchCard(code.replace(/^c/, ''))))
  loadedCards.value.push(...fetched)
}

onMounted(loadMissingCards)
watch([recordedSets, selectedTitle, investigators], loadMissingCards)

// --- Display helpers ------------------------------------------------------------
const isSeal = (key: string): boolean =>
  ['edgeOfTheEarth.key.sealsRecovered', 'edgeOfTheEarth.key.sealsPlaced'].includes(key)

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
  if (Object.entries(recordedSets.value ?? {}).length > 0) return false
  return true
})

const bonusXp = computed(() => props.game.campaign?.meta?.bonusXp ?? null)

const mapData = computed(() => {
  const current = props.game.campaign?.meta?.currentLocation || 'London'
  const unlocked = props.game.campaign?.meta?.unlockedLocations || []
  return {
    current,
    hasTicket: false,
    available: unlocked,
    locations: [
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
    ],
  }
})
</script>

<template>
  <LogIcons />
  <div class="content column">
    <div class="investigators-log">
      <InvestigatorRow
        v-for="investigator in investigators"
        :key="investigator.id"
        :investigator="investigator"
        :game="game"
        :bonus-xp="bonusXp && bonusXp[investigator.id]"
      />
    </div>

    <div v-if="time || scarletKeys" class="column">
      <div class="world-map-container">
        <WorldMap :game="game" :playerId="playerId" :mapData="mapData" :embark="false" />
      </div>
      <div class="scarlet-keys">
        <Calendar v-if="time" :time="time" :theta="theta" :delta="delta" :psi="psi" />
        <KeysStatus v-if="scarletKeys" :keys="scarletKeys" :toTitle="cardCodeToShortTitle" />
      </div>
    </div>

    <div class="log-column">
      <div class="campaign-log column">
        <h1>Campaign Log: {{ game.name }}</h1>

        <div v-if="emptyLog" class="empty-state">No entries yet.</div>

        <CampaignLogSection
          v-if="remembered.length > 0"
          title="Remembered"
          :items="remembered"
        />

        <div class="log-categories">
          <div v-if="logTitles.length > 1" class="options">
            <div
              v-for="title in logTitles"
              :key="title"
              class="log-title-option"
              :class="{ checked: title === selectedTitle }"
            >
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

          <CampaignLogSection
            v-if="recorded.length > 0"
            title="Campaign Notes"
            :items="recorded.map(r => t(r))"
          />

          <!-- Campaign sections -->
          <template v-for="section in sections" :key="section.key">
            <component
              v-if="section.component"
              :is="section.component"
              :sectionId="section.id"
              :prefix="section.titleKey.split('.').slice(0, 1).join('.')"
              :records="section.records"
              :relationshipLevel="section.relationshipLevel"
            />
            <CampaignLogSection
              v-else
              :title="t(section.titleKey)"
              :items="section.records.map(r => t(r))"
            />
          </template>

          <CampaignLogInvestigatorSection
            v-for="m in investigatorLogSections"
            :key="m.investigator.id"
            :name="fullName(m.investigator.name)"
            :recorded="m.recorded"
            :recordedCounts="m.recordedCounts"
            :recordedSetsEntries="m.recordedSetsEntries"
            :displayRecordValue="displayRecordValue"
          />

          <!-- Campaign recorded sets + counts -->
          <CampaignLogRecordedSets
            :entries="Object.entries(recordedSets)"
            :counts="recordedCounts"
            :displayRecordValue="displayRecordValue"
          />

          <CampaignLogPartners
            v-if="Object.values(partners).length > 0"
            :partners="partners"
            :cardCodeToTitle="cardCodeToTitle"
          />
        </div>
      </div>

      <XpBreakdown
        v-for="([step, entries], idx) in breakdowns"
        :key="idx"
        :game="game"
        :step="step"
        :entries="entries"
        :playerId="playerId"
        :showAll="true"
        :investigators="investigators"
        :defaultCollapsed="idx > 0"
      />
    </div>
  </div>
</template>

<style scoped>
/* ── Page ────────────────────────────────────────────────── */

.content {
  overflow: auto;
  width: 100%;
  padding-bottom: 60px;
}

/* ── Investigators ───────────────────────────────────────── */

.investigators-log {
  display: flex;
  flex-direction: column;
  gap: 10px;
  place-items: center;
  padding: 24px 20px 0;
}

/* ── Campaign log ────────────────────────────────────────── */

.log-column {
  width: 80%;
  margin-inline: auto;
  margin-block: 28px;
  display: flex;
  flex-direction: column;
  gap: 20px;
}

.campaign-log {
  font-size: 1rem;
  color: var(--title);
}

h1 {
  font-family: teutonic, sans-serif;
  font-size: 2.2em;
  margin: 0 0 20px;
  padding: 0 0 14px;
  color: var(--title);
  letter-spacing: 0.06em;
  text-transform: uppercase;
  border-bottom: 1px solid rgba(255,255,255,0.08);
}

/* ── Empty state ─────────────────────────────────────────── */

.empty-state {
  padding: 32px;
  text-align: center;
  color: rgba(255,255,255,0.3);
  font-size: 0.9rem;
  font-style: italic;
  background: rgba(255,255,255,0.03);
  border: 1px solid rgba(255,255,255,0.06);
  border-radius: 8px;
}

/* ── Log categories ──────────────────────────────────────── */

.log-categories {
  display: flex;
  flex-direction: column;
  gap: 12px;
}

/* ── Log tabs (Dream Eaters split) ───────────────────────── */

.options {
  display: flex;
  gap: 8px;
}

.log-title-option {
  flex: 1;
  display: flex;
  align-items: center;
  gap: 8px;
  padding: 8px 14px;
  border-radius: 6px;
  background: rgba(255,255,255,0.04);
  border: 1px solid rgba(255,255,255,0.08);
  cursor: pointer;
  transition: background 0.15s, border-color 0.15s;

  input[type="radio"] { accent-color: var(--spooky-green); cursor: pointer; }

  label {
    flex: 1;
    cursor: pointer;
    font-family: teutonic, sans-serif;
    font-size: 1em;
    font-weight: normal;
    letter-spacing: 0.06em;
    color: rgba(255,255,255,0.45);
  }

  &.checked {
    background: rgba(255,255,255,0.10);
    border-color: rgba(255,255,255,0.18);
    label { color: #f0f0f0; }
  }

  &:hover:not(.checked) {
    background: rgba(255,255,255,0.07);
    border-color: rgba(255,255,255,0.12);
  }
}

/* ── Supplies ────────────────────────────────────────────── */

.supplies-container {
  background: var(--box-background);
  border: 1px solid rgba(255,255,255,0.07);
  border-radius: 8px;
  padding: 14px 16px;

  h2 {
    font-family: teutonic, sans-serif;
    font-size: 1.1em;
    font-weight: normal;
    color: rgba(255,255,255,0.5);
    text-transform: uppercase;
    letter-spacing: 0.08em;
    margin: 0 0 12px;
    padding-bottom: 8px;
    border-bottom: 1px solid rgba(255,255,255,0.07);
  }

  h3 {
    font-family: teutonic, sans-serif;
    font-size: 0.95em;
    font-weight: normal;
    color: rgba(255,255,255,0.6);
    letter-spacing: 0.04em;
    margin: 0 0 6px;
  }
}

.supplies-content {
  display: flex;
  flex-direction: row;
  flex-wrap: wrap;
  gap: 12px;
}


/* ── Scarlet Keys / World Map ────────────────────────────── */

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

.hidden { display: none; }
</style>
