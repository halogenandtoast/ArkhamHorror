<script lang="ts" setup>
import { computed, onMounted, onUnmounted, ref } from 'vue'
import { useI18n } from 'vue-i18n'
import { imgsrc } from '@/arkham/helpers'
import { useDebug } from '@/arkham/debug'

const props = defineProps<{
  sectionId: string
  prefix: string
  records: string[]
  relationshipLevel: number
  gameId?: string
}>()

const emit = defineEmits<{ refresh: [] }>()

const { t } = useI18n()
const debug = useDebug()
const residentDebug = ref(false)
const hovering = ref(false)
const shiftHeld = ref(false)

const showDebugToggle = computed(() => residentDebug.value || (hovering.value && shiftHeld.value))
const isDebugging = computed(() => residentDebug.value)

const onKeyDown = (event: KeyboardEvent) => {
  if (event.key === 'Shift') shiftHeld.value = true
}

const onKeyUp = (event: KeyboardEvent) => {
  if (event.key === 'Shift') shiftHeld.value = false
}

onMounted(() => {
  window.addEventListener('keydown', onKeyDown)
  window.addEventListener('keyup', onKeyUp)
})

onUnmounted(() => {
  window.removeEventListener('keydown', onKeyDown)
  window.removeEventListener('keyup', onKeyUp)
})

type ResidentMeta = {
  card: string
  constructor: string
  relationshipTag: string
  notes: string[]
}

const residentMeta: Record<string, ResidentMeta> = {
  motherRachelNotes: {
    card: '10693',
    constructor: 'MotherRachelNotes',
    relationshipTag: 'MotherRachelRelationshipLevel',
    notes: ['MotherRachelIntervened', 'MotherRachelSharedHerDoubts', 'MotherRachelCrossedOut'],
  },
  leahAtwoodNotes: {
    card: '10694',
    constructor: 'LeahAtwoodNotes',
    relationshipTag: 'LeahAtwoodRelationshipLevel',
    notes: ['LeahSawSomethingInTheMine', 'LeahAndSimeonWereReunited', 'LeahSearchedThePearlRuins', 'LeahSharedHerFrustrations', 'LeahIsSearchingForSimeon', 'LeahSharedADance', 'LeahStoodByYou', 'LeahSacrificedThemselvesForTheInvestigators', 'LeahCrossedOut'],
  },
  simeonAtwoodNotes: {
    card: '10695',
    constructor: 'SimeonAtwoodNotes',
    relationshipTag: 'SimeonAtwoodRelationshipLevel',
    notes: ['SimeonSurvived', 'SimeonDisappeared', 'SimeonHatchedAPlan', 'ThePlanIsUnderway', 'SimeonSharedADance', 'SimeonStoodByYou', 'SimeonSacrificedThemselvesForTheInvestigators', 'SimeonCrossedOut'],
  },
  williamHemlockNotes: {
    card: '10696',
    constructor: 'WilliamHemlockNotes',
    relationshipTag: 'WilliamHemlockRelationshipLevel',
    notes: ['WilliamSharedHisLegacy', 'WilliamTookHeart', 'WilliamSharedADance', 'WilliamStoodByYou', 'WilliamIsResolved', 'WilliamSacrificedThemselvesForTheInvestigators', 'WilliamCrossedOut'],
  },
  riverHawthorneNotes: {
    card: '10697',
    constructor: 'RiverHawthorneNotes',
    relationshipTag: 'RiverHawthorneRelationshipLevel',
    notes: ['RiverSharedTheirAmbitions', 'TheSchemeIsInMotion', 'RiverAskedForHelp', 'RiverSharedADance', 'RiverStoodByYou', 'RiverIsReclaimingTheirLegacy', 'RiverSacrificedThemselvesForTheInvestigators', 'RiverCrossedOut'],
  },
  gideonMizrahNotes: {
    card: '10698',
    constructor: 'GideonMizrahNotes',
    relationshipTag: 'GideonMizrahRelationshipLevel',
    notes: ['GideonFoundHisTreasure', 'GideonToldTheStoryOfCaptainHemlock', 'GideonToldTheTaleOfTheAnnabelleLee', 'GideonSharedADance', 'GideonStoodByYou', 'GideonSacrificedThemselvesForTheInvestigators', 'GideonCrossedOut'],
  },
  judithParkNotes: {
    card: '10699',
    constructor: 'JudithParkNotes',
    relationshipTag: 'JudithParkRelationshipLevel',
    notes: ['JudithSharedAGrudge', 'JudithSavedYourAss', 'JudithSharedADance', 'JudithStoodByYou', 'YouBackedJudithUp', 'JudithSacrificedThemselvesForTheInvestigators', 'JudithCrossedOut'],
  },
  theoPetersNotes: {
    card: '10700',
    constructor: 'TheoPetersNotes',
    relationshipTag: 'TheoPetersRelationshipLevel',
    notes: ['TheoReconciledWithHelen', 'TheoDistractedTheBear', 'TheoSharedADance', 'TheoRejectedHisFamily', 'TheoStoodByYou', 'TheoIsHavingSecondThoughts', 'TheoSacrificedThemselvesForTheInvestigators', 'TheoCrossedOut'],
  },
}

const sectionKey = (str: string) => t(`${props.prefix}.key['[${props.sectionId}]'].${str}`)
const lowerFirst = (str: string) => str.slice(0, 1).toLowerCase() + str.slice(1)
const recordKey = (tag: string) => `${props.prefix}.key['[${props.sectionId}]'].${lowerFirst(tag)}`

const residentName = computed(() => sectionKey('name'))

const clampedLevel = computed(() => Math.max(0, Math.min(6, Math.floor(props.relationshipLevel || 0))))

const meta = computed(() => residentMeta[props.sectionId])
const card = computed(() => meta.value ? imgsrc(`cards/${meta.value.card}.avif`) : null)

const presentRecordKeys = computed(() => new Set(props.records))

const validRecords = computed(() => props.records.filter(rk => !rk.endsWith('CrossedOut')))

const noteOptions = computed(() => {
  if (!isDebugging.value || !meta.value) return validRecords.value.map((key) => ({ key, tag: '', checked: true }))

  return meta.value.notes.map((tag) => {
    const key = recordKey(tag)
    return { key, tag, checked: presentRecordKeys.value.has(key) }
  })
})

const crossedOut = computed(() => props.records.some(rk => rk.endsWith('CrossedOut')))

const campaignLogKey = (tag: string) => ({
  tag: 'TheFeastOfHemlockValeKey',
  contents: { tag: meta.value?.constructor, contents: tag },
})

const canDebug = computed(() => isDebugging.value && props.gameId && meta.value)

async function toggleNote(tag: string, checked: boolean) {
  if (!canDebug.value || !props.gameId) return
  await debug.send(props.gameId, {
    tag: checked ? 'CrossOutRecord' : 'Record',
    contents: campaignLogKey(tag),
  })
  emit('refresh')
}

async function changeRelationship(delta: 1 | -1) {
  if (!canDebug.value || !props.gameId || !meta.value) return
  if (delta > 0 && clampedLevel.value >= 6) return
  if (delta < 0 && clampedLevel.value <= 0) return

  await debug.send(props.gameId, {
    tag: delta > 0 ? 'IncrementRecordCount' : 'DecrementRecordCount',
    contents: [campaignLogKey(meta.value.relationshipTag), 1],
  })
  emit('refresh')
}

</script>

<template>
  <div
    class='resident'
    :class="{ 'resident-crossed-out': crossedOut, 'resident-debugging': isDebugging }"
    @mouseenter="hovering = true"
    @mouseleave="hovering = false"
  >
    <div class="resident-card">
      <img v-if="card" :src="card" class="card no-overlay" />
    </div>
    <div class="resident-notes">
      <div class="resident-header">
        <h3 class="name">{{ residentName }}</h3>
        <button
          v-if="showDebugToggle"
          type="button"
          class="resident-debug-toggle"
          :class="{ active: isDebugging }"
          @click="residentDebug = !residentDebug"
        >Debug</button>
      </div>

      <div class="notes">
        <h4 class="notes-title">{{ t('theFeastOfHemlockVale.campaignLog.residents.notes') }}</h4>
        <ul class="notes-list">
          <li
            v-for="note in noteOptions"
            :key="note.key"
            class="note-item"
            :class="{ 'debug-unchecked': isDebugging && !note.checked }"
          >
            <label v-if="isDebugging && note.tag" class="debug-note-toggle">
              <input type="checkbox" :checked="note.checked" @change="toggleNote(note.tag, note.checked)" />
              <span>{{ t(note.key) }}</span>
            </label>
            <template v-else>{{ t(note.key) }}</template>
          </li>
        </ul>
      </div>
      <div class="relationship" v-if="!crossedOut || isDebugging">
        <div class="relationship-title">{{ t('theFeastOfHemlockVale.campaignLog.residents.relationshipLevel') }}</div>
        <button
          v-if="isDebugging"
          type="button"
          class="relationship-debug"
          :disabled="clampedLevel <= 0"
          @click="changeRelationship(-1)"
        >−</button>
        <div class="boxes" role="img">
          <span v-for="n in 6" :key="n" class="relationship-box" :class="{ filled: n <= clampedLevel }" />
        </div>
        <button
          v-if="isDebugging"
          type="button"
          class="relationship-debug"
          :disabled="clampedLevel >= 6"
          @click="changeRelationship(1)"
        >+</button>
      </div>
    </div>
  </div>
</template>

<style scoped>
h3 {
  font-family: Wolgast;
  text-align: center;
}
h4 {
  font-size: 0.9em;
}
.resident {
  --color: #325162;
  display: flex;
  flex-direction: row;
  gap: 2ch;
  color: var(--color);
  background-color: #CDC2B1;
  padding: 1ch;
  border-radius: 3px;
  box-shadow: 0 4px 6px rgba(0, 0, 0, 0.8);
}

.resident-notes {
  display: flex;
  flex-direction: column;
  gap: 12px;
  flex: 1;
}

.top {
  display: flex;
  justify-content: space-between;
  gap: 12px;
  align-items: flex-start;
}

.resident-header {
  position: relative;
  min-height: 1.5em;
}

.name {
  margin: 0;
  font-family: Wolgast, sans-serif;
  text-box-trim: trim-both;
  text-box-edge: text alphabetic;
}

.resident-debug-toggle {
  position: absolute;
  top: 0;
  right: 0;
  appearance: none;
  border: 1px solid #5C757C;
  border-radius: 3px;
  background: rgba(255, 255, 255, 0.25);
  color: var(--color);
  padding: 3px 8px;
  font-size: 0.8em;
  cursor: pointer;
}

.resident-debug-toggle.active {
  background: #6d1f1f;
  border-color: #9d3030;
  color: white;
}

.relationship {
  display: flex;
  flex-direction: row;
  align-items: center;
  justify-content: flex-end;
  gap: 6px;
  border-bottom: 2px solid #88ABB2;
}

.relationship-title {
  font-size: 0.9em;
  opacity: 0.9;
  font-family: Wolgast, sans-serif;
}

.boxes {
  display: flex;
}

.relationship-box {
  width: 1em;
  height: auto;
  aspect-ratio: 1;
  border: 1px solid #5C757C;
  background: rgba(255, 255, 255, 0.08);
  border-right: none;
  /* no right border except last */
  &:last-child {
    border-right: 1px solid #5C757C;
  }

}

.relationship-box.filled {
  background: rgba(255, 255, 255, 0.6);
}

.notes-title {
  font-family: Wolgast, sans-serif;
  height: 1lh;
}

.notes-list {
  margin: 0;
  padding: 0;
  list-style: none;
  display: flex;
  flex-direction: column;
  gap: 8px;
  border-top: 2px solid #88ABB2;
}

.note-item {
  border-bottom: 2px solid #88ABB2;
  padding: 10px;
  margin: 0;
}

.debug-note-toggle {
  display: flex;
  align-items: flex-start;
  gap: 8px;
  cursor: pointer;
}

.debug-unchecked {
  opacity: 0.55;
}

.relationship-debug {
  appearance: none;
  border: 1px solid #5C757C;
  background: rgba(255, 255, 255, 0.2);
  color: var(--color);
  width: 1.5em;
  height: 1.5em;
  line-height: 1;
  cursor: pointer;

  &:disabled {
    opacity: 0.4;
    cursor: not-allowed;
  }
}

.resident-card {
  width: 25%;
  max-width: 200px;
  img {
    border-radius: 0.25em;
  }
}

.resident-crossed-out {
  opacity: 0.5;
  .name {
    text-decoration: line-through;
  }
}

.resident-debugging {
  outline: 2px dashed #6d1f1f;
  opacity: 1;
}
</style>
