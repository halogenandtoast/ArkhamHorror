<script lang="ts" setup>
import { computed } from 'vue'
import { useI18n } from 'vue-i18n'

type Area = {
  id: string
  nameFallback: string
  subtitleFallback: string
  refFallback: string
}

const props = defineProps<{
  prefix?: string
  records?: string[]
  surveyed?: string[]
}>()

const { t, te } = useI18n()

const sectionKey = (str: string) => `${props.prefix}.key['[areasSurveyed]'].${str}`

const AREAS: Area[] = [
  { id: 'northPointMine', nameFallback: 'North Point Mine', subtitleFallback: 'Written in Rock', refFallback: 'p10' },
  { id: 'hemlockHarbor', nameFallback: 'Hemlock Harbor', subtitleFallback: 'Hemlock House', refFallback: 'p14' },
  { id: 'pearlRidge', nameFallback: 'Pearl Ridge', subtitleFallback: 'The Silent Heath', refFallback: 'p19' },
  { id: 'akwanShoreline', nameFallback: 'Akwan Shoreline', subtitleFallback: 'The Lost Sister', refFallback: 'p23' },
  { id: 'eastwickBog', nameFallback: 'Eastwick Bog', subtitleFallback: 'The Thing in the Depths', refFallback: 'p27' },
  { id: 'westernWoods', nameFallback: 'Western Woods', subtitleFallback: 'The Twisted Hollow', refFallback: 'p34' },
  { id: 'southernFields', nameFallback: 'Southern Fields', subtitleFallback: 'The Longest Night', refFallback: 'p46' },
]

const lowerFirst = (s: string) => (s ? s.slice(0, 1).toLowerCase() + s.slice(1) : s)

const normalizeId = (s: string) => {
  const last = s.includes('.') ? s.split('.').pop() ?? s : s
  return lowerFirst(last.replace(/[^a-zA-Z0-9]/g, ''))
}

const surveyedSet = computed(() => {
  const src = props.surveyed?.length ? props.surveyed : (props.records ?? [])
  return new Set(src.map(normalizeId))
})

const areaText = (id: string, field: 'name' | 'scenarioName' | 'ref', fallback: string) => {
  const k = sectionKey(`${id}.${field}`)
  if (te(k)) return t(k)
  return fallback
}
</script>

<template>
  <div class="sheet">
    <div class="frame">
      <div class="header">
        <div class="header-left">{{ t(sectionKey('name')) }}</div>
        <div class="header-right">{{ t('theFeastOfHemlockVale.campaignLog.areasSurveyed.refHeader') }}</div>
      </div>

      <div class="rows">
        <div v-for="a in AREAS" :key="a.id" class="row">
          <div class="line">
            <span class="label">{{ areaText(a.id, 'name', a.nameFallback) }}</span>
            <span class="dots" aria-hidden="true" />
            <span class="ref">{{ areaText(a.id, 'ref', a.refFallback) }}</span>
          </div>

          <div class="subline">
            <span class="indent" aria-hidden="true" />
            <span class="area-box" :class="{ checked: surveyedSet.has(a.id) }" aria-hidden="true" />
            <span class="subtitle">{{ areaText(a.id, 'scenarioName', a.subtitleFallback) }}</span>
            <span class="dots faint" aria-hidden="true" />
          </div>
        </div>
      </div>
    </div>
  </div>
</template>

<style scoped>
.sheet {
  width: 100%;
  color: rgba(30, 23, 16, 0.95);
}

.frame {
  border: 1px solid rgba(80, 60, 40, 0.25);
  padding: 16px 14px;
  background:
    radial-gradient(1200px 600px at 30% 10%, rgba(255, 255, 255, 0.7), rgba(255, 255, 255, 0)),
    linear-gradient(180deg, rgba(255, 248, 235, 0.92), rgba(246, 235, 214, 0.92));
  box-shadow: inset 0 0 0 1px rgba(90, 70, 45, 0.12);
  border-radius: 6px;
}

.header {
  display: grid;
  grid-template-columns: 1fr auto;
  align-items: end;
  padding-bottom: 10px;
  margin-bottom: 10px;
  border-bottom: 1px solid rgba(90, 70, 45, 0.22);
}

.header-left {
  text-transform: uppercase;
  letter-spacing: 0.12em;
  font-weight: 600;
  font-size: 0.9em;
  font-family: serif;
  opacity: 0.95;
}

.header-right {
  text-transform: uppercase;
  letter-spacing: 0.12em;
  font-weight: 600;
  font-size: 0.85em;
  font-family: serif;
  opacity: 0.75;
}

.rows {
  display: flex;
  flex-direction: column;
  gap: 12px;
}

.row {
  display: flex;
  flex-direction: column;
  gap: 6px;
}

.line {
  display: grid;
  grid-template-columns: auto 1fr auto;
  align-items: baseline;
  column-gap: 10px;
}

.subline {
  display: grid;
  grid-template-columns: 22px auto 1fr auto;
  align-items: baseline;
  column-gap: 10px;
}

.label {
  text-transform: uppercase;
  letter-spacing: 0.08em;
  font-weight: 600;
  font-family: serif;
  font-size: 0.95em;
}

.ref {
  font-family: serif;
  letter-spacing: 0.08em;
  font-style: italic;
  opacity: 0.9;
  padding-left: 10px;
}

.subtitle {
  font-family: serif;
  font-style: italic;
  letter-spacing: 0.06em;
  opacity: 0.95;
}

.dots {
  height: 1px;
  border-bottom: 2px dotted rgba(70, 50, 30, 0.35);
  transform: translateY(-2px);
}

.dots.faint {
  border-bottom-color: rgba(70, 50, 30, 0.22);
}

.area-box {
  width: 18px;
  height: 18px;
  border: 1px solid rgba(70, 50, 30, 0.55);
  border-radius: 2px;
  background: rgba(255, 255, 255, 0.35);
  position: relative;
  transform: translateY(2px);
}

.area-box.checked {
  background: rgba(255, 255, 255, 0.55);
}

.area-box.checked::after {
  content: '';
  position: absolute;
  left: 4px;
  top: 2px;
  width: 8px;
  height: 12px;
  border-right: 2px solid rgba(45, 32, 18, 0.85);
  border-bottom: 2px solid rgba(45, 32, 18, 0.85);
  transform: rotate(40deg);
}

.area-box.ghost {
  background: transparent;
  border-color: rgba(70, 50, 30, 0.45);
  opacity: 0.9;
}

.indent {
  width: 22px;
  height: 1px;
}
</style>
