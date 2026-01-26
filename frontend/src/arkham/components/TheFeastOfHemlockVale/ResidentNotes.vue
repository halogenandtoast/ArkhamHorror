<script lang="ts" setup>
import { computed } from 'vue'
import { useI18n } from 'vue-i18n'
import { imgsrc } from '@/arkham/helpers'

const props = defineProps<{
  sectionId: string
  prefix: string
  records: string[]
  relationshipLevel: number
}>()

const { t } = useI18n()

const sectionKey = (str: string) => t(`${props.prefix}.key['[${props.sectionId}]'].${str}`)

const residentName = computed(() => sectionKey('name'))

const clampedLevel = computed(() => Math.max(0, Math.min(6, Math.floor(props.relationshipLevel || 0))))

const card = computed(() => {
  switch(props.sectionId) {
    case 'motherRachelNotes': return imgsrc("cards/10693.avif")
    case 'leahAtwoodNotes': return imgsrc("cards/10694.avif")
    case 'simeonAtwoodNotes': return imgsrc("cards/10695.avif")
    case 'williamHemlockNotes': return imgsrc("cards/10696.avif")
    case 'riverHawthorneNotes': return imgsrc("cards/10697.avif")
    case 'gideonMizrahNotes': return imgsrc("cards/10698.avif")
    case 'judithParkNotes': return imgsrc("cards/10699.avif")
    case 'theoPetersNotes': return imgsrc("cards/10700.avif")
    default: return null
  }
})
</script>

<template>
  <div class='resident'>
    <div class="resident-card">
      <img v-if="card" :src="card" class="card no-overlay" />
    </div>
    <div class="resident-notes">
      <h3 class="name">{{ residentName }}</h3>

      <div class="notes">
        <h4 class="notes-title">{{ t('theFeastOfHemlockVale.campaignLog.residents.notes') }}</h4>
        <ul class="notes-list">
          <li v-for="rk in records" :key="rk" class="note-item">{{ t(rk) }}</li>
        </ul>
      </div>
      <div class="relationship">
        <div class="relationship-title">{{ t('theFeastOfHemlockVale.campaignLog.residents.relationshipLevel') }}</div>
        <div class="boxes" role="img">
          <span v-for="n in 6" :key="n" class="relationship-box" :class="{ filled: n <= clampedLevel }" />
        </div>
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

.name {
  margin: 0;
  font-family: Wolgast, sans-serif;
  text-box-trim: trim-both;
  text-box-edge: text alphabetic;
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

.resident-card {
  width: 25%;
  max-width: 200px;
  img {
    border-radius: 0.25em;
  }
}
</style>
