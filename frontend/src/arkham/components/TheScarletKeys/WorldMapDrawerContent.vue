<script lang="ts" setup>
import { computed } from 'vue'
import { useI18n } from 'vue-i18n'

const { t } = useI18n()

type MapLocationId = string

interface LocationData {
  travel: number | null
  unlocked: boolean
  subtitle?: string
}

interface MapData {
  current: string
  hasTicket: boolean
  available: MapLocationId[]
}

const props = defineProps<{
  selectedLocation: MapLocationId
  locationData: Record<MapLocationId, LocationData>
  mapData: MapData
  embark: boolean
  isFinale: boolean
}>()

const emit = defineEmits<{
  close: []
  travelTo: []
  travelVia: []
  travelWithTicket: []
}>()

const dossierKey = computed<string | null>(() => {
  switch (props.selectedLocation) {
    case 'Marrakesh':
    case 'BuenosAires':
    case 'Constantinople':
    case 'Havana':
    case 'Shanghai':
    case 'Anchorage':
    case 'Tokyo':
    case 'Alexandria':
    case 'Sydney':
    case 'YborCity':
    case 'Kathmandu':
    case 'Nairobi':
      return props.selectedLocation
    case 'SanFrancisco':
    case 'Moscow':
      return 'Sanctums'
    case 'RioDeJaneiro':
    case 'Perth':
      return 'DrIrawan'
    default:
      return null
  }
})
</script>

<template>
  <header>
    <button class="close-btn" @click.stop="emit('close')">×</button>
    <h2>{{ t(`theScarletKeys.locations.${selectedLocation}.name`) }}</h2>
    <h3>{{ locationData[selectedLocation].subtitle }}</h3>
  </header>

  <div class="drawer-content">
    <template v-if="selectedLocation === mapData.current">
      <p v-if="!isFinale">{{ t('scarletKeys.youAreCurrentlyHere') }}</p>
      <button v-else class="action" @click="emit('travelTo')">{{ t('scarletKeys.travelHere') }}</button>
    </template>
    <template v-else-if="embark">
      <p><strong>{{ t('scarletKeys.travelTime') }}:</strong> {{ locationData[selectedLocation].travel }}</p>
      <div v-if="selectedLocation === 'Venice'" class="side-story-info">
        <p>{{ t('scarletKeys.sideStoryLocation') }}</p>
        <p>{{ t('scarletKeys.sideStoryDescription') }}</p>
      </div>
      <template v-if="locationData[selectedLocation].unlocked">
        <button class="action" @click="emit('travelTo')">{{ t('scarletKeys.travelHere') }}</button>
        <button
          v-if="mapData.hasTicket && (locationData[selectedLocation].travel ?? 0) > 1"
          class="action"
          @click="emit('travelWithTicket')"
        >{{ t('scarletKeys.travelWithExpeditedTicket') }}</button>
        <button class="action secondary" @click="emit('travelVia')">{{ t('scarletKeys.travelWithoutStopping') }}</button>
      </template>
      <template v-else>
        <p class="action locked">{{ t('scarletKeys.locationLocked') }}</p>
        <button class="action secondary" @click="emit('travelVia')">{{ t('scarletKeys.travelWithoutStopping') }}</button>
      </template>
    </template>

    <div v-if="dossierKey" class='dossier'>
      <header><h3>{{ t('theScarletKeys.worldMap.dossierHeading') }}</h3></header>
      <section v-html="t(`theScarletKeys.worldMap.dossiers.${dossierKey}`)"></section>
    </div>
    <div v-else-if="selectedLocation === 'Kabul'" class='dossier'>
    </div>
  </div>
</template>

<style scoped>
header {
  background: rgba(255,255,255,0.1);
  padding: 0.3em 0.5em;
  border-bottom: 1px solid rgba(255,255,255,0.15);
  position: relative;
  display: flex;
  flex-direction: column;
  justify-content: center;
  min-height: 1.5em;
}

h2 {
  color: white;
  margin: 0 0 0.1em;
}

h3 {
  font-size: 0.6em;
  font-weight: normal;
  color: #ccc;
  margin: 0;
}

.close-btn {
  position: absolute;
  top: 0.25em;
  right: 0.5em;
  background: none;
  border: none;
  color: #ccc;
  cursor: pointer;
  font-size: 1.5em;
  line-height: 1;
}

.drawer-content {
  flex: 1;
  overflow-y: auto;
  padding: 0 1em 1em;
  scrollbar-width: thin;
  margin-top: 0.5em;
}

.action {
  display: block;
  width: 100%;
  margin-top: 0.375em;
  padding: 0.375em;
  border-radius: 4px;
  font-weight: bold;
  background: #2e3a4f;
  color: #eee;
  font-size: 1em;
  cursor: pointer;
  border: none;
  &:hover { background: #3b4a6b; }
}

.action.secondary {
  background: #e2dfcc;
  color: #222;
  &:hover { background: #ccc9b3; }
}

p.locked {
  color: #888;
  background-color: darkred;
  font-style: italic;
}

.side-story-info {
  text-align: left;
  margin-block: 1em;
  display: flex;
  flex-direction: column;
  gap: 0.5em;
}

.dossier {
  margin-top: 1.5em;
  padding: 1em;
  background: #F9F1DE;
  color: #222;
  font-family: 'Typewriter', serif;
  border-radius: 8px;
  font-size: 0.8em;
  line-height: 1.4;
  text-align: left;

  section {
    display: flex;
    flex-direction: column;
    gap: 1em;
  }

  header {
    margin-bottom: 0.5em;
    text-align: center;
    background: none;
    border: none;
    padding: 0;
    min-height: unset;
  }

  h3 {
    align-self: center;
    text-align: center;
    border-bottom: 1px solid #000;
    display: inline-block;
    position: relative;
    color: black;
    font-weight: bold;
    font-size: 1.4em;
    font-family: 'Teutonic', serif;
    &::after {
      content: '';
      position: absolute;
      inset: 0;
      bottom: 3px;
      border-bottom: 1px solid #000;
    }
  }
}

.dossier :deep(.censor) {
  background-color: black;
  color: black;
  padding: 0 4px;
  border-radius: 2px;
}

.dossier :deep(.indent) {
  text-indent: 1.5em;
}
</style>
