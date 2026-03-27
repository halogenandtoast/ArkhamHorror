<script setup lang="ts">
import { imgsrc } from '@/arkham/helpers';
import { watch, computed, ref, inject } from 'vue'
import { Game } from '@/arkham/types/Game';
import { useI18n } from 'vue-i18n';
import WorldMapDrawerContent from '@/arkham/components/TheScarletKeys/WorldMapDrawerContent.vue';

const { t } = useI18n();
type MapLocationId =
  | 'Alexandria'
  | 'Anchorage'
  | 'Arkham'
  | 'Bermuda'
  | 'BermudaTriangle'
  | 'Bombay'
  | 'BuenosAires'
  | 'Cairo'
  | 'Constantinople'
  | 'Havana'
  | 'HongKong'
  | 'Kabul'
  | 'Kathmandu'
  | 'KualaLumpur'
  | 'Lagos'
  | 'London'
  | 'Manokwari'
  | 'Marrakesh'
  | 'MonteCarlo'
  | 'Moscow'
  | 'Nairobi'
  | 'NewOrleans'
  | 'Perth'
  | 'Quito'
  | 'Reykjavik'
  | 'RioDeJaneiro'
  | 'Rome'
  | 'SanFrancisco'
  | 'SanJuan'
  | 'Shanghai'
  | 'Stockholm'
  | 'Sydney'
  | 'Tokyo'
  | 'Tunguska'
  | 'Venice'
  | 'YborCity'

interface LocationData {
  travel: number | null
  unlocked: boolean
}
interface MapData {
  current: string
  hasTicket: boolean
  available: MapLocationId[]
  locations: [MapLocationId, LocationData][]
} 

const send = inject<(msg: string) => void>('send', () => {})

const props = defineProps<{
  game: Game
  playerId: string
  mapData: MapData
  embark: boolean
}>()

const isFinale = computed(() => {
  return props.mapData.available.length === 1
})
const greenLocations = ['Arkham', 'Cairo', 'NewOrleans', 'Venice', 'MonteCarlo'] as MapLocationId[]
const data = {
  'Alexandria': { x: 1691, y: 613, subtitle: t('theScarletKeys.locations.Alexandria.subtitle') },
  'Anchorage': { x: 226, y: 289, subtitle: t('theScarletKeys.locations.Anchorage.subtitle') },
  'Arkham': { x: 771, y: 494 },
  'Bermuda': { x: 815, y: 603, subtitle: t('theScarletKeys.locations.Bermuda.subtitle') },
  'BermudaTriangle': { x: 728, y: 680 },
  'Bombay': { x: 2117, y: 760, subtitle: t('theScarletKeys.locations.Bombay.subtitle') },
  'BuenosAires': { x: 866, y: 1382, subtitle: t('theScarletKeys.locations.BuenosAires.subtitle') },
  'Cairo': { x: 1706, y: 646 },
  'Constantinople': { x: 1675, y: 503, subtitle: t('theScarletKeys.locations.Constantinople.subtitle') },
  'Havana': { x: 617, y: 712, subtitle: t('theScarletKeys.locations.Havana.subtitle') },
  'HongKong': { x: 2515, y: 719 },
  'Kabul': { x: 2064, y: 568 , subtitle: t('theScarletKeys.locations.Kabul.subtitle') },
  'Kathmandu': { x: 2238, y: 673, subtitle: t('theScarletKeys.locations.Kathmandu.subtitle') },
  'KualaLumpur': { x: 2412, y: 944 },
  'Lagos': { x: 1437, y: 904, subtitle: t('theScarletKeys.locations.Lagos.subtitle') },
  'London': { x: 1419, y: 386 },
  'Manokwari': { x: 2735, y: 987 },
  'Marrakesh': { x: 1334, y: 612, subtitle: t('theScarletKeys.locations.Marrakesh.subtitle') },
  'MonteCarlo': { x: 1483, y: 472 },
  'Moscow': { x: 1746, y: 332, subtitle: t('theScarletKeys.locations.Moscow.subtitle') },
  'Nairobi': { x: 1776, y: 990, subtitle: t('theScarletKeys.locations.Nairobi.subtitle') },
  'NewOrleans': { x: 548, y: 635 },
  'Perth': { x: 2506, y: 1345, subtitle: t('theScarletKeys.locations.Perth.subtitle') },
  'Quito': { x: 624, y: 980 },
  'Reykjavik': { x: 1258, y: 249 },
  'RioDeJaneiro': { x: 991, y: 1240, subtitle: t('theScarletKeys.locations.RioDeJaneiro.subtitle') },
  'Rome': { x: 1530, y: 498, subtitle: t('theScarletKeys.locations.Rome.subtitle') },
  'SanFrancisco': { x: 273, y: 545, subtitle: t('theScarletKeys.locations.SanFrancisco.subtitle') },
  'SanJuan': { x: 758, y: 766 },
  'Shanghai': { x: 2565, y: 619, subtitle: t('theScarletKeys.locations.Shanghai.subtitle') },
  'Stockholm': { x: 1568, y: 299, subtitle: t('theScarletKeys.locations.Stockholm.subtitle') },
  'Sydney': { x: 2837, y: 1362, subtitle: t('theScarletKeys.locations.Sydney.subtitle') },
  'Tokyo': { x: 2714, y: 573, subtitle: t('theScarletKeys.locations.Tokyo.subtitle') },
  'Tunguska': { x: 2299, y: 295 },
  'Venice': { x: 1523, y: 453 },
  'YborCity': { x: 616, y: 657, subtitle: t('theScarletKeys.locations.YborCity.subtitle') },
}

//convert props.mapData.locations to a Record<MapLocationId, LocationData>
const locationData = computed<Record<MapLocationId, LocationData>>(() => {
  const record: Record<MapLocationId, LocationData> = {} as Record<MapLocationId, LocationData>
  for (const [key, value] of props.mapData.locations) {
    const unlocked = props.mapData.available.includes(key)
    if (greenLocations.includes(key)) {
      record[key] = {...value, ...data[key], travel: (value.travel ?? 0) + 1 , unlocked }
    } else {
      record[key] = {...value, ...data[key], unlocked}
    }
  }
  return record
})


const svgEl = ref<SVGSVGElement | null>(null)
const wrapperEl = ref<HTMLDivElement | null>(null)
const fullScreen = ref(false)

// ── Pan / drag ────────────────────────────────────────────
const panOffset = ref({ x: 0, y: 0 })
const isDragging = ref(false)
const dragStart = ref({ x: 0, y: 0 })
const dragStartPan = ref({ x: 0, y: 0 })

function startDrag(e: MouseEvent) {
  if (e.button !== 0 || props.embark) return
  isDragging.value = true
  dragStart.value = { x: e.clientX, y: e.clientY }
  dragStartPan.value = { ...panOffset.value }
  e.preventDefault()
}

function onDrag(e: MouseEvent) {
  if (!isDragging.value || !svgEl.value) return
  const { w, h } = baseViewBox.value
  const rect = svgEl.value.getBoundingClientRect()
  const scaleX = w / rect.width
  const scaleY = h / rect.height
  panOffset.value = {
    x: dragStartPan.value.x - (e.clientX - dragStart.value.x) * scaleX,
    y: dragStartPan.value.y - (e.clientY - dragStart.value.y) * scaleY,
  }
}

function stopDrag() { isDragging.value = false }
// select a location
const selectedLocation = ref<MapLocationId | null>(null)
function select(location: MapLocationId) {
  selectedLocation.value = location
}

function travelToSelected() {
  if (selectedLocation.value) {
    const goTo = selectedLocation.value  
    selectedLocation.value = null
    send(JSON.stringify({
      tag: 'CampaignSpecificAnswer',
      contents: ["travel", goTo]
    }))
  }
}

function travelViaSelected() {
  if (selectedLocation.value) {
    const goTo = selectedLocation.value  
    selectedLocation.value = null
    send(JSON.stringify({
      tag: 'CampaignSpecificAnswer',
      contents: ["travelVia", goTo]
    }))
  }
}

function travelWithTicket() {
  if (selectedLocation.value) {
    const goTo = selectedLocation.value  
    selectedLocation.value = null
    send(JSON.stringify({
      tag: 'CampaignSpecificAnswer',
      contents: ["travelWithTicket", goTo]
    }))
  }
}

// close the popup
function closePopup() {
  selectedLocation.value = null
}

const coordinates = computed(() => {
  const loc = data[props.mapData.current as MapLocationId]
  if (loc) return { x: loc.x, y: loc.y }
  return null
})

watch([coordinates, fullScreen], () => { panOffset.value = { x: 0, y: 0 } })

const ZOOM_W = 1200
const ZOOM_H = 781

// PIN_OFFSET: the pin circle head is ~24 SVG units above the tip (scaled 1.5 * 16 circle center at -24)
const PIN_OFFSET_Y = 24

const baseViewBox = computed(() => {
  if (fullScreen.value || !coordinates.value || props.embark) return { x0: 0, y0: 0, w: 3000, h: 1952 }
  const { x, y } = coordinates.value
  const x0 = Math.max(0, Math.min(3000 - ZOOM_W, x - ZOOM_W / 2))
  const y0 = Math.max(0, Math.min(1952 - ZOOM_H, (y - PIN_OFFSET_Y) - ZOOM_H / 2))
  return { x0, y0, w: ZOOM_W, h: ZOOM_H }
})

const viewBoxValues = computed(() => {
  const { x0, y0, w, h } = baseViewBox.value
  return {
    x0: Math.max(0, Math.min(3000 - w, x0 + panOffset.value.x)),
    y0: Math.max(0, Math.min(1952 - h, y0 + panOffset.value.y)),
    w, h,
  }
})

const viewBox = computed(() => {
  const { x0, y0, w, h } = viewBoxValues.value
  return `${x0} ${y0} ${w} ${h}`
})

const worldMap = imgsrc('world-map.jpg')

const toggleFullScreen = async () => {
  const el = wrapperEl.value
  if (!el) return

  if (!document.fullscreenElement) {
    await el.requestFullscreen?.()
    fullScreen.value = true
  } else {
    await document.exitFullscreen?.()
    fullScreen.value = false
  }
}

// Handle user pressing ESC or system exit
document.addEventListener('fullscreenchange', () => {
  fullScreen.value = !!document.fullscreenElement
})

</script>

<template>
  <div class="map-wrapper" :class="{ 'embark-view': embark }" ref="wrapperEl">
  <div class="map-svg-container">
  <svg
    :viewBox="viewBox"
    fill="none"
    xmlns="http://www.w3.org/2000/svg"
    ref="svgEl"
    :class="{ dragging: isDragging }"
    @mousedown="startDrag"
    @mousemove="onDrag"
    @mouseup="stopDrag"
    @mouseleave="stopDrag"
  >
    <defs>
      <g id="marker-red">
        <circle r="13" fill="none" stroke="#d82e21" stroke-width="3"/>
        <circle r="10" fill="#ffffff" stroke="#ffffff" stroke-width="3"/>
        <circle r="7" fill="#d82e21" stroke="#d82e21" stroke-width="1.6"/>
      </g>

      <g id="marker-blue">
        <circle r="13" fill="#EAEAEC" stroke="#31369B" stroke-width="3"/>
        <path
          d="M 0,-11
             L 2.95,-3.4 11, -3.4
             L 4.6, 1.3 7.1, 9.5
             L 0, 4.9 -7.1, 9.5
             L -4.6, 1.3 -11, -3.4
             L -2.95,-3.4 Z"
          fill="#31369B"
          stroke="none"
        />
      </g>

      <g id="marker-green">
        <circle r="9.5" fill="none" stroke="#2f6a3f" stroke-width="2.5"/>
        <circle r="7" fill="#D6DFCC" stroke="#D6DFCC" stroke-width="2.5"/>
        <circle r="4" fill="#ffffff" stroke="#2f6a3f" stroke-width="2.5"/>
      </g>
    </defs>

    <image :href="worldMap" x="0" y="0" width="3000" height="1952"/>

    <g id="bermuda-triangle--bermuda" class="route" v-if="locationData.BermudaTriangle.unlocked === true">
      <path d="M 728 680 L 815 603" class="route-highlight"/>
      <path d="M 728 680 L 815 603" class="route-base"/>
      <path d="M 728 680 L 815 603" class="route-hit"/>
    </g>

    <g id="bermuda-triangle--ybor-city" class="route" v-if="locationData.BermudaTriangle.unlocked === true">
      <path d="M 728 680 L 616 657" class="route-highlight"/>
      <path d="M 728 680 L 616 657" class="route-base"/>
      <path d="M 728 680 L 616 657" class="route-hit"/>
    </g>

    <g id="bermuda-triangle--san-juan" class="route" v-if="locationData.BermudaTriangle.unlocked === true"> 
      <path d="M 728 680 L 758 766" class="route-highlight"/>
      <path d="M 728 680 L 758 766" class="route-base"/>
      <path d="M 728 680 L 758 766" class="route-hit"/>
    </g>

    <g id="anchorage" class="marker" transform="translate(226,289)" @click="select('Anchorage')">
      <use :href="`#marker-${locationData.Anchorage.unlocked ? 'blue' : 'red'}`"/>
      <rect width="153" x="-160" y="-11" height="23" fill="#00000000" />
    </g>

    <g id="san_francisco" class="marker" transform="translate(273,545)" @click="select('SanFrancisco')">
      <use :href="`#marker-${locationData.SanFrancisco.unlocked ? 'blue' : 'red'}`"/>
      <rect width="213" x="-218" y="-11" height="23" fill="#00000000" />
    </g>

    <g id="arkham" class="marker" transform="translate(771,494)" @click="select('Arkham')">
      <use href="#marker-green"/>
      <rect width="89" y="-11" height="20" fill="#00000000" />
    </g>

    <g id="ybor_city" class="marker" transform="translate(616,657)" @click="select('YborCity')">
      <use :href="`#marker-${locationData.YborCity.unlocked ? 'blue' : 'red'}`"/>
      <rect width="159" x="-155" y="-11" height="23" fill="#00000000" />
    </g>

    <g id="havana" class="marker" transform="translate(617,712)" @click="select('Havana')">
      <use :href="`#marker-${locationData.Havana.unlocked ? 'blue' : 'red'}`"/>
      <rect width="129" x="-125" y="-11" height="23" fill="#00000000" />
    </g>

    <g id="bermuda" class="marker" transform="translate(815,603)" @click="select('Bermuda')">
      <use :href="`#marker-${locationData.Bermuda.unlocked ? 'blue' : 'red'}`"/>
      <rect width="134" y="-11" height="23" fill="#00000000" />
    </g>

    <g id="new_orleans" class="marker" transform="translate(548,635)" @click="select('NewOrleans')">
      <use href="#marker-green"/>
      <rect width="134" x="-134" y="-11" height="20" fill="#00000000" />
    </g>

    <g id="san_juan" class="marker" transform="translate(758,766)" @click="select('SanJuan')">
      <g v-if="locationData.SanJuan.unlocked" class="location-code">
        <rect
          x="0"
          y="13"
          width="56"
          height="24"
          rx="3"
          fill="#323035"
        />
        <text
          x="29"
          y="27"
          text-anchor="middle"
          dominant-baseline="middle"
          fill="#CCCCCC"
          class="label"
          font-family="MapTypewriter, Typewriter, sans-serif"
          font-size="40"
          font-weight="bold"
        >
          14-C
        </text>
      </g>
      <use :href="`#marker-${locationData.SanJuan.unlocked ? 'blue' : 'red'}`"/>
      <rect width="145" y="-11" height="23" fill="#00000000" />
    </g>

    <g id="quito" class="marker" transform="translate(626,979)" @click="select('Quito')">
      <g v-if="locationData.Quito.unlocked" class="location-code">
        <rect
          x="0"
          y="13"
          width="56"
          height="24"
          rx="3"
          fill="#323035"
        />
        <text
          x="29"
          y="27"
          text-anchor="middle"
          dominant-baseline="middle"
          fill="#CCCCCC"
          class="label"
          font-family="MapTypewriter, Typewriter, sans-serif"
          font-size="40"
          font-weight="bold"
        >
          14-C
        </text>
      </g>
      <use :href="`#marker-${locationData.Quito.unlocked ? 'blue' : 'red'}`"/>
      <rect width="100" y="-11" height="23" fill="#00000000" />
    </g>

    <g id="rio_de_janeiro" class="marker" transform="translate(991,1240)" @click="select('RioDeJaneiro')">
      <use :href="`#marker-${locationData.RioDeJaneiro.unlocked ? 'blue' : 'red'}`"/>
      <rect width="220" x="-225" y="-11" height="23" fill="#00000000" />
    </g>

    <g id="buenos_aires" class="marker" transform="translate(866,1382)" @click="select('BuenosAires')">
      <use :href="`#marker-${locationData.BuenosAires.unlocked ? 'blue' : 'red'}`"/>
      <rect width="200" x="-200" y="-11" height="23" fill="#00000000" />
    </g>

    <g id="lagos" class="marker" transform="translate(1437,904)" @click="select('Lagos')">
      <use :href="`#marker-${locationData.Lagos.unlocked ? 'blue' : 'red'}`"/>
      <rect width="100" y="-11" height="23" fill="#00000000" />
    </g>

    <g id="nairobi" class="marker" transform="translate(1776,990)" @click="select('Nairobi')">
      <use :href="`#marker-${locationData.Nairobi.unlocked ? 'blue' : 'red'}`"/>
      <rect width="132" x="-132" y="-11" height="23" fill="#00000000" />
    </g>

    <g id="marrakesh" class="marker" transform="translate(1334,612)" @click="select('Marrakesh')">
      <use :href="`#marker-${locationData.Marrakesh.unlocked ? 'blue' : 'red'}`"/>
      <rect width="168" y="-11" height="23" fill="#00000000" />
    </g>

    <g id="alexandria" class="marker" transform="translate(1691,613)" @click="select('Alexandria')">
      <use :href="`#marker-${locationData.Alexandria.unlocked ? 'blue' : 'red'}`"/>
      <rect width="179" x="-179" y="-11" height="23" fill="#00000000" />
    </g>

    <g id="cairo" class="marker" transform="translate(1706,646)" @click="select('Cairo')">
      <use href="#marker-green"/>
      <rect width="75" y="-11" height="21" fill="#00000000" />
    </g>

    <g v-if="locationData.BermudaTriangle.unlocked === true" id="bermuda_triangle" class="marker" transform="translate(728,680)" @click="select('BermudaTriangle')">
      <use href="#marker-blue"/>
    </g>

    <g id="london" class="marker" transform="translate(1420.5,384)" @click="select('London')">
      <g v-if="locationData.London.unlocked" class="location-code">
        <rect
          x="0"
          y="13"
          width="56"
          height="24"
          rx="3"
          fill="#323035"
        />
        <text
          x="29"
          y="27"
          text-anchor="middle"
          dominant-baseline="middle"
          fill="#CCCCCC"
          class="label"
          font-family="MapTypewriter, Typewriter, sans-serif"
          font-size="40"
          font-weight="bold"
        >
          27-H
        </text>
      </g>
      <use :href="`#marker-${locationData.London.unlocked ? 'blue' : 'red'}`"/>
      <rect width="120" y="-11" height="23" fill="#00000000" />
    </g>

    <g id="reykjavik" class="marker" transform="translate(1258,249)" @click="select('Reykjavik')">
      <g v-if="locationData.Reykjavik.unlocked" class="location-code">
        <rect
          x="0"
          y="13"
          width="56"
          height="24"
          rx="3"
          fill="#323035"
        />
        <text
          x="29"
          y="27"
          text-anchor="middle"
          dominant-baseline="middle"
          fill="#CCCCCC"
          class="label"
          font-family="MapTypewriter, Typewriter, sans-serif"
          font-size="40"
          font-weight="bold"
        >
          14-C
        </text>
      </g>
      <use :href="`#marker-${locationData.Reykjavik.unlocked ? 'blue' : 'red'}`"/>
      <rect width="161" y="-11" height="23" fill="#00000000" />
    </g>

    <g id="stockholm" class="marker" transform="translate(1568,299)" @click="select('Stockholm')">
      <use :href="`#marker-${locationData.Stockholm.unlocked ? 'blue' : 'red'}`"/>
      <rect width="155" x="-155" y="-11" height="23" fill="#00000000" />
    </g>

    <g id="rome" class="marker" transform="translate(1530,498)" @click="select('Rome')">
      <use :href="`#marker-${locationData.Rome.unlocked ? 'blue' : 'red'}`"/>
      <rect width="85" y="-11" height="23" fill="#00000000" />
    </g>

    <g id="moscow" class="marker" transform="translate(1746,332)" @click="select('Moscow')">
      <use :href="`#marker-${locationData.Moscow.unlocked ? 'blue' : 'red'}`"/>
      <rect width="120" y="-11" height="23" fill="#00000000" />
    </g>

    <g id="constantinople" class="marker" transform="translate(1675,503)" @click="select('Constantinople')">
      <use :href="`#marker-${locationData.Constantinople.unlocked ? 'blue' : 'red'}`"/>
      <rect width="230" y="-11" height="23" fill="#00000000" />
    </g>

    <g id="bombay" class="marker" transform="translate(2117,760)" @click="select('Bombay')">
      <use :href="`#marker-${locationData.Bombay.unlocked ? 'blue' : 'red'}`"/>
      <rect width="120" y="-11" height="23" fill="#00000000" />
    </g>

    <g id="kathmandu" class="marker" transform="translate(2238,673)" @click="select('Kathmandu')">
      <use :href="`#marker-${locationData.Kathmandu.unlocked ? 'blue' : 'red'}`"/>
      <rect width="170" y="-11" height="23" fill="#00000000" />
    </g>

    <g id="shanghai" class="marker" transform="translate(2565,619)" @click="select('Shanghai')">
      <use :href="`#marker-${locationData.Shanghai.unlocked ? 'blue' : 'red'}`"/>
      <rect width="150" x="-150" y="-11" height="23" fill="#00000000" />
    </g>

    <g id="tokyo" class="marker" transform="translate(2714,573)" @click="select('Tokyo')">
      <use :href="`#marker-${locationData.Tokyo.unlocked ? 'blue' : 'red'}`"/>
      <rect width="100" y="-11" height="23" fill="#00000000" />
    </g>

    <g id="perth" class="marker" transform="translate(2506,1345)" @click="select('Perth')">
      <use :href="`#marker-${locationData.Perth.unlocked ? 'blue' : 'red'}`"/>
      <rect width="100" y="-11" height="23" fill="#00000000" />
    </g>

    <g id="sydney" class="marker" transform="translate(2837,1362)" @click="select('Sydney')">
      <use :href="`#marker-${locationData.Sydney.unlocked ? 'blue' : 'red'}`"/>
      <rect width="115" x="-115" y="-11" height="23" fill="#00000000" />
    </g>

    <g id="venice" class="marker" transform="translate(1523,453)" @click="select('Venice')">
      <use href="#marker-green"/>
      <rect width="85" y="-11" height="21" fill="#00000000" />
    </g>

    <g id="monte_carlo" class="marker" transform="translate(1483,472)" @click="select('MonteCarlo')">
      <use href="#marker-green"/>
      <rect width="135" x="-135" y="-11" height="21" fill="#00000000" />
    </g>

    <g id="tunguska" class="marker" transform="translate(2299,295)" @click="select('Tunguska')">
      <g v-if="locationData.Tunguska.unlocked" class="location-code">
        <rect
          x="0"
          y="13"
          width="56"
          height="24"
          rx="3"
          fill="#323035"
        />
        <text
          x="29"
          y="27"
          text-anchor="middle"
          dominant-baseline="middle"
          fill="#CCCCCC"
          class="label"
          font-family="MapTypewriter, Typewriter, sans-serif"
          font-size="40"
          font-weight="bold"
        >
          59-Z
        </text>
      </g>
      <use :href="`#marker-${locationData.Tunguska.unlocked ? 'blue' : 'red'}`"/>
      <rect width="153" y="-11" height="23" fill="#00000000" />
    </g>

    <g id="kabul" class="marker" transform="translate(2064,568)" @click="select('Kabul')">
      <g v-if="locationData.Kabul.unlocked" class="location-code">
        <rect
          x="0"
          y="13"
          width="56"
          height="24"
          rx="3"
          fill="#323035"
        />
        <text
          x="29"
          y="27"
          text-anchor="middle"
          dominant-baseline="middle"
          fill="#CCCCCC"
          class="label"
          font-family="MapTypewriter, Typewriter, sans-serif"
          font-size="40"
          font-weight="bold"
        >
          14-C
        </text>
      </g>
      <use :href="`#marker-${locationData.Kabul.unlocked ? 'blue' : 'red'}`"/>
      <rect width="103" y="-11" height="23" fill="#00000000" />
    </g>

    <g id="hong_kong" class="marker" transform="translate(2515,719)" @click="select('HongKong')">
      <g v-if="locationData.HongKong.unlocked" class="location-code">
        <rect
          x="-56"
          y="13"
          width="56"
          height="24"
          rx="3"
          fill="#323035"
        />
        <text
          x="-27"
          y="27"
          text-anchor="middle"
          dominant-baseline="middle"
          fill="#CCCCCC"
          class="label"
          font-family="MapTypewriter, Typewriter, sans-serif"
          font-size="40"
          font-weight="bold"
        >
          50-S
        </text>
      </g>
      <use :href="`#marker-${locationData.HongKong.unlocked ? 'blue' : 'red'}`"/>
      <rect width="160" x="-160" y="-11" height="23" fill="#00000000" />
    </g>

    <g id="kuala_lumpur" class="marker" transform="translate(2413,943)" @click="select('KualaLumpur')">
      <g v-if="locationData.KualaLumpur.unlocked" class="location-code">
        <rect
          x="-56"
          y="13"
          width="56"
          height="24"
          rx="3"
          fill="#323035"
        />
        <text
          x="-27"
          y="27"
          text-anchor="middle"
          dominant-baseline="middle"
          fill="#CCCCCC"
          class="label"
          font-family="MapTypewriter, Typewriter, sans-serif"
          font-size="40"
          font-weight="bold"
        >
          46-Q
        </text>
      </g>
      <use :href="`#marker-${locationData.KualaLumpur.unlocked ? 'blue' : 'red'}`"/>
      <rect width="210" x="-210" y="-11" height="23" fill="#00000000" />
    </g>

    <g id="manokwari" class="marker" transform="translate(2735,987)" @click="select('Manokwari')">
      <use :href="`#marker-${locationData.Manokwari.unlocked ? 'blue' : 'red'}`"/>
      <rect width="160" x="-160" y="-11" height="23" fill="#00000000" />
    </g>

    <g v-if="coordinates" :transform="`translate(${coordinates.x}, ${coordinates.y}) scale(1.5)`">
      <!-- Pin shape -->
      <path
        d="M 0 0 L -8 -16 A 16 16 0 1 1 8 -16 Z"
        fill="rgba(0, 0, 0, 0.8)"
        stroke="rgba(0, 0, 0, 0.8)"
        stroke-width="1"
      />
      <!-- Letter label -->
      <text
        x="0"
        y="-28"
        text-anchor="middle"
        dominant-baseline="middle"
        fill="white"
        font-size="28"
        font-family="Arkham"
        font-weight="bold"
        class="pin-label"
      >
        {{ String.fromCodePoint(0x0048) }}
      </text>
    </g>

    <!-- Embark view: original overlay drawer inside SVG -->
    <Transition v-if="embark" name="drawer" mode="out-in">
      <foreignObject
        v-if="selectedLocation"
        :x="(3000 * 0.5) - ((3000 * 0.3) / 2)"
        :y="1952 * 0.5"
        :width="3000 * 0.3"
        :height="1952 * 0.5"
      >
        <div class="drawer-container">
          <div class="drawer">
            <WorldMapDrawerContent
              v-if="selectedLocation"
              :selectedLocation="selectedLocation"
              :locationData="locationData"
              :mapData="mapData"
              :embark="embark"
              :isFinale="isFinale"
              @close="closePopup"
              @travelTo="travelToSelected"
              @travelVia="travelViaSelected"
              @travelWithTicket="travelWithTicket"
            />
          </div>
        </div>
      </foreignObject>
    </Transition>
  </svg>

  <!-- HTML overlay button — unaffected by SVG viewBox/coordinate system -->
  <button class="expand-btn" @click.stop="toggleFullScreen" title="Toggle fullscreen">
    <svg v-if="!fullScreen" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" fill="currentColor">
      <path fill-rule="evenodd" d="M15 3.75a.75.75 0 0 1 .75-.75h4.5a.75.75 0 0 1 .75.75v4.5a.75.75 0 0 1-1.5 0V5.56l-3.97 3.97a.75.75 0 1 1-1.06-1.06l3.97-3.97h-2.69a.75.75 0 0 1-.75-.75Zm-12 0A.75.75 0 0 1 3.75 3h4.5a.75.75 0 0 1 0 1.5H5.56l3.97 3.97a.75.75 0 0 1-1.06 1.06L4.5 5.56v2.69a.75.75 0 0 1-1.5 0v-4.5Zm11.47 11.78a.75.75 0 1 1 1.06-1.06l3.97 3.97v-2.69a.75.75 0 0 1 1.5 0v4.5a.75.75 0 0 1-.75.75h-4.5a.75.75 0 0 1 0-1.5h2.69l-3.97-3.97Zm-4.94-1.06a.75.75 0 0 1 0 1.06L5.56 19.5h2.69a.75.75 0 0 1 0 1.5h-4.5a.75.75 0 0 1-.75-.75v-4.5a.75.75 0 0 1 1.5 0v2.69l3.97-3.97a.75.75 0 0 1 1.06 0Z" clip-rule="evenodd"/>
    </svg>
    <svg v-else xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" fill="currentColor">
      <path fill-rule="evenodd" d="M3.22 3.22a.75.75 0 0 1 1.06 0l3.97 3.97V4.5a.75.75 0 0 1 1.5 0V9a.75.75 0 0 1-.75.75H4.5a.75.75 0 0 1 0-1.5h2.69L3.22 4.28a.75.75 0 0 1 0-1.06Zm17.56 0a.75.75 0 0 1 0 1.06l-3.97 3.97h2.69a.75.75 0 0 1 0 1.5H15a.75.75 0 0 1-.75-.75V4.5a.75.75 0 0 1 1.5 0v2.69l3.97-3.97a.75.75 0 0 1 1.06 0ZM3.75 15a.75.75 0 0 1 .75-.75H9a.75.75 0 0 1 .75.75v4.5a.75.75 0 0 1-1.5 0v-2.69l-3.97 3.97a.75.75 0 0 1-1.06-1.06l3.97-3.97H4.5a.75.75 0 0 1-.75-.75Zm10.5 0a.75.75 0 0 1 .75-.75h4.5a.75.75 0 0 1 0 1.5h-2.69l3.97 3.97a.75.75 0 1 1-1.06 1.06l-3.97-3.97v2.69a.75.75 0 0 1-1.5 0V15Z" clip-rule="evenodd"/>
    </svg>
  </button>
  </div>

  <!-- Campaign log view: HTML panel below the map -->
  <Transition v-if="!embark" name="drawer">
    <div v-if="selectedLocation" class="drawer-panel">
      <div class="drawer">
        <WorldMapDrawerContent
          :selectedLocation="selectedLocation"
          :locationData="locationData"
          :mapData="mapData"
          :embark="embark"
          :isFinale="isFinale"
          @close="closePopup"
          @travelTo="travelToSelected"
          @travelVia="travelViaSelected"
          @travelWithTicket="travelWithTicket"
        />
      </div>
    </div>
  </Transition>
  </div>
</template>

<style scoped>
.map-wrapper {
  width: 100%;

  &:fullscreen, &:-webkit-full-screen {
    display: flex;
    align-items: center;
    justify-content: center;
    background: #000;

    .map-svg-container > svg {
      width: 100% !important;
      height: 100% !important;
      max-width: none !important;
      max-height: none !important;
    }
  }
}

.map-svg-container {
  position: relative;
  width: 100%;
}

svg {
  width: 100%;
  height: auto;
  box-shadow: 0 0 10px rgba(0,0,0,0.5);
  cursor: grab;
  display: block;

  &.dragging { cursor: grabbing; }
}

.embark-view svg {
  cursor: default;
}

/* In embark (StoryQuestion) view, cap height to available space below header */
.embark-view {
  max-width: calc((100vh - 80px) * 3000 / 1952);
  margin: 0 auto;
}

.embark-view svg {
  max-height: calc(100vh - 80px);
}

.expand-btn {
  position: absolute;
  bottom: 10px;
  right: 10px;
  background: rgba(0,0,0,0.5);
  border: none;
  border-radius: 6px;
  padding: 6px;
  cursor: pointer;
  color: white;
  display: flex;
  align-items: center;
  justify-content: center;
  opacity: 0.8;
  z-index: 10;
  transition: opacity 0.15s, background 0.15s;

  &:hover { opacity: 1; background: rgba(0,0,0,0.75); }

  svg {
    width: 22px;
    height: 22px;
    display: block;
    box-shadow: none;
    cursor: pointer;
  }
}

.pin-label {
  pointer-events: none;
  font-family: 'Arkham', sans-serif;
  font-size: 22px;
}

use {
  transform-box: fill-box;      /* use element’s own box */
  transform-origin: center;     /* scale about the center */
  transition: transform .15s ease;
}
.marker:hover {
  cursor: pointer;
  use {
    transform: scale(1.5);        /* now it grows in place */
  }
}

.route-base {
  stroke: #353944;         /* dark ink */
  stroke-width: 1.5;
  stroke-linecap: round;
  vector-effect: non-scaling-stroke;
}
.route-highlight {
  stroke: #ffffff;         /* light edge */
  stroke-width: 2.5;
  stroke-linecap: round;
  vector-effect: non-scaling-stroke;
  filter: url(#routeHighlight);
}
.route-hit {
  stroke: transparent;     /* large hit area for hover/click */
  stroke-width: 16;
  pointer-events: stroke;
}

.fullscreen-button {
  cursor: pointer;
  opacity: 0.85;
  &:hover { opacity: 1; }
}
/* optional hover pop */
/*.route:hover .route-base { stroke-width: 7 }
.route:hover .route-highlight { stroke-width: 3.6 }*/

/* Panel-specific overrides — use :deep() to reach into WorldMapDrawerContent child */
.drawer-panel header {
  padding: 10px 14px;
}

.drawer-panel .drawer-content {
  max-height: 300px;
  padding: 10px 14px 12px;
}

.drawer-panel .drawer :deep(h2) {
  font-size: 1.2rem;
  margin: 0 0 2px;
}

.drawer-panel .drawer :deep(> header > h3) {
  font-size: 0.85rem;
  color: #aaa;
  margin: 0;
}

.drawer-panel .drawer :deep(.close-btn) {
  font-size: 1.4rem;
  top: 0.3rem;
  right: 0.6rem;
}

.drawer-panel .drawer :deep(.dossier) {
  font-size: 0.8rem;
}

/* ── Embark (foreignObject) drawer ── */
.drawer-container {
  border-top-left-radius: 1rem;
  border-top-right-radius: 1rem;
  width: 100%;
  height: 100%;
}

/* ── Shared drawer inner shell ── */
.drawer {
  width: 100%;
  height: 100%;
  background: #1b2635;
  color: #fff;
  border-top-left-radius: 1rem;
  border-top-right-radius: 1rem;
  box-shadow: 0 -6px 20px rgba(0,0,0,0.5);
  font-size: 2rem;
  font-family: "Georgia", serif;
  display: flex;
  flex-direction: column;
  overflow: hidden;
  transition: all 0.1s linear;
  transform-origin: bottom;
  transform: translateY(0);
}

@starting-style {
  .drawer { transform: translateY(100%); }
}

/* ── Campaign-log HTML panel overrides ── */
.drawer-panel {
  overflow: hidden;

  .drawer {
    height: auto;
    font-size: 1rem;
    border-radius: 0;
    box-shadow: 0 6px 20px rgba(0,0,0,0.5);
    transform: none;
    transition: none;
  }
}

/* Embark drawer slides up from bottom of SVG */
.drawer-enter-from,
.drawer-leave-to {
  transform: translateY(100%);
}
.drawer-enter-active,
.drawer-leave-active {
  transform-origin: top;
  transition: transform 0.3s ease;
}
.drawer-enter-to,
.drawer-leave-from {
  transform: translateY(0%);
}

/* Campaign-log panel fades + slides down */
.drawer-panel.drawer-enter-from,
.drawer-panel.drawer-leave-to {
  opacity: 0;
  transform: translateY(-8px);
}
.drawer-panel.drawer-enter-active,
.drawer-panel.drawer-leave-active {
  transition: opacity 0.2s ease, transform 0.2s ease;
}
.drawer-panel.drawer-enter-to,
.drawer-panel.drawer-leave-from {
  opacity: 1;
  transform: translateY(0);
}

.label {
  font-family: 'MapTypewriter', serif;
  font-size: 16px;
  font-weight: bold;
  letter-spacing: 2px;
}
</style>
