<script setup lang="ts">
import { imgsrc } from '@/arkham/helpers';
import { computed, ref, inject } from 'vue'
import { ArrowsPointingOutIcon, ArrowsPointingInIcon } from '@heroicons/vue/24/solid'
import { Game } from '@/arkham/types/Game';

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
}
interface MapData {
  current: string
  available: MapLocationId[]
  locations: [MapLocationId, LocationData][]
} 

const send = inject<(msg: string) => void>('send', () => {})

const props = defineProps<{
  game: Game
  playerId: string
  mapData: MapData
}>()

const greenLocations = ['Arkham', 'Cairo', 'NewOrleans', 'Venice', 'MonteCarlo'] as MapLocationId[]

//convert props.mapData.locations to a Record<MapLocationId, LocationData>
const locationData = computed<Record<MapLocationId, LocationData>>(() => {
  const record: Record<MapLocationId, LocationData> = {} as Record<MapLocationId, LocationData>
  for (const [key, value] of props.mapData.locations) {
    if (greenLocations.includes(key)) {
      record[key] = {...value, travel: (value.travel ?? 0) + 1 }
    } else {
      record[key] = value
    }
  }
  return record
})


const data = computed(() => ({
  'Alexandria': { x: 1691, y: 613, unlocked: props.mapData.available.includes('Alexandria') },
  'Anchorage': { x: 226, y: 289, unlocked: props.mapData.available.includes('Anchorage') },
  'Arkham': { x: 771, y: 494, unlocked: props.mapData.available.includes('Arkham') },
  'Bermuda': { x: 815, y: 603, unlocked: props.mapData.available.includes('Bermuda') },
  'BermudaTriangle': { x: 728, y: 680, unlocked: props.mapData.available.includes('BermudaTriangle') },
  'Bombay': { x: 2117, y: 760, unlocked: props.mapData.available.includes('Bombay') },
  'BuenosAires': { x: 866, y: 1382, unlocked: props.mapData.available.includes('BuenosAires') },
  'Cairo': { x: 1706, y: 646, unlocked: props.mapData.available.includes('Cairo') },
  'Constantinople': { x: 1675, y: 503, unlocked: props.mapData.available.includes('Constantinople') },
  'Havana': { x: 617, y: 712, unlocked: props.mapData.available.includes('Havana') },
  'HongKong': { x: 2515, y: 719, unlocked: props.mapData.available.includes('HongKong') },
  'Kabul': { x: 2064, y: 568, unlocked: props.mapData.available.includes('Kabul') },
  'Kathmandu': { x: 2238, y: 673, unlocked: props.mapData.available.includes('Kathmandu') },
  'KualaLumpur': { x: 2412, y: 944, unlocked: props.mapData.available.includes('KualaLumpur') },
  'Lagos': { x: 1437, y: 904, unlocked: props.mapData.available.includes('Lagos') },  
  'London': { x: 1419, y: 386, unlocked: props.mapData.available.includes('London') },
  'Manokwari': { x: 2735, y: 987, unlocked: props.mapData.available.includes('Manokwari') },
  'Marrakesh': { x: 1334, y: 612, unlocked: props.mapData.available.includes('Marrakesh') },
  'MonteCarlo': { x: 1483, y: 472, unlocked: props.mapData.available.includes('MonteCarlo') },
  'Moscow': { x: 1746, y: 332, unlocked: props.mapData.available.includes('Moscow') },
  'Nairobi': { x: 1776, y: 990, unlocked: props.mapData.available.includes('Nairobi') },
  'NewOrleans': { x: 548, y: 635, unlocked: props.mapData.available.includes('NewOrleans') },
  'Perth': { x: 2506, y: 1345, unlocked: props.mapData.available.includes('Perth') },
  'Quito': { x: 624, y: 980, unlocked: props.mapData.available.includes('Quito') },
  'Reykjavik': { x: 1258, y: 249, unlocked: props.mapData.available.includes('Reykjavik') },
  'RioDeJaneiro': { x: 991, y: 1240, unlocked: props.mapData.available.includes('RioDeJaneiro') },
  'Rome': { x: 1530, y: 498, unlocked: props.mapData.available.includes('Rome') },
  'SanFrancisco': { x: 273, y: 545, unlocked: props.mapData.available.includes('SanFrancisco') },
  'SanJuan': { x: 758, y: 766, unlocked: props.mapData.available.includes('SanJuan') },
  'Shanghai': { x: 2565, y: 619, unlocked: props.mapData.available.includes('Shanghai') },
  'Stockholm': { x: 1568, y: 299, unlocked: props.mapData.available.includes('Stockholm') },
  'Sydney': { x: 2837, y: 1362, unlocked: props.mapData.available.includes('Sydney') },
  'Tokyo': { x: 2714, y: 573, unlocked: props.mapData.available.includes('Tokyo') },
  'Tunguska': { x: 2299, y: 295, unlocked: props.mapData.available.includes('Tunguska') },
  'Venice': { x: 1523, y: 453, unlocked: props.mapData.available.includes('Venice') },
  'YborCity': { x: 616, y: 657, unlocked: props.mapData.available.includes('YborCity') },
}))

const svgEl = ref<SVGSVGElement | null>(null)
const fullScreen = ref(false);
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

// close the popup
function closePopup() {
  selectedLocation.value = null
}

const coordinates = computed(() => {
  const loc = data.value[props.mapData.current as MapLocationId]
  if (loc) return { x: loc.x, y: loc.y }
  return null
})

const worldMap = imgsrc('world-map.jpg')

const toggleFullScreen = async () => {
  const el = svgEl.value
  if (!el) return

  if (!document.fullscreenElement) {
    // Request fullscreen for the SVG container
    await el.requestFullscreen?.()
    fullScreen.value = true
  } else {
    // Exit fullscreen mode
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
  <svg width="60vw" viewBox="0 0 3000 1952" fill="none" xmlns="http://www.w3.org/2000/svg" ref="svgEl">
    <defs>
      <g id="marker-red">
        <!-- outer red ring -->
        <circle r="13" fill="none" stroke="#d82e21" stroke-width="3"/>
        <!-- white ring -->
        <circle r="10" fill="#ffffff" stroke="#ffffff" stroke-width="3"/>
        <!-- red center -->
        <circle r="7" fill="#d82e21" stroke="#d82e21" stroke-width="1.6"/>
      </g>

      <g id="marker-blue">
        <!-- outer ring -->
        <circle r="13" fill="#EAEAEC" stroke="#31369B" stroke-width="3"/>
        <!-- 5-point star (radius ~11) -->
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
        <!-- outer green ring -->
        <circle r="9.5" fill="none" stroke="#2f6a3f" stroke-width="2.5"/>
        <!-- white ring -->
        <circle r="7" fill="#D6DFCC" stroke="#D6DFCC" stroke-width="2.5"/>
        <!-- white center -->
        <circle r="4" fill="#ffffff" stroke="#2f6a3f" stroke-width="2.5"/>
      </g>
    </defs>

    <image :href="worldMap" x="0" y="0" width="3000" height="1952"/>

    <g id="bermuda-triangle--bermuda" class="route" v-if="data.BermudaTriangle.unlocked === true">
      <path d="M 728 680 L 815 603" class="route-highlight"/>
      <path d="M 728 680 L 815 603" class="route-base"/>
      <path d="M 728 680 L 815 603" class="route-hit"/>
    </g>

    <g id="bermuda-triangle--ybor-city" class="route" v-if="data.BermudaTriangle.unlocked === true">
      <path d="M 728 680 L 616 657" class="route-highlight"/>
      <path d="M 728 680 L 616 657" class="route-base"/>
      <path d="M 728 680 L 616 657" class="route-hit"/>
    </g>

    <g id="bermuda-triangle--san-juan" class="route" v-if="data.BermudaTriangle.unlocked === true"> 
      <path d="M 728 680 L 758 766" class="route-highlight"/>
      <path d="M 728 680 L 758 766" class="route-base"/>
      <path d="M 728 680 L 758 766" class="route-hit"/>
    </g>

    <g id="anchorage" class="marker" transform="translate(226,289)" @click="select('Anchorage')">
      <use :href="`#marker-${data.Anchorage.unlocked ? 'blue' : 'red'}`"/>
      <rect width="153" x="-160" y="-11" height="23" fill="#00000000" />
    </g>

    <g id="san_francisco" class="marker" transform="translate(273,545)" @click="select('SanFrancisco')">
      <use :href="`#marker-${data.SanFrancisco.unlocked ? 'blue' : 'red'}`"/>
      <rect width="213" x="-218" y="-11" height="23" fill="#00000000" />
    </g>

    <g id="arkham" class="marker" transform="translate(771,494)" @click="select('Arkham')">
      <use href="#marker-green"/>
      <rect width="89" y="-11" height="20" fill="#00000000" />
    </g>

    <g id="ybor_city" class="marker" transform="translate(616,657)" @click="select('YborCity')">
      <use :href="`#marker-${data.YborCity.unlocked ? 'blue' : 'red'}`"/>
      <rect width="159" x="-155" y="-11" height="23" fill="#00000000" />
    </g>

    <g id="havana" class="marker" transform="translate(617,712)" @click="select('Havana')">
      <use :href="`#marker-${data.Havana.unlocked ? 'blue' : 'red'}`"/>
      <rect width="129" x="-125" y="-11" height="23" fill="#00000000" />
    </g>

    <g id="bermuda" class="marker" transform="translate(815,603)" @click="select('Bermuda')">
      <use :href="`#marker-${data.Bermuda.unlocked ? 'blue' : 'red'}`"/>
      <rect width="134" y="-11" height="23" fill="#00000000" />
    </g>

    <g id="new_orleans" class="marker" transform="translate(548,635)" @click="select('NewOrleans')">
      <use href="#marker-green"/>
      <rect width="134" x="-134" y="-11" height="20" fill="#00000000" />
    </g>

    <g id="san_juan" class="marker" transform="translate(758,766)" @click="select('SanJuan')">
      <use :href="`#marker-${data.SanJuan.unlocked ? 'blue' : 'red'}`"/>
      <rect width="145" y="-11" height="23" fill="#00000000" />
    </g>

    <g id="quito" class="marker" transform="translate(624,980)" @click="select('Quito')">
      <use :href="`#marker-${data.Quito.unlocked ? 'blue' : 'red'}`"/>
      <rect width="100" y="-11" height="23" fill="#00000000" />
    </g>

    <g id="rio_de_janeiro" class="marker" transform="translate(991,1240)" @click="select('RioDeJaneiro')">
      <use :href="`#marker-${data.RioDeJaneiro.unlocked ? 'blue' : 'red'}`"/>
      <rect width="220" x="-225" y="-11" height="23" fill="#00000000" />
    </g>

    <g id="buenos_aires" class="marker" transform="translate(866,1382)" @click="select('BuenosAires')">
      <use :href="`#marker-${data.BuenosAires.unlocked ? 'blue' : 'red'}`"/>
      <rect width="200" x="-200" y="-11" height="23" fill="#00000000" />
    </g>

    <g id="lagos" class="marker" transform="translate(1437,904)" @click="select('Lagos')">
      <use :href="`#marker-${data.Lagos.unlocked ? 'blue' : 'red'}`"/>
      <rect width="100" y="-11" height="23" fill="#00000000" />
    </g>

    <g id="nairobi" class="marker" transform="translate(1776,990)" @click="select('Nairobi')">
      <use :href="`#marker-${data.Nairobi.unlocked ? 'blue' : 'red'}`"/>
      <rect width="132" x="-132" y="-11" height="23" fill="#00000000" />
    </g>

    <g id="marrakesh" class="marker" transform="translate(1334,612)" @click="select('Marrakesh')">
      <use :href="`#marker-${data.Marrakesh.unlocked ? 'blue' : 'red'}`"/>
      <rect width="168" y="-11" height="23" fill="#00000000" />
    </g>

    <g id="alexandria" class="marker" transform="translate(1691,613)" @click="select('Alexandria')">
      <use :href="`#marker-${data.Alexandria.unlocked ? 'blue' : 'red'}`"/>
      <rect width="179" x="-179" y="-11" height="23" fill="#00000000" />
    </g>

    <g id="cairo" class="marker" transform="translate(1706,646)" @click="select('Cairo')">
      <use href="#marker-green"/>
      <rect width="75" y="-11" height="21" fill="#00000000" />
    </g>

    <g v-if="data.BermudaTriangle.unlocked === true" id="bermuda_triangle" class="marker" transform="translate(728,680)" @click="select('BermudaTriangle')">
      <use href="#marker-blue"/>
    </g>

    <g id="london" class="marker" transform="translate(1419,384)" @click="select('London')">
      <use :href="`#marker-${data.London.unlocked ? 'blue' : 'red'}`"/>
      <rect width="120" y="-11" height="23" fill="#00000000" />
    </g>

    <g id="reykjavik" class="marker" transform="translate(1258,249)" @click="select('Reykjavik')">
      <use :href="`#marker-${data.Reykjavik.unlocked ? 'blue' : 'red'}`"/>
      <rect width="161" y="-11" height="23" fill="#00000000" />
    </g>

    <g id="stockholm" class="marker" transform="translate(1568,299)" @click="select('Stockholm')">
      <use :href="`#marker-${data.Stockholm.unlocked ? 'blue' : 'red'}`"/>
      <rect width="155" x="-155" y="-11" height="23" fill="#00000000" />
    </g>

    <g id="rome" class="marker" transform="translate(1530,498)" @click="select('Rome')">
      <use :href="`#marker-${data.Rome.unlocked ? 'blue' : 'red'}`"/>
      <rect width="85" y="-11" height="23" fill="#00000000" />
    </g>

    <g id="moscow" class="marker" transform="translate(1746,332)" @click="select('Moscow')">
      <use :href="`#marker-${data.Moscow.unlocked ? 'blue' : 'red'}`"/>
      <rect width="120" y="-11" height="23" fill="#00000000" />
    </g>

    <g id="constantinople" class="marker" transform="translate(1675,503)" @click="select('Constantinople')">
      <use :href="`#marker-${data.Constantinople.unlocked ? 'blue' : 'red'}`"/>
      <rect width="230" y="-11" height="23" fill="#00000000" />
    </g>

    <g id="bombay" class="marker" transform="translate(2117,760)" @click="select('Bombay')">
      <use :href="`#marker-${data.Bombay.unlocked ? 'blue' : 'red'}`"/>
      <rect width="120" y="-11" height="23" fill="#00000000" />
    </g>

    <g id="kathmandu" class="marker" transform="translate(2238,673)" @click="select('Kathmandu')">
      <use :href="`#marker-${data.Kathmandu.unlocked ? 'blue' : 'red'}`"/>
      <rect width="170" y="-11" height="23" fill="#00000000" />
    </g>

    <g id="shanghai" class="marker" transform="translate(2565,619)" @click="select('Shanghai')">
      <use :href="`#marker-${data.Shanghai.unlocked ? 'blue' : 'red'}`"/>
      <rect width="150" x="-150" y="-11" height="23" fill="#00000000" />
    </g>

    <g id="tokyo" class="marker" transform="translate(2714,573)" @click="select('Tokyo')">
      <use :href="`#marker-${data.Tokyo.unlocked ? 'blue' : 'red'}`"/>
      <rect width="100" y="-11" height="23" fill="#00000000" />
    </g>

    <g id="perth" class="marker" transform="translate(2506,1345)" @click="select('Perth')">
      <use :href="`#marker-${data.Perth.unlocked ? 'blue' : 'red'}`"/>
      <rect width="100" y="-11" height="23" fill="#00000000" />
    </g>

    <g id="sydney" class="marker" transform="translate(2837,1362)" @click="select('Sydney')">
      <use :href="`#marker-${data.Sydney.unlocked ? 'blue' : 'red'}`"/>
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
      <use :href="`#marker-${data.Tunguska.unlocked ? 'blue' : 'red'}`"/>
      <rect width="153" y="-11" height="23" fill="#00000000" />
    </g>

    <g id="kabul" class="marker" transform="translate(2064,568)" @click="select('Kabul')">
      <use :href="`#marker-${data.Kabul.unlocked ? 'blue' : 'red'}`"/>
      <rect width="103" y="-11" height="23" fill="#00000000" />
    </g>

    <g id="hong_kong" class="marker" transform="translate(2515,719)" @click="select('HongKong')">
      <use :href="`#marker-${data.HongKong.unlocked ? 'blue' : 'red'}`"/>
      <rect width="160" x="-160" y="-11" height="23" fill="#00000000" />
    </g>

    <g id="kuala_lumpur" class="marker" transform="translate(2412,944)" @click="select('KualaLumpur')">
      <use :href="`#marker-${data.KualaLumpur.unlocked ? 'blue' : 'red'}`"/>
      <rect width="210" x="-210" y="-11" height="23" fill="#00000000" />
    </g>

    <g id="manokwari" class="marker" transform="translate(2735,987)" @click="select('Manokwari')">
      <use :href="`#marker-${data.Manokwari.unlocked ? 'blue' : 'red'}`"/>
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

    <!-- Button in the bottom right to toggle full screen -->
    <g
      class="fullscreen-button"
      @click="toggleFullScreen"
      :transform="`translate(${3000 - 90}, ${1952 - 90})`"
    >
      <foreignObject x="0" y="0" width="80" height="80">
        <ArrowsPointingOutIcon
          v-if="!fullScreen"
          :class="[
            'h-20 w-20 transition-colors duration-200 cursor-pointer',
            fullScreen
              ? 'text-gray-400'
              : 'text-gray-800'
          ]"
        />
        <ArrowsPointingInIcon
          v-else
          :class="[
            'h-20 w-20 transition-colors duration-200 cursor-pointer',
            fullScreen
              ? 'text-gray-400'
              : 'text-gray-800'
          ]"
        />
      </foreignObject>
    </g>

    <Transition name="drawer" mode="out-in">
      <foreignObject
        v-if="selectedLocation"
        :x="(3000 * 0.5) - ((3000 * 0.3) / 2)"
        :y="1952 * 0.5"
        :width="3000 * 0.3"
        :height="1952 * 0.5"
      >
        <div class="drawer">
          <button class="close-btn" @click.stop="closePopup">×</button>
          <h2 v-if="selectedLocation">{{ selectedLocation }}</h2>

          <div class="drawer-content" v-if="selectedLocation">
            <p><strong>Location:</strong> Russian Soviet Federative Socialist Republic</p>
            <p><strong>Travel time:</strong> {{locationData[selectedLocation].travel}}</p>
            <p>Lorem ipsum dolor sit amet, consectetur adipiscing elit...</p>
            <button class="action" @click="travelToSelected" >Travel here</button>
            <button class="action secondary">Travel here without stopping</button>
          </div>
        </div>
      </foreignObject>
    </Transition>
  </svg>
</template>

<style scoped>
svg {
  width: 60vw;
  margin: 0 auto;
  box-shadow: 0 0 10px rgba(0,0,0,0.5);
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
  color: red;
  &:hover {
    cursor: pointer;
    color: blue;
  }
}
/* optional hover pop */
/*.route:hover .route-base { stroke-width: 7 }
.route:hover .route-highlight { stroke-width: 3.6 }*/

/* Drawer container */
/* Header */
.drawer h2 {
  margin: 1rem 2rem 0.5rem;
}

/* Scrollable content */
.drawer-content {
  flex: 1;
  overflow-y: auto;
  padding: 0 2rem 2rem;
  scrollbar-width: thin;
}

/* Buttons inside the drawer */
.action {
  display: block;
  width: 100%;
  margin-top: 0.75rem;
  padding: 0.75rem;
  border-radius: 4px;
  background: #e2dfcc;
  color: #222;
  font-weight: bold;
  font-size: 2rem;
  cursor: pointer;
  border: none;
}
.action.secondary {
  background: #2e3a4f;
  color: #eee;
}

/* Close button */
.close-btn {
  position: absolute;
  top: 0.5rem;
  right: 1rem;
  background: none;
  border: none;
  color: #ccc;
  cursor: pointer;
  font-size: 3rem;
}

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
  transform: scaleY(1);
}

@starting-style {
  .drawer {
    transform: scaleY(0);
  }
}

.drawer-enter-from,
.drawer-leave-to {
  transform: scaleY(0);
}
.drawer-enter-active,
.drawer-leave-active {
  transform-origin: bottom;
  transition: transform 0.3s ease;
}
.drawer-enter-to,
.drawer-leave-from {
  transform: scaleY(1);
}
</style>
