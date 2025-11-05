<script setup lang="ts">
import { imgsrc } from '@/arkham/helpers';
import { computed, ref, inject } from 'vue'
import { ArrowsPointingOutIcon, ArrowsPointingInIcon } from '@heroicons/vue/24/solid'
import { Game } from '@/arkham/types/Game';
import { useI18n } from 'vue-i18n';

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
  'Kabul': { x: 2064, y: 568 },
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
  <svg width="90vw" viewBox="0 0 3000 1952" fill="none" xmlns="http://www.w3.org/2000/svg" ref="svgEl">
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
      <use :href="`#marker-${locationData.SanJuan.unlocked ? 'blue' : 'red'}`"/>
      <rect width="145" y="-11" height="23" fill="#00000000" />
    </g>

    <g id="quito" class="marker" transform="translate(624,980)" @click="select('Quito')">
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

    <g id="london" class="marker" transform="translate(1419,384)" @click="select('London')">
      <use :href="`#marker-${locationData.London.unlocked ? 'blue' : 'red'}`"/>
      <rect width="120" y="-11" height="23" fill="#00000000" />
    </g>

    <g id="reykjavik" class="marker" transform="translate(1258,249)" @click="select('Reykjavik')">
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
      <use :href="`#marker-${locationData.Tunguska.unlocked ? 'blue' : 'red'}`"/>
      <rect width="153" y="-11" height="23" fill="#00000000" />
    </g>

    <g id="kabul" class="marker" transform="translate(2064,568)" @click="select('Kabul')">
      <use :href="`#marker-${locationData.Kabul.unlocked ? 'blue' : 'red'}`"/>
      <rect width="103" y="-11" height="23" fill="#00000000" />
    </g>

    <g id="hong_kong" class="marker" transform="translate(2515,719)" @click="select('HongKong')">
      <use :href="`#marker-${locationData.HongKong.unlocked ? 'blue' : 'red'}`"/>
      <rect width="160" x="-160" y="-11" height="23" fill="#00000000" />
    </g>

    <g id="kuala_lumpur" class="marker" transform="translate(2412,944)" @click="select('KualaLumpur')">
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
        class="drawer-container"
      >
        <div class="drawer">
          <header>
            <button class="close-btn" @click.stop="closePopup">×</button>
            <h2 v-if="selectedLocation">{{ t(`theScarletKeys.locations.${selectedLocation}.name`) }}</h2>
            <h3 v-if="selectedLocation">{{ locationData[selectedLocation].subtitle }}</h3>
          </header>

          <div class="drawer-content" v-if="selectedLocation">
            <template v-if="selectedLocation === props.mapData.current">
              <p>You are currently here.</p>
            </template>
            <template v-else-if="embark === true">
              <p><strong>Travel time:</strong> {{locationData[selectedLocation].travel}}</p>
              <div v--if="selectedLocation === 'Venice'" class="side-story-info">
                <p>This is a side-story location.</p>
                <p>If you wish to add a side-story to this campaign, you may travel to this location and spend additional time equal to the normal experience for playing that side story.</p>
              </div>
              <template v-if="locationData[selectedLocation].unlocked">
                <button class="action" @click="travelToSelected">Travel here</button>
                <button
                  v-if="mapData.hasTicket && (locationData[selectedLocation].travel ?? 0) > 1"
                  class="action"
                  @click="travelWithTicket"
                >Travel with Expedited Ticket (1 time)</button>
                <button class="action secondary" @click="travelViaSelected">Travel here without stopping</button>
              </template>
              <template v-else>
                <p class="action locked">This location is currently locked.</p>
                <button class="action secondary" @click="travelViaSelected">Travel here without stopping</button>
              </template>
            </template>
            <div v-if="selectedLocation === 'Marrakesh'" class='dossier'>
              <header><h3>You may read this dossier at any time</h3></header>
              <section>
                <p>
                  File #11–B<br/>
                  Subject Class: Red<br/>
                  Real Identity: Unknown<br/>
                  Last Known Location: Marrakesh, Morocco
                </p>
                <p>
                  Description: Subject #11–B (hereinafter "Amaranth") is
                  a woman of European descent; appears to be in early
                  20s. Typically seen wearing a large red flower that
                  partly obscures her face.
                </p>
                <p>
                  Paradimensional Capabilities: Power to <span class='censor'>transfer life</span>
                  and <span class='censor'>death</span>. Channels <span class='censor'>energy</span> via <span class='censor'>extradimensional means</span> to <span class='censor'>heal wounded entities</span> and <span class='censor'>sap life</span> from
                  <span class='censor'>the living</span>. No recorded limits to this <span class='censor'>exchange</span>; data
                  limited only to minor events. Subject <span class='censor'>transfers life</span>
                  via tactile contact, which limits <span class='censor'>scale</span> of <span class='censor'>transfer</span>.
                  Possible <span class='censor'>near apocalyptic</span> ramifications.
                </p>
                <p>
                  Sightings: 1. November 23, 1923: Arkham, MA. Agents
                  witnessed subject healing wounds of <span class='censor'>fellow Coterie</span>
                  member, <span class='censor'>draining and shriveling</span> nearby trees.
                  Elevated <span class='censor'>Outsider/Paracausal</span> activity reported in
                  the months following.
                </p>
                <p class='indent'>
                  2. January 11, 1924: Lisbon, Portugal. Local cell
                  apprehended subject. Physical contact confirmed.
                  One cell member immediately deceased; second victim
                  remains catatonic. Subject eluded questioning.
                </p>
                <p class='indent'>
                  3. Unconfirmed sighting in Marrakesh. Further
                  intel required. Immediate assistance requested.
                </p>
                <p>
                  Approach Procedure: Subject is incredibly dangerous.
                  Engage only with extreme caution. Physical contact
                  prohibited. More information available in San
                  Francisco office (File #26–G2).
                </p>
              </section>
            </div>
            <div v-else-if="selectedLocation === 'BuenosAires'" class='dossier'>
              <header><h3>You may read this dossier at any time</h3></header>
              <section>
                <p>
                  File #16–D<br/>
                  Subject Class: Yellow<br/>
                  Real Identity: Unknown<br/>
                  Last Known Location: Buenos Aires, Argentina
                </p>
                <p>
                  Description: Subject #16–D (hereinafter "Girl in
                  Carmine Coat") is a woman of apparently Argentinian
                  descent (<span class='censor'>possible human</span> origin?), early 20s,
                  approximately 165cm in height. <span class='censor'>Extraterrestrial</span>
                  presence undetected (as of yet). Coterie paraphernalia
                  consists of stark red coat and matching hat possessive
                  of paradimensional faculty (see details below). Called
                  "La Chica Roja" by locals (no doubt a reference to her
                  attire, although it is unclear whether such title is a
                  term of endearment or infamy).
                </p>
                <p>
                  Paradimensional Capabilities: Reports by local
                  authorities suggest Girl in Carmine Coat is capable
                  of either <span class='censor'>hyperphysical speed</span> or <span class='censor'>ethereal</span> <span class='censor'>projection</span>.
                  Local authorities appear incapable of capturing
                  subject despite repeated attempts. (Can she manipulate
                  <span class='censor'>darkness</span> and <span class='censor'>shadows</span>? Or is she simply that good
                  at evading detection?) Subject seems to wish to avoid
                  conflict; as of yet, no deaths or harm to any locals
                  can be traced to Girl in Carmine Coat. (Cannot rule
                  out potential for violent escalation.)
                </p>
                <p>
                  Sightings: Girl in Carmine Coat is responsible for
                  several high profile burglaries in Buenos Aires.
                  Reports by local papers indicate she is still at large.
                </p>
                <p>
                  Approach Procedure: Despite her apparent <span class='censor'>coterie</span>
                  association, Girl in Carmine Coat does not appear to be
                  hostile. She has been heard speaking fluent Spanish,
                  Portugese, French, and English. For these reasons, we
                  believe subject can be interrogated and/or reasoned
                  with. Recommend capture and questioning regarding
                  <span class='censor'>coterie</span> motives.
                </p>
                <p>
                  Persons of Interest: Oficial Principal Matias Bolívar
                  (ma-tee-as boh-lee-vahr), principal officer in charge
                  of Girl in Carmine Coat's capture. (Possible asset?)
                </p>
              </section>
            </div>
            <div v-else-if="selectedLocation === 'Constantinople'" class='dossier'>
              <header><h3>You may read this dossier at any time</h3></header>
              <section>
                <p>
                  File #21–F<br/>
                  Subject Class: Green<br/>
                  Real Identity: Şahin, Ece (shah-heen, eh- jay)<br/>
                  Last Known Location: Constantinople, Turkey
                </p>
                <p>
                  Description: Subject #21–F (hereinafter "Lady in
                  Vermillion Veil" or real name "Ece Şahin") is a woman
                  of Turkish descent, 34 years of age, approximately
                  161cm in height. It is unknown whether this is
                  subject's <span class='censor'>true form</span> or a disguise or false identity.
                  At all times, subject wears traditional hijab of
                  unusual stark red color, likely a <span class='censor'>paradimensionally bound</span> object designating coterie membership.
                </p>
                <p>
                  Paradimensional Capabilities: Lady in Vermillion
                  Veil appears to either possess no such capabilities,
                  or has successfully hidden such capabilities from
                  Foundation intelligence.
                </p>
                <p>
                  Sightings: Ece Şahin is a well known and renowned
                  art collector and museum curator operating in the
                  Turkish and Islamic Arts Museum in Constantinople.
                  She has either <span class='censor'>integrated completely with human society</span> or is, for all intents and purposes, a normal
                  everyday person. (Note: Leadership is under assumption
                  <span class='censor'>Ece Şahin is a well constructed cover story, but has no evidence to back up such assumptions.</span>)
                </p>
                <p>
                  Approach Procedure: Ece has already reached out
                  to Foundation envoys. Dispatch to confront and
                  question intentions.
                </p>
              </section>
            </div>
            <div v-else-if="selectedLocation === 'SanFrancisco' || selectedLocation === 'Moscow'" class='dossier'>
              <header><h3>You may read this dossier at any time</h3></header>
              <section>
                <p>
                  File #26–G1–6<br />
                  Sanctum Class: Green<br />
                  Sanctum Locations: <span class='censor'>Cape Town</span>, <span class='censor'>Union of South Africa</span>;
                  San Francisco, California; <span class='censor'>Seoul</span>, <span class='censor'>South Korea</span>; Moscow,
                  Russia; <span class='censor'>Bruges</span>, <span class='censor'>Belgium</span>; <span class='censor'>Bern</span>, <span class='censor'>Switzerland</span>.
                </p>
                <p>
                  Description: Sanctums #26–G1 through G6
                  are Foundation offices and storehouses for
                  paradimensional artifacts. Do not disclose locations
                  of sanctums with non–Foundation personnel under
                  penalty of <span class='censor'>expulsion and/or indefinite imprisonment</span>.
                </p>
                <p>
                  Approach Procedure: Agent in charge of sanctum will
                  meet you upon arrival. Entry instructions attached.
                </p>
              </section>
            </div>
            <div v-else-if="selectedLocation === 'Havana'" class='dossier'>
              <header><h3>You may read this dossier at any time</h3></header>
              <section>
                <p>
                  File #28–I<br />
                  Subject Class: Yellow<br />
                  Real Identity: Delgado Álvarez, Desiderio (del-gah-doh al-vah-rez, deh-see-deh-ree-oh)<br />
                  Last Known Location: Havana, Cuba
                </p>
                <p>
                  Description: Subject #28–I (hereinafter "Man in
                  Blood–Soaked Suit" or "Desiderio Delgado Álvarez") is
                  a man of Cuban descent, approximately 42 years old
                  and 192cm in height. Typically seen in a black suit
                  accessorized with apparent coterie paraphernalia,
                  including tie and hat, all with red accents.
                </p>
                <p>
                  Paradimensional Capabilities: Man in Blood–Soaked
                  Suit does not appear to possess any extraordinary
                  powers aside from peak <span class='censor'>human</span> physical performance,
                  and unusually high skill and accuracy with firearms.
                  (<span class='censor'>Perhaps</span> enhanced <span class='censor'>by a Key?</span>) Approach with caution.
                </p>
                <p>
                  Sightings: Mr. Álvarez is a longtime resident of
                  Havana. However, no records exist pertaining to
                  subject's childhood or early life. (Likely destroyed
                  upon induction into Coterie, but possibility remains
                  of <span class='censor'>extraterrestrial origin</span>. Either way, his identity
                  may be an alias.) Mr. Álvarez is known to frequent a
                  nightclub known as Cafe Luna.
                </p>
                <p>
                  Note: Foundation has no knowledge pertaining to
                  any Key in Havana, however, Mr. Álvarez is likely to
                  know location of other Keys.
                </p>
                <p>
                  Approach Procedure: Open negotiations with subject to
                  acquire location of Coterie vaults and/or hideouts.
                  If he does not cooperate, subdue and interrogate. More
                  information available in Moscow office (File #26–G4).
                </p>
              </section>
            </div>
            <div v-else-if="selectedLocation === 'Shanghai'" class='dossier'>
              <header><h3>You may read this dossier at any time</h3></header>
              <section>
                <p>
                  File #32–J<br />
                  Asset Name: Flint, Li<br />
                  Area of Operation: Shanghai, China
                </p>
                <p>
                  Profile: Recently acquired agent in charge
                  of undisclosed cell reporting directly to
                  Commissioner Taylor.
                </p>
                <p>
                  Current Assignment: According to recent report, asset
                  has split from rest of cell and is currently in
                  Shanghai investigating whereabouts of subject #46–Q.
                </p>
                <p>
                  Notes: Asset loyalty to Foundation cause is unsure.
                  Extreme vigilance recommended.
                </p>
              </section>
            </div>
            <div v-else-if="selectedLocation === 'Anchorage'" class='dossier'>
              <header><h3>You may read this dossier at any time</h3></header>
              <section>
                <p>
                  File #33–K<br />
                  Subject Class: Yellow<br />
                  Real Identity: Unknown.<br />
                  Last Known Location: Anchorage, Alaska.
                </p>
                <p>
                  Description: Subject #33–K (hereinafter "Thorne") is
                  a tall, gaunt person with androgynous features and
                  fair complexion. Prefers practical, loose–fitting
                  clothing. Subject appears to be <span class='censor'>of Anglo–Celtic descent</span>
                  and conjectured to be far older than they appear
                  (collaborating source suggests D.O.B. in <span class='censor'>1761</span>). Subject
                  wears a distinct red cravat around their neck, often
                  obscuring their face.
                </p>
                <p>
                  Paradimensional Capabilities: Subject possesses acute
                  sensitivity to <span class='censor'>otherworldly residue</span>. Coterie leverages
                  these capabilities to locate <span class='censor'>otherworldly</span> artifacts
                  and track movement of <span class='censor'>paradimensional entities</span>.
                </p>
                <p>
                  Sightings: Numerous sightings across the globe.
                  Profiled as one of the most mobile Coterie members.
                  Despite notorious secrecy, Thorne has a reputation
                  for appraising and acquiring <span class='censor'>paradimensional</span>
                  artifacts. Analysis suggests they are amenable to
                  business exchange and receptive to haggling. Recent
                  activity suggests Coterie is in a state of <span class='censor'>aggressive acquisition</span>.
                </p>
                <p>
                  Approach Procedure: You and Thorne likely have
                  similar objectives. Extreme caution suggested. Thorne
                  is likely to negotiate but only on their terms.
                </p>
              </section>
            </div>
            <div v-else-if="selectedLocation === 'Tokyo'" class='dossier'>
              <header><h3>You may read this dossier at any time</h3></header>
              <section>
                <p>
                  File #37–M<br />
                  Asset Name: Taylor, Qiana<br />
                  Area of Operation: See current assignment status.
                </p>
                <p>
                  Profile: <span class='censor'>Commissioner Taylor was appointed as head
                  of the Foundation by unanimous consent. Most people
                  will never see the rest of this text, because I
                  really want this whole section to just look like
                  an enormous block of redacted "who knows what she
                  is up to" kind of thing, so I will redact it, but
                  somebody in the PDF might still be able to read it,
                  so oh well. Hey, if you're reading this: kudos, that
                  is awesome. I appreciate that you cared enough to
                  go through the effort of figuring out what is here.
                  Thank you for playing. Much love.</span>
                </p>
                <p>
                  Current Assignment: <span class='censor'>Oh, here we go with the rest of
                  the text, then, okay. Commissioner is looking into
                  reports of paradimensional activity in a number
                  of places. Current task list includes</span> Tokyo, Japan;
                  <span class='censor'>Prague, Czechoslovakia; and</span> Lagos, Nigeria.
                </p>
              </section>
            </div>
            <div v-else-if="selectedLocation === 'Alexandria'" class='dossier'>
              <header><h3>You may read this dossier at any time</h3></header>
              <section>
                <p>
                  File #38–N<br />
                  Subject Class: Red<br />
                  Real Identity: Unknown<br />
                  Last Known Location: Alexandria, Egypt
                </p>
                <p>
                  Description: Subject #38–N (hereinafter "Beast in
                  Cowl of Crimson") is a roughly humanoid figure,
                  approximately 183cm in height, clothed in a red
                  cloak. The hood of this cloak effectively masks the
                  figure's identity (very likely to be <span class='censor'>extraterrestrial</span>
                  in origin). Only appendage as of yet observed is a
                  set of <span class='censor'>wolflike</span> claws <span class='censor'>covered in gray fur</span>, hence the
                  moniker. <span class='censor'>Inhuman origin</span> seems likely.
                </p>
                <p>
                  Paradimensional Capabilities: Beast in Cowl of
                  Crimson appears to have high aptitude for physical
                  confrontation. Prior encounters have led to <span class='censor'>the deaths of several agents</span>. Victims are <span class='censor'>eviscerated</span>
                  and <span class='censor'>left to bleed out</span>, or <span class='censor'>partially devoured and battered around like a bored cat playing with its food</span>. Extreme caution is advised.
                </p>
                <p>
                  Sightings: Beast in Cowl of Crimson is prime suspect
                  in several gruesome murders throughout Alexandria.
                </p>
                <p>
                  Note: Paradimensional implement (aka "Key") known to
                  exist in Alexandria. Coterie element may be using
                  key to <span class='censor'>transform or mutate their body</span>.
                </p>
                <p>
                  Approach Procedure: Subdue subject with extreme
                  prejudice and acquire paradimensional implement for
                  further study.
                </p>
              </section>
            </div>
            <div v-else-if="selectedLocation === 'RioDeJaneiro' || selectedLocation == 'Perth'" class='dossier'>
              <header><h3>You may read this dossier at any time</h3></header>
              <section>
                <p>
                  File #44–O/55–X<br />
                  Asset Name: Irawan, Dewi<br />
                  Area of Operation: Southern Hemisphere
                </p>
                <p>
                  Profile: Prominent zoologist has published several
                  surveys regarding disappearing wildlife. Zoologist
                  claims entire species and the memory of their
                  existence are being erased. Possible <span class='censor'>paracausal</span>
                  disturbance.
                </p>
              </section>
            </div>
            <div v-else-if="selectedLocation === 'Sydney'" class='dossier'>
              <header><h3>You may read this dossier at any time</h3></header>
              <section>
                <p>
                  File #49–R<br />
                  Asset Name: Quinn, Ari<br />
                  Area of Operation: Sydney, Australia
                </p>
                <p>
                  Profile: Field researcher in charge of
                  paradimensional analysis. Despite recent events,
                  asset has not yet requested transfer. Recommend
                  continued psychiatric evaluation.
                </p>
                <p>
                  Current Assignment: Agent Quinn is currently
                  performing independent research regarding recent
                  paradimensional disturbances.
                </p>
              </section>
            </div>
            <div v-else-if="selectedLocation === 'YborCity'" class='dossier'>
              <header><h3>You may read this dossier at any time</h3></header>
              <section>
                <p>
                  File #52–U<br />
                  Sanctum Class: Yellow<br />
                  Sanctum Location: Ybor City, Florida
                </p>
                <p>
                  Description: Sanctum #52–U is an abandoned cigar
                  factory in Ybor City, north of McKay Bay.
                </p>
                <p>
                  Sightings: Subject #28–I known to frequent sanctum.
                  Be on alert.
                </p>
                <p>
                  Approach Procedure: Enter with caution and search
                  for signs of paradimensional implements. Take any
                  and all evidence into Foundation custody.
                </p>
              </section>
            </div>
            <div v-else-if="selectedLocation === 'Kathmandu'" class='dossier'>
              <header><h3>You may read this dossier at any time</h3></header>
              <section>
                <p>
                  File #53–V<br />
                  Subject Class: Undetermined<br />
                  Real Identity: Uperetria, Aliki Zoni (oo-peh-reh-tree- ah, a-lee-kee zoh-nee)<br />
                  Last Known Location: Kathmandu, Nepal
                </p>
                <p>
                  Description: Subject #53–V (hereinafter "Maid With
                  Scarlet Sash") appears as a teenage girl of unknown
                  descent wearing a white dress and red sash. <span class='censor'>It is unclear as of now whether such physical form is a true representation or a false image</span>.
                </p>
                <p>
                  Paradimensional Capabilities: <span class='censor'>Aliki is thought to possess uncanny knowledge beyond her years, and is likely much, much older than she appears. Foundation has yet to confirm paradimensional capabilities of bound key, if any. Subject may be incorporeal</span>.
                </p>
                <p>
                  Sightings: Locals in region have reported seeing
                  a "spirit" matching subject's description, heralded
                  by a high–pitched whistling sound. Unclear if <span class='censor'>Maid with Scarlet Sash is indeed ectoplasmic in nature, or if her paradimensional abilities reinforce such a description</span>.
                </p>
                <p>
                  Approach Procedure: Subject Risk Class uncomfirmed.
                  Do not approach.
                </p>
              </section>
            </div>
            <div v-else-if="selectedLocation === 'Nairobi'" class='dossier'>
              <header><h3>You may read this dossier at any time</h3></header>
              <section>
                <p>
                  File #54–W<br />
                  Subject Class: Green<br />
                  Real Identity: Masai, Tuwile (muh-sah-ee, t-wayl)<br />
                  Last Known Location: Nairobi, Kenya
                </p>
                <p>
                  Description: Subject #54–W (hereinafter "Tuwile
                  Masai") is a slim man of Kenyan descent, middle–
                  aged, and 176cm in height. Coterie signifiers include
                  large red spectacles, <span class='censor'>access to and disposal</span> of
                  <span class='censor'>Coterie funding</span>, and <span class='censor'>confirmed contact</span> with Coterie
                  operatives. Subject has written and published
                  openly under their name in numerous geological and
                  archeological periodicals.
                </p>
                <p>
                  Paradimensional Capabilities: No known reports.
                  Subject appears to possess no such capabilities,
                  or has successfully hidden <span class='censor'>several bound paradimensional implements</span> from Foundation intelligence.
                </p>
                <p>
                  Sightings: Tuwile Masai teaches under a fellowship
                  at Oxford University and has published numerous
                  surveys on work at and around Lake Victoria.
                </p>
                <p>
                  Approach Procedure: Masai has spurned all Foundation
                  contact thus far. Operatives may wish to approach
                  only if Masai has reason to trust them.
                </p>
              </section>
            </div>
          </div>
        </div>
      </foreignObject>
    </Transition>
  </svg>
</template>

<style scoped>
svg {
  width: auto;
  max-width: 95%;
  min-width: 60vw;
  max-height: 95%;
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

/* Scrollable content */
.drawer-content {
  flex: 1;
  overflow-y: auto;
  padding: 0 2rem 2rem;
  scrollbar-width: thin;
  margin-top: 1em;
}

/* Buttons inside the drawer */
.action {
  display: block;
  width: 100%;
  margin-top: 0.75rem;
  padding: 0.75rem;
  border-radius: 4px;
  font-weight: bold;
  background: #2e3a4f;
  color: #eee;
  font-size: 2rem;
  cursor: pointer;
  border: none;
  &:hover {
    background: #3b4a6b;
  }
}

.action.secondary {
  background: #e2dfcc;
  color: #222;
  &:hover {
    background: #ccc9b3;
  }
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

.drawer-container {
  border-top-left-radius: 1rem;
  border-top-right-radius: 1rem;
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
  transform: translateY(0);
}

@starting-style {
  .drawer {
    transform: translateY(100%);
  }
}

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

header {
  background: rgba(255,255,255,0.2);
  color: var(--title);
  font-size: 1.5em;
  margin: 0;
  padding: 0;
  padding: 10px 15px;
  border-top-left-radius: 1rem;
  border-top-right-radius: 1rem;
  text-align: center;
}

h2 {
  color: white;
}

h3 {
  font-size: 0.6em;
  font-weight: normal;
  color: #ccc;
}

p.locked {
  color: #888;
  background-color: darkred;
  font-style: italic;
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

.censor {
  background-color: black;
  color: black;
  padding: 0 4px;
  border-radius: 2px;
}

.indent {
  text-indent: 1.5em;
}

.side-story-info {
  text-align: left;
  margin-block: 1em;
  display: flex;
  flex-direction: column;
  gap: 0.5em;
}
</style>
