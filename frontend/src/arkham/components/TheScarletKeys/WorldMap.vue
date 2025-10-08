<script setup lang="ts">
import { imgsrc } from '@/arkham/helpers';
import { computed } from 'vue'

const props = defineProps<{
  position: string
}>()

const coordinates = computed(() => {
  switch (props.position) {
    case 'London':
      return { x: 1419, y: 386 };
    default: return null;
  }
})

const worldMap = imgsrc('world-map.jpg')

function select(location: string) {
  alert(`Selected location: ${location}`);
}

</script>

<template>
  <svg width="60vw" viewBox="0 0 3000 1952" fill="none" xmlns="http://www.w3.org/2000/svg">
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

    <g id="bermuda-triangle--bermuda" class="route">
      <path d="M 728 680 L 815 603" class="route-highlight"/>
      <path d="M 728 680 L 815 603" class="route-base"/>
      <path d="M 728 680 L 815 603" class="route-hit"/>
    </g>

    <g id="bermuda-triangle--ybor-city" class="route">
      <path d="M 728 680 L 616 657" class="route-highlight"/>
      <path d="M 728 680 L 616 657" class="route-base"/>
      <path d="M 728 680 L 616 657" class="route-hit"/>
    </g>

    <g id="bermuda-triangle--san-juan" class="route">
      <path d="M 728 680 L 758 766" class="route-highlight"/>
      <path d="M 728 680 L 758 766" class="route-base"/>
      <path d="M 728 680 L 758 766" class="route-hit"/>
    </g>

    <g id="anchorage" class="marker" transform="translate(226,289)" @click="select('Anchorage')">
      <use href="#marker-blue"/>
    </g>

    <g id="san_francisco" class="marker" transform="translate(273,545)" @click="select('San Francisco')">
      <use href="#marker-blue"/>
    </g>

    <g id="arkham" class="marker" transform="translate(771,494)" @click="select('Arkham')">
      <use href="#marker-green"/>
    </g>

    <g id="ybor_city" class="marker" transform="translate(616,657)" @click="select('Ybor City')">
      <use href="#marker-blue"/>
    </g>

    <g id="havana" class="marker" transform="translate(617,712)" @click="select('Havana')">
      <use href="#marker-blue"/>
    </g>

    <g id="bermuda" class="marker" transform="translate(815,603)" @click="select('Bermuda')">
      <use href="#marker-blue"/>
    </g>

    <g id="new_orleans" class="marker" transform="translate(548,635)" @click="select('New Orleans')">
      <use href="#marker-green"/>
    </g>

    <g id="san_juan" class="marker" transform="translate(758,766)" @click="select('San Juan')">
      <use href="#marker-red"/>
    </g>

    <g id="quito" class="marker" transform="translate(624,980)" @click="select('Quito')">
      <use href="#marker-red"/>
    </g>

    <g id="rio_de_janeiro" class="marker" transform="translate(991,1240)" @click="select('Rio de Janeiro')">
      <use href="#marker-blue"/>
    </g>

    <g id="buenos_aires" class="marker" transform="translate(866,1382)" @click="select('Buenos Aires')">
      <use href="#marker-blue"/>
    </g>

    <g id="lagos" class="marker" transform="translate(1437,904)" @click="select('Lagos')">
      <use href="#marker-blue"/>
    </g>

    <g id="nairobi" class="marker" transform="translate(1776,990)" @click="select('Nairobi')">
      <use href="#marker-blue"/>
    </g>

    <g id="marrakesh" class="marker" transform="translate(1334,612)" @click="select('Marrakesh')">
      <use href="#marker-blue"/>
    </g>

    <g id="alexandria" class="marker" transform="translate(1691,613)" @click="select('Alexandria')">
      <use href="#marker-blue"/>
    </g>

    <g id="cairo" class="marker" transform="translate(1706,646)" @click="select('Cairo')">
      <use href="#marker-green"/>
    </g>

    <g id="bermuda_triangle" class="marker" transform="translate(728,680)" @click="select('Bermuda Triangle')">
      <use href="#marker-red"/>
    </g>

    <g id="london" class="marker" transform="translate(1419,384)" @click="select('London')">
      <use href="#marker-red"/>
    </g>

    <g id="reykjavik" class="marker" transform="translate(1258,249)" @click="select('Reykjavik')">
      <use href="#marker-red"/>
    </g>

    <g id="stockholm" class="marker" transform="translate(1568,299)" @click="select('Stockholm')">
      <use href="#marker-blue"/>
    </g>

    <g id="rome" class="marker" transform="translate(1530,498)" @click="select('Rome')">
      <use href="#marker-blue"/>
    </g>

    <g id="moscow" class="marker" transform="translate(1746,332)" @click="select('Moscow')">
      <use href="#marker-blue"/>
    </g>

    <g id="constantinople" class="marker" transform="translate(1675,503)" @click="select('Constantinople')">
      <use href="#marker-blue"/>
    </g>

    <g id="bombay" class="marker" transform="translate(2117,760)" @click="select('Bombay')">
      <use href="#marker-blue"/>
    </g>

    <g id="kathmandu" class="marker" transform="translate(2238,673)" @click="select('Kathmandu')">
      <use href="#marker-blue"/>
    </g>

    <g id="shanghai" class="marker" transform="translate(2565,619)" @click="select('Shanghai')">
      <use href="#marker-blue"/>
    </g>

    <g id="tokyo" class="marker" transform="translate(2714,573)" @click="select('Tokyo')">
      <use href="#marker-blue"/>
    </g>

    <g id="perth" class="marker" transform="translate(2506,1345)" @click="select('Perth')">
      <use href="#marker-blue"/>
    </g>

    <g id="sydney" class="marker" transform="translate(2837,1362)" @click="select('Sydney')">
      <use href="#marker-blue"/>
    </g>

    <g id="venice" class="marker" transform="translate(1523,453)" @click="select('Venice')">
      <use href="#marker-green"/>
    </g>

    <g id="monte_carlo" class="marker" transform="translate(1483,472)" @click="select('Monte Carlo')">
      <use href="#marker-green"/>
    </g>

    <g id="tunguska" class="marker" transform="translate(2299,295)" @click="select('Tunguska')">
      <use href="#marker-red"/>
    </g>

    <g id="kabul" class="marker" transform="translate(2064,568)" @click="select('Kabul')">
      <use href="#marker-red"/>
    </g>

    <g id="hong_kong" class="marker" transform="translate(2515,719)" @click="select('Hong Kong')">
      <use href="#marker-red"/>
    </g>

    <g id="kuala_lumpur" class="marker" transform="translate(2412,944)" @click="select('Kuala Lumpur')">
      <use href="#marker-red"/>
    </g>

    <g id="manokwari" class="marker" transform="translate(2735,987)" @click="select('Manokwari')">
      <use href="#marker-red"/>
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
use:hover {
  cursor: pointer;
  transform: scale(1.5);        /* now it grows in place */
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
/* optional hover pop */
/*.route:hover .route-base { stroke-width: 7 }
.route:hover .route-highlight { stroke-width: 3.6 }*/
</style>
