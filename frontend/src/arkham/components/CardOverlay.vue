<script lang="ts" setup>
import { ref, computed, onMounted, onUnmounted } from 'vue';
import { imgsrc } from '@/arkham/helpers'

const cardOverlay = ref<HTMLElement | null>(null);
const hoveredElement = ref<HTMLElement | null>(null);

onMounted(() => {
  const handleMouseover = (event: Event) => {
    const target = event.target as HTMLElement;
    if (target && (target.classList.contains('card') || target.dataset.imageId || target.dataset.target || target.dataset.image)) {
      hoveredElement.value = target;
    } else {
      hoveredElement.value = null;
    }
  };

  document.addEventListener('mouseover', handleMouseover);

  onUnmounted(() => {
    document.removeEventListener('mouseover', handleMouseover);
  });
});

const card = computed(() => {
  if (!hoveredElement.value) return null;
  return getImage(hoveredElement.value);
});

const allCustomizations = ["09021", "09022", "09023", "09040", "09041", "09042", "09059", "09060", "09061", "09079", "09080", "09081", "09099", "09100", "09101", "09119"]

const cardCode = computed(() => {
  if (card.value) {
    const pattern = /cards\/(\d+)\.jpg/;
    const match = card.value.match(pattern);
    if (match) {
      return match[1];
    }
  }
  return null;
});

const customizationsCard = computed(() => {
  if (cardCode.value) {
    if (allCustomizations.includes(cardCode.value)) {
      return imgsrc(`customizations/${cardCode.value}.jpg`);
    }
  }
  return null;
});

const customizationTicks = computed(() => {
  if (cardCode.value && customizations.value) {
    const customizationObject: string[] = [];

    customizations.value.forEach((customization: [number, number]) => {
        const firstValue = customization[0];
        const secondValue = customization[1];
        for (let i = 1; i <= secondValue; i++) {
          const key = `customization-${cardCode.value}-${firstValue}-${i}`;
          customizationObject.push(key);
        }
    });

    return customizationObject;
  }

  return [];
});

const customizations = computed(() => {
  if (!hoveredElement.value) return null;
  const str = hoveredElement.value.dataset.customizations

  if (!str) return null;

  const pairs = str.split(",").map(Number); // Convert string to array of numbers
  const customizations = [];

  // Group pairs into tuples
  for (let i = 0; i < pairs.length; i += 2) {
    customizations.push([pairs[i], pairs[i + 1]]);
  }

  return customizations.length === 0 ? null : customizations;
});

const fight = computed(() => {
  if (!hoveredElement.value) return null;
  return hoveredElement.value.dataset.fight;
});

const health = computed(() => {
  if (!hoveredElement.value) return null;
  return hoveredElement.value.dataset.health;
});

const damage = computed(() => {
  if (!hoveredElement.value) return null;
  return hoveredElement.value.dataset.damage;
});

const horror = computed(() => {
  if (!hoveredElement.value) return null;
  return hoveredElement.value.dataset.horror;
});

const victory = computed(() => {
  if (!hoveredElement.value) return null;
  return hoveredElement.value.dataset.victory;
});

const keywords = computed(() => {
  if (!hoveredElement.value) return null;
  return hoveredElement.value.dataset.keywords;
});

const evade = computed(() => {
  if (!hoveredElement.value) return null;
  return hoveredElement.value.dataset.evade;
});

const reversed = computed(() => {
  return hoveredElement.value?.classList.contains('Reversed') ?? false;
});

const overlayPosition = computed(() => {
  if (!hoveredElement.value) return { top: 0, left: 0 };
  return getPosition(hoveredElement.value);
});

const sideways = computed<boolean>(() => {
  if (!hoveredElement.value) return false;

  const rect = hoveredElement.value.getBoundingClientRect();

  return rect.width > rect.height;
})

const getRotated = (el: HTMLElement) => {
  const elementsToCheck = [el, el.parentElement];

  for (let i = 0; i < elementsToCheck.length; i++) {
    if (elementsToCheck[i]) {
      const style = window.getComputedStyle(elementsToCheck[i] as Element);
      const matrix = new WebKitCSSMatrix(style.transform);
      const angle = Math.round(Math.atan2(matrix.m21, matrix.m11) * (180 / Math.PI));

      if (Math.abs(angle) === 90 || Math.abs(angle) === -270) {
          return true
      }
    }
  }

  return false
}
const getPosition = (el: HTMLElement) => {
  const rect = el.getBoundingClientRect();
  const overlayWidth = 300; // Adjust this value if the overlay width changes

  // Calculate the ratio based on the orientation of the card
  const rotated = getRotated(el);
  const ratio = rotated ? rect.height / rect.width : rect.width / rect.height;

  // Calculate the height of the overlay based on the ratio
  const width = sideways.value ? (overlayWidth / ratio) : 300
  const height = sideways.value ? 300 : overlayWidth / ratio;

  // Calculate the top position, ensuring it doesn't go off the screen
  const top = rect.top + window.scrollY - 40;
  const bottom = top + height;
  const newTop = Math.max(0, bottom > window.innerHeight ? rect.bottom - height + window.scrollY - 40 : top);

  // Calculate the left position, adjusting for rotated cards
  const left = rect.left + window.scrollX + rect.width + 10;

  if (left + width >= window.innerWidth) {
    return { top: newTop, left: rect.left - overlayWidth - 10 };
  } else {
    return { top: newTop, left: left };
  }
};

const getImage = (el: HTMLElement): string | null => {
  if (el instanceof HTMLImageElement && el.classList.contains('card') && !el.closest(".revelation")) {
    return el.src;
  }

  if (el instanceof HTMLDivElement && el.classList.contains('card')) {
    return el.style.backgroundImage.slice(4, -1).replace(/"/g, "");
  }

  if (el.dataset.imageId) {
    return imgsrc(`cards/${el.dataset.imageId}.jpg`);
  }

  if (el.dataset.target) {
    const target = document.querySelector(`[data-id="${el.dataset.target}"]`) as HTMLElement;
    return target ? getImage(target) : null;
  }

  if (el.dataset.image) {
    return el.dataset.image;
  }

  return null;
};
</script>

<template>
  <div class="card-overlay" ref="cardOverlay" :style="{ top: overlayPosition.top + 'px', left: overlayPosition.left + 'px'}" :class="{ sideways }">
    <img v-if="card" :src="card" :class="{ reversed }" />
    <span class="fight" v-if="fight">{{ fight }}</span>
    <span class="health" v-if="health">{{ health }}</span>
    <span class="evade" v-if="evade">{{ evade }}</span>
    <span class="victory" v-if="victory">Victory {{ victory }}.</span>
    <span class="keywords" v-if="keywords">{{ keywords }}.</span>
    <img class="damage damage-1" v-if="damage && damage >= 1" :src="imgsrc('damage-overlay.png')"/>
    <img class="damage damage-2" v-if="damage && damage >= 2" :src="imgsrc('damage-overlay.png')"/>
    <img class="damage damage-3" v-if="damage && damage >= 3" :src="imgsrc('damage-overlay.png')"/>
    <img class="horror horror-1" v-if="horror && horror >= 1" :src="imgsrc('horror-overlay.png')"/>
    <img class="horror horror-2" v-if="horror && horror >= 2" :src="imgsrc('horror-overlay.png')"/>
    <img class="horror horror-3" v-if="horror && horror >= 3" :src="imgsrc('horror-overlay.png')"/>
    <div v-if="customizationTicks.length > 0" class="customizations-wrapper">
      <img v-if="customizationsCard" :src="customizationsCard" />
      <div v-for="tick in customizationTicks" :key="tick" :class="`tick tick-${cardCode} ${tick}`">
        <svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24"><path d="M20.285 2l-11.285 11.567-5.286-5.011-3.714 3.716 9 8.728 15-15.285z"/></svg>
      </div>
    </div>
  </div>
</template>

<style lang="scss">
.fight, .evade, .health {
  font-family: "Teutonic";
  position: absolute;
  color: white;
  font-weight: bold;
  font-size: 1.3em;
  text-shadow:
    1px 1px 0 #000,
    -1px 1px 0 #000,
    -1px -1px 0 #000,
    1px -1px 0 #000;
}

.fight {
  top: 11%;
  left: 30%;
}

.health {
  font-size: 1.6em;
  top: 10%;
  transform: translate(-50%, 0);
  left:50%;
}

.evade {
  top: 11%;
  left: 68%;
}

.victory {
  position: absolute;
  color: rgba(0, 0, 0, 0.6);
  top: 47%;
  transform: translate(-50%, 0);
  left:50%;
  font-weight: 900;
  font-size: 0.8em;
}

.keywords {
  width: 80%;
  position: absolute;
  color: rgba(0, 0, 0, 0.6);
  top: 23.2%;
  left:13%;
  font-weight: bold;
  font-size: 0.8em;
}

.card-overlay {
  position: absolute;
  z-index: 1000;
  max-width: 420px;
  max-height: 420px;
  height: fit-content;
  display: flex;
  img {
    border-radius: 15px;
    width: 300px;
    height: fit-content;
  }
  img.damage {
    width: auto;
    height: 22px;
    position: absolute;
    top: 53.5%;
    left: 34.5%;
    &.damage-2 {
      top: 52%;
      left: 27.5%;
    }

    &.damage-3 {
      top: 50.5%;
      left: 20.5%;
    }


  }
  img.horror {
    width: auto;
    height: 22px;
    position: absolute;
    top: 53.5%;
    left: 57.5%;
    &.horror-2 {
      top: 52%;
      left: 64.5%;
    }

    &.horror-3 {
      top: 50.5%;
      left: 71.5%;
    }

  }
  &.sideways {
    height: 300px !important;
    width: fit-content !important;
    img {
      border-radius: 15px;
      height: 300px;
      width: fit-content;
    }
  }
}

.reversed {
  transform: rotateZ(180deg);
}

.customizations-wrapper {
  position: relative;
  width: fit-content;
  height: fit-content;
  margin-left: 2px;
}

.tick {
  line-height: 0;
  position: absolute;
  width: 2.8%;
  height: 2.8%;

  svg {
    width: 100%;
    height: 100%;
  }
}

.tick-09021 {
  --top-0: 19.1%;
  --top-1: 30.0%;
  --top-2: 40.9%;
  --top-3: 45.1%;
  --top-4: 49.4%;
  --top-5: 60.3%;
  --top-6: 74.4%;
  --left-1: 8.6%;
  --left-2: 11.6%;
  --left-3: 15.2%;
}


.customization-09021-0-1 {
  top: var(--top-0);
  left: var(--left-1);
}

.customization-09021-1-1 {
  top: var(--top-1);
  left: var(--left-1);
}
.customization-09021-1-2 {
  top: var(--top-1);
  left: var(--left-2);
}
.customization-09021-2-1 {
  top: var(--top-2);
  left: var(--left-1);
}
.customization-09021-2-2 {
  top: var(--top-2);
  left: var(--left-2);
}
.customization-09021-3-1 {
  top: var(--top-3);
  left: var(--left-1);
}
.customization-09021-3-2 {
  top: var(--top-3);
  left: var(--left-2);
}
.customization-09021-4-1 {
  top: var(--top-4);
  left: var(--left-1);
}
.customization-09021-4-2 {
  top: var(--top-4);
  left: var(--left-2);
}
.customization-09021-5-1 {
  top: var(--top-5);
  left: var(--left-1);
}
.customization-09021-5-2 {
  top: var(--top-5);
  left: var(--left-2);
}
.customization-09021-5-3 {
  top: var(--top-5);
  left: var(--left-3);
}
.customization-09021-6-1 {
  top: var(--top-6);
  left: var(--left-1);
}
.customization-09021-6-2 {
  top: var(--top-6);
  left: var(--left-2);
}
.customization-09021-6-3 {
  top: var(--top-6);
  left: var(--left-3);
}

.tick-09119 {
  --top-0: 19.0%;
  --top-1: 28.4%;
  --top-2: 40.6%;
  --top-3: 55.6%;
  --top-4: 67.9%;
  --top-5: 80.1%;
  --top-6: 86.4%;
  --left-1: 8.4%;
  --left-2: 11.2%;
  --left-3: 14.2%;
  --left-4: 17.3%;
}


.customization-09119-0-1 {
  top: var(--top-0);
  left: var(--left-1);
}

.customization-09119-0-2 {
  top: var(--top-0);
  left: var(--left-2);
}
.customization-09119-1-1 {
  top: var(--top-1);
  left: var(--left-1);
}
.customization-09119-1-2 {
  top: var(--top-1);
  left: var(--left-2);
}
.customization-09119-2-1 {
  top: var(--top-2);
  left: var(--left-1);
}
.customization-09119-2-2 {
  top: var(--top-2);
  left: var(--left-2);
}
.customization-09119-3-1 {
  top: var(--top-3);
  left: var(--left-1);
}
.customization-09119-3-2 {
  top: var(--top-3);
  left: var(--left-2);
}
.customization-09119-4-1 {
  top: var(--top-4);
  left: var(--left-1);
}
.customization-09119-4-2 {
  top: var(--top-4);
  left: var(--left-2);
}
.customization-09119-5-1 {
  top: var(--top-5);
  left: var(--left-1);
}
.customization-09119-5-2 {
  top: var(--top-5);
  left: var(--left-2);
}
.customization-09119-5-3 {
  top: var(--top-5);
  left: var(--left-3);
}
.customization-09119-5-4 {
  top: var(--top-5);
  left: var(--left-4);
}
.customization-09119-6-1 {
  top: var(--top-6);
  left: var(--left-1);
}
.customization-09119-6-2 {
  top: var(--top-6);
  left: var(--left-2);
}
.customization-09119-6-3 {
  top: var(--top-6);
  left: var(--left-3);
}
.customization-09119-6-4 {
  top: var(--top-6);
  left: var(--left-4);
}

</style>
