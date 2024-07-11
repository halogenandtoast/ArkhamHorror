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

    customizations.value.forEach((customization: [number, [number, string[]]]) => {
        const firstValue = customization[0];
        const secondValue = customization[1][0];
        for (let i = 1; i <= secondValue; i++) {
          const key = `customization-${cardCode.value}-${firstValue}-${i}`;
          customizationObject.push(key);
        }
    });

    return customizationObject;
  }

  return [];
});

const customizationLabels = computed(() => {
  if (cardCode.value && customizations.value) {
    const customizationObject: [string, string][] = [];

    customizations.value.forEach((customization: [number, [number, string[]]]) => {
      const firstValue = customization[0];
      const thirdValue = customization[1][1];
      for (let j = 0; j < thirdValue.length; j++) {
        if (thirdValue[j].tag === "ChosenCard" || thirdValue[j].tag === "ChosenTrait") {
          const key = `label-${cardCode.value}-${firstValue}-${j}`;
          customizationObject.push([key, thirdValue[j].contents]);
        }
      }
    });

    return customizationObject;
  }

  return [];
});

const customizationSkills = computed(() => {
  if (cardCode.value && customizations.value) {
    const customizationObject: string[] = [];

    customizations.value.forEach((customization: [number, [number, string[]]]) => {
      const thirdValue = customization[1][1];
      for (let j = 0; j < thirdValue.length; j++) {
        if (thirdValue[j].tag === "ChosenSkill") {
          customizationObject.push(`skill-${cardCode.value}-${thirdValue[j].contents}`);
        }
      }
    });

    return customizationObject;
  }

  return [];
});

const customizationIndexes = computed(() => {
  if (cardCode.value && customizations.value) {
    const customizationObject: string[] = [];

    customizations.value.forEach((customization: [number, [number, string[]]]) => {
      const firstValue = customization[0];
      const thirdValue = customization[1][1];
      for (let j = 0; j < thirdValue.length; j++) {
        if (thirdValue[j].tag === "ChosenIndex") {
          customizationObject.push(`index-${cardCode.value}-${firstValue}-${thirdValue[j].contents}`);
        }
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

  let customizations
  try { customizations = JSON.parse(str) } catch (e) { console.log(str); customizations = [] } ;
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
      <div v-for="label in customizationLabels" :key="label[0]" :class="`label label-${cardCode} ${label[0]}`">
        <svg xmlns="http://www.w3.org/2000/svg" width="100" height="20" viewBox="0 0 100 20">
         <g>
          <path id="svg-text" d="M 0 10 H 100" fill="transparent" stroke="lightgray" />
          <text><textPath xlink:href="#svg-text" method="stretch" lengthAdjust="spacingAndGlyphs" textLength="100%">{{ label[1] }}</textpath></text>
         </g>
        </svg>
      </div>
      <div v-for="skill in customizationSkills" :key="skill" :class="`skill skill-${cardCode} ${skill}`">
      </div>
      <div v-for="index in customizationIndexes" :key="index" :class="`index index-${cardCode} ${index}`">
      </div>
      <div v-for="tick in customizationTicks" :key="tick" :class="`tick tick-${cardCode} ${tick}`">
        <svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24"><path d="M20.285 2l-11.285 11.567-5.286-5.011-3.714 3.716 9 8.728 15-15.285z"/></svg>
      </div>
    </div>
  </div>
</template>

<style scoped lang="scss">
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

.label {
  line-height: 1;
  position: absolute;
  width: 100%;
  height: 1em;
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

.skill {
  line-height: 0;
  position: absolute;
  width: 7%;
  aspect-ratio: 1/1;
  border-radius: 50%;
  border: 1px solid #222;
  box-sizing: border-box;
  background-color: rgba(0,0,0,0.4);
}

.index {
  line-height: 0;
  position: absolute;
  width: 7%;
  aspect-ratio: 1/1;
  border-radius: 50%;
  border: 1px solid #222;
  box-sizing: border-box;
  background-color: rgba(0,0,0,0.4);
}

// Hunter's Armor
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

// Runic Axe
.tick-09022 {
  --top-0: 18.6%;
  --top-1: 25.3%;
  --top-2: 34.9%;
  --top-3: 47.2%;
  --top-4: 56.7%;
  --top-5: 69.3%;
  --top-6: 75.9%;
  --top-7: 82.4%;
  --left-1: 8.6%;
  --left-2: 11.6%;
  --left-3: 14.7%;
  --left-4: 17.7%;
}

.customization-09022-0-1 {
  top: var(--top-0);
  left: var(--left-1);
}
.customization-09022-1-1 {
  top: var(--top-1);
  left: var(--left-1);
}
.customization-09022-2-1 {
  top: var(--top-2);
  left: var(--left-1);
}
.customization-09022-3-1 {
  top: var(--top-3);
  left: var(--left-1);
}
.customization-09022-4-1 {
  top: var(--top-4);
  left: var(--left-1);
}
.customization-09022-4-2 {
  top: var(--top-4);
  left: var(--left-2);
}
.customization-09022-5-1 {
  top: var(--top-5);
  left: var(--left-1);
}
.customization-09022-5-2 {
  top: var(--top-5);
  left: var(--left-2);
}
.customization-09022-5-3 {
  top: var(--top-5);
  left: var(--left-3);
}
.customization-09022-6-1 {
  top: var(--top-6);
  left: var(--left-1);
}
.customization-09022-6-2 {
  top: var(--top-6);
  left: var(--left-2);
}
.customization-09022-6-3 {
  top: var(--top-6);
  left: var(--left-3);
}
.customization-09022-7-1 {
  top: var(--top-7);
  left: var(--left-1);
}
.customization-09022-7-2 {
  top: var(--top-7);
  left: var(--left-2);
}
.customization-09022-7-3 {
  top: var(--top-7);
  left: var(--left-3);
}
.customization-09022-7-4 {
  top: var(--top-7);
  left: var(--left-4);
}

// Custom Modifications
.tick-09023 {
  --top-0: 19.1%;
  --top-1: 33.4%;
  --top-2: 40.9%;
  --top-3: 51.7%;
  --top-4: 62.5%;
  --top-5: 73.3%;
  --left-1: 8.6%;
  --left-2: 11.9%;
  --left-3: 15.4%;
  --left-4: 19.1%;
}

.customization-09023-0-1 {
  top: var(--top-0);
  left: var(--left-1);
}
.customization-09023-1-1 {
  top: var(--top-1);
  left: var(--left-1);
}
.customization-09023-1-2 {
  top: var(--top-1);
  left: var(--left-2);
}
.customization-09023-2-1 {
  top: var(--top-2);
  left: var(--left-1);
}
.customization-09023-2-2 {
  top: var(--top-2);
  left: var(--left-2);
}
.customization-09023-3-1 {
  top: var(--top-3);
  left: var(--left-1);
}
.customization-09023-3-2 {
  top: var(--top-3);
  left: var(--left-2);
}
.customization-09023-3-3 {
  top: var(--top-3);
  left: var(--left-3);
}
.customization-09023-4-1 {
  top: var(--top-4);
  left: var(--left-1);
}
.customization-09023-4-2 {
  top: var(--top-4);
  left: var(--left-2);
}
.customization-09023-4-3 {
  top: var(--top-4);
  left: var(--left-3);
}
.customization-09023-5-1 {
  top: var(--top-5);
  left: var(--left-1);
}
.customization-09023-5-2 {
  top: var(--top-5);
  left: var(--left-2);
}
.customization-09023-5-3 {
  top: var(--top-5);
  left: var(--left-3);
}
.customization-09023-5-4 {
  top: var(--top-5);
  left: var(--left-4);
}

// Alchemical Distillation
.tick-09040 {
  --top-0: 19.1%;
  --top-1: 26.7%;
  --top-2: 34.3%;
  --top-3: 45.1%;
  --top-4: 52.8%;
  --top-5: 60.2%;
  --top-6: 74.3%;
  --left-1: 8.6%;
  --left-2: 11.9%;
  --left-3: 15.4%;
  --left-4: 19.1%;
  --left-5: 22.4%;
}

.customization-09040-0-1 {
  top: var(--top-0);
  left: var(--left-1);
}
.customization-09040-1-1 {
  top: var(--top-1);
  left: var(--left-1);
}
.customization-09040-2-1 {
  top: var(--top-2);
  left: var(--left-1);
}
.customization-09040-3-1 {
  top: var(--top-3);
  left: var(--left-1);
}
.customization-09040-4-1 {
  top: var(--top-4);
  left: var(--left-1);
}
.customization-09040-4-2 {
  top: var(--top-4);
  left: var(--left-2);
}
.customization-09040-5-1 {
  top: var(--top-5);
  left: var(--left-1);
}
.customization-09040-5-2 {
  top: var(--top-5);
  left: var(--left-2);
}
.customization-09040-5-3 {
  top: var(--top-5);
  left: var(--left-3);
}
.customization-09040-5-4 {
  top: var(--top-5);
  left: var(--left-4);
}
.customization-09040-6-1 {
  top: var(--top-6);
  left: var(--left-1);
}
.customization-09040-6-2 {
  top: var(--top-6);
  left: var(--left-2);
}
.customization-09040-6-3 {
  top: var(--top-6);
  left: var(--left-3);
}
.customization-09040-6-4 {
  top: var(--top-6);
  left: var(--left-4);
}
.customization-09040-6-5 {
  top: var(--top-6);
  left: var(--left-5);
}

// Empirical Hypothesis
.tick-09041 {
  --top-0: 18.4%;
  --top-1: 25.2%;
  --top-2: 31.8%;
  --top-3: 38.3%;
  --top-4: 45.0%;
  --top-5: 57.4%;
  --top-6: 67.0%;
  --top-7: 76.5%;
  --left-1: 8.6%;
  --left-2: 11.5%;
  --left-3: 14.4%;
  --left-4: 17.5%;
}

.customization-09041-0-1 {
  top: var(--top-0);
  left: var(--left-1);
}
.customization-09041-1-1 {
  top: var(--top-1);
  left: var(--left-1);
}
.customization-09041-2-1 {
  top: var(--top-2);
  left: var(--left-1);
}
.customization-09041-3-1 {
  top: var(--top-3);
  left: var(--left-1);
}
.customization-09041-4-1 {
  top: var(--top-4);
  left: var(--left-1);
}
.customization-09041-4-2 {
  top: var(--top-4);
  left: var(--left-2);
}
.customization-09041-5-1 {
  top: var(--top-5);
  left: var(--left-1);
}
.customization-09041-5-2 {
  top: var(--top-5);
  left: var(--left-2);
}
.customization-09041-5-3 {
  top: var(--top-5);
  left: var(--left-3);
}
.customization-09041-5-4 {
  top: var(--top-5);
  left: var(--left-4);
}
.customization-09041-6-1 {
  top: var(--top-6);
  left: var(--left-1);
}
.customization-09041-6-2 {
  top: var(--top-6);
  left: var(--left-2);
}
.customization-09041-6-3 {
  top: var(--top-6);
  left: var(--left-3);
}
.customization-09041-7-1 {
  top: var(--top-7);
  left: var(--left-1);
}
.customization-09041-7-2 {
  top: var(--top-7);
  left: var(--left-2);
}
.customization-09041-7-3 {
  top: var(--top-7);
  left: var(--left-3);
}
.customization-09041-7-4 {
  top: var(--top-7);
  left: var(--left-4);
}

// The Raven Quill
.tick-09042 {
  --top-1: 24.8%;
  --top-2: 31.2%;
  --top-3: 37.7%;
  --top-4: 44.4%;
  --top-5: 51.0%;
  --top-6: 60.3%;
  --top-7: 70.0%;
  --left-1: 8.6%;
  --left-2: 11.5%;
  --left-3: 14.4%;
  --left-4: 17.5%;
}

.label-09042-0-0 {
  top: 18%;
  left: 52%;
  width: 40%;
  height: 5%;
  svg {
    width: 100%;
    height: 100%;
    path {
      d: path("M 0 14 H 100");
    }
  }
}

.label-09042-4-0 {
  top: 46.5%;
  left: 18%;
  width: 36%;
  height: 5%;
  svg {
    width: 100%;
    height: 100%;
    path {
      d: path("M 0 14 H 100");
    }
  }
}

.label-09042-4-1 {
  top: 46.5%;
  left: 55%;
  width: 35%;
  height: 5%;
  svg {
    width: 100%;
    height: 100%;
    path {
      d: path("M 0 14 H 100");
    }
  }
}

.customization-09042-1-1 {
  top: var(--top-1);
  left: var(--left-1);
}
.customization-09042-2-1 {
  top: var(--top-2);
  left: var(--left-1);
}
.customization-09042-3-1 {
  top: var(--top-3);
  left: var(--left-1);
}
.customization-09042-3-2 {
  top: var(--top-3);
  left: var(--left-2);
}
.customization-09042-4-1 {
  top: var(--top-4);
  left: var(--left-1);
}
.customization-09042-4-2 {
  top: var(--top-4);
  left: var(--left-2);
}
.customization-09042-5-1 {
  top: var(--top-5);
  left: var(--left-1);
}
.customization-09042-5-2 {
  top: var(--top-5);
  left: var(--left-2);
}
.customization-09042-6-1 {
  top: var(--top-6);
  left: var(--left-1);
}
.customization-09042-6-2 {
  top: var(--top-6);
  left: var(--left-2);
}
.customization-09042-6-3 {
  top: var(--top-6);
  left: var(--left-3);
}
.customization-09042-7-1 {
  top: var(--top-7);
  left: var(--left-1);
}
.customization-09042-7-2 {
  top: var(--top-7);
  left: var(--left-2);
}
.customization-09042-7-3 {
  top: var(--top-7);
  left: var(--left-3);
}
.customization-09042-7-4 {
  top: var(--top-7);
  left: var(--left-4);
}

// Damning Testimony
.tick-09059 {
  --top-0: 18.6%;
  --top-1: 32.7%;
  --top-2: 40.2%;
  --top-3: 47.6%;
  --top-4: 61.9%;
  --top-5: 72.9%;
  --left-1: 8.5%;
  --left-2: 11.9%;
  --left-3: 15.2%;
  --left-4: 18.5%;
}

.customization-09059-0-1 {
  top: var(--top-0);
  left: var(--left-1);
}
.customization-09059-1-1 {
  top: var(--top-1);
  left: var(--left-1);
}
.customization-09059-1-2 {
  top: var(--top-1);
  left: var(--left-2);
}
.customization-09059-2-1 {
  top: var(--top-2);
  left: var(--left-1);
}
.customization-09059-2-2 {
  top: var(--top-2);
  left: var(--left-2);
}
.customization-09059-3-1 {
  top: var(--top-3);
  left: var(--left-1);
}
.customization-09059-3-2 {
  top: var(--top-3);
  left: var(--left-2);
}
.customization-09059-3-3 {
  top: var(--top-3);
  left: var(--left-3);
}
.customization-09059-4-1 {
  top: var(--top-4);
  left: var(--left-1);
}
.customization-09059-4-2 {
  top: var(--top-4);
  left: var(--left-2);
}
.customization-09059-4-3 {
  top: var(--top-4);
  left: var(--left-3);
}
.customization-09059-5-1 {
  top: var(--top-5);
  left: var(--left-1);
}
.customization-09059-5-2 {
  top: var(--top-5);
  left: var(--left-2);
}
.customization-09059-5-3 {
  top: var(--top-5);
  left: var(--left-3);
}
.customization-09059-5-4 {
  top: var(--top-5);
  left: var(--left-4);
}

// Friends in Low Places
.tick-09060 {
  --top-1: 24.3%;
  --top-2: 34.0%;
  --top-3: 46.5%;
  --top-4: 56.0%;
  --top-5: 65.4%;
  --top-6: 72.1%;
  --top-7: 78.6%;
  --left-1: 8.5%;
  --left-2: 11.3%;
  --left-3: 14.2%;
}

.label-09060-0-0 {
  top: 18%;
  left: 29%;
  width: 40%;
  height: 5%;
  svg {
    width: 100%;
    height: 100%;
    path {
      d: path("M 0 14 H 100");
    }
  }
}

.label-09060-2-0 {
  top: 33.4%;
  left: 66%;
  width: 25%;
  height: 5%;
  svg {
    width: 100%;
    height: 100%;
    path {
      d: path("M 0 14 H 100");
    }
  }
}

.customization-09060-1-1 {
  top: var(--top-1);
  left: var(--left-1);
}
.customization-09060-2-1 {
  top: var(--top-2);
  left: var(--left-1);
}
.customization-09060-2-2 {
  top: var(--top-2);
  left: var(--left-2);
}
.customization-09060-3-1 {
  top: var(--top-3);
  left: var(--left-1);
}
.customization-09060-3-2 {
  top: var(--top-3);
  left: var(--left-2);
}
.customization-09060-4-1 {
  top: var(--top-4);
  left: var(--left-1);
}
.customization-09060-4-2 {
  top: var(--top-4);
  left: var(--left-2);
}
.customization-09060-5-1 {
  top: var(--top-5);
  left: var(--left-1);
}
.customization-09060-5-2 {
  top: var(--top-5);
  left: var(--left-2);
}
.customization-09060-6-1 {
  top: var(--top-6);
  left: var(--left-1);
}
.customization-09060-6-2 {
  top: var(--top-6);
  left: var(--left-2);
}
.customization-09060-6-3 {
  top: var(--top-6);
  left: var(--left-3);
}
.customization-09060-7-1 {
  top: var(--top-7);
  left: var(--left-1);
}
.customization-09060-7-2 {
  top: var(--top-7);
  left: var(--left-2);
}
.customization-09060-7-3 {
  top: var(--top-7);
  left: var(--left-3);
}

// Honed Instinct
.tick-09061 {
  --top-0: 19.0%;
  --top-1: 25.6%;
  --top-2: 32.3%;
  --top-3: 38.6%;
  --top-4: 45.4%;
  --top-5: 52.1%;
  --top-6: 58.6%;
  --top-7: 68.2%;
  --left-1: 8.4%;
  --left-2: 11.3%;
  --left-3: 14.2%;
  --left-4: 17.3%;
  --left-5: 20.6%;
}

.customization-09061-0-1 {
  top: var(--top-0);
  left: var(--left-1);
}
.customization-09061-0-2 {
  top: var(--top-0);
  left: var(--left-2);
}
.customization-09061-1-1 {
  top: var(--top-1);
  left: var(--left-1);
}
.customization-09061-1-2 {
  top: var(--top-1);
  left: var(--left-2);
}
.customization-09061-2-1 {
  top: var(--top-2);
  left: var(--left-1);
}
.customization-09061-2-2 {
  top: var(--top-2);
  left: var(--left-2);
}
.customization-09061-3-1 {
  top: var(--top-3);
  left: var(--left-1);
}
.customization-09061-3-2 {
  top: var(--top-3);
  left: var(--left-2);
}
.customization-09061-4-1 {
  top: var(--top-4);
  left: var(--left-1);
}
.customization-09061-4-2 {
  top: var(--top-4);
  left: var(--left-2);
}
.customization-09061-5-1 {
  top: var(--top-5);
  left: var(--left-1);
}
.customization-09061-5-2 {
  top: var(--top-5);
  left: var(--left-2);
}
.customization-09061-5-3 {
  top: var(--top-5);
  left: var(--left-3);
}
.customization-09061-5-4 {
  top: var(--top-5);
  left: var(--left-4);
}
.customization-09061-6-1 {
  top: var(--top-6);
  left: var(--left-1);
}
.customization-09061-6-2 {
  top: var(--top-6);
  left: var(--left-2);
}
.customization-09061-6-3 {
  top: var(--top-6);
  left: var(--left-3);
}
.customization-09061-6-4 {
  top: var(--top-6);
  left: var(--left-4);
}
.customization-09061-7-1 {
  top: var(--top-7);
  left: var(--left-1);
}
.customization-09061-7-2 {
  top: var(--top-7);
  left: var(--left-2);
}
.customization-09061-7-3 {
  top: var(--top-7);
  left: var(--left-3);
}
.customization-09061-7-4 {
  top: var(--top-7);
  left: var(--left-4);
}
.customization-09061-7-5 {
  top: var(--top-7);
  left: var(--left-5);
}

// Living Ink
.tick-09079 {
  --top-1: 25.5%;
  --top-2: 36.5%;
  --top-3: 50.7%;
  --top-4: 61.6%;
  --top-5: 65.7%;
  --top-6: 69.9%;
  --top-7: 80.8%;
  --left-1: 8.4%;
  --left-2: 11.8%;
  --left-3: 15.2%;
}

.skill-09079-SkillWillpower {
  top: 18.6%;
  left: 42.5%;
}

.skill-09079-SkillIntellect {
  top: 18.6%;
  left: 55%;
}

.skill-09079-SkillCombat {
  top: 18.6%;
  left: 68.4%;
}

.skill-09079-SkillAgility {
  top: 18.6%;
  left: 81%;
}

.customization-09079-1-1 {
  top: var(--top-1);
  left: var(--left-1);
}
.customization-09079-2-1 {
  top: var(--top-2);
  left: var(--left-1);
}
.customization-09079-3-1 {
  top: var(--top-3);
  left: var(--left-1);
}
.customization-09079-3-2 {
  top: var(--top-3);
  left: var(--left-2);
}
.customization-09079-4-1 {
  top: var(--top-4);
  left: var(--left-1);
}
.customization-09079-4-2 {
  top: var(--top-4);
  left: var(--left-2);
}
.customization-09079-5-1 {
  top: var(--top-5);
  left: var(--left-1);
}
.customization-09079-5-2 {
  top: var(--top-5);
  left: var(--left-2);
}
.customization-09079-5-3 {
  top: var(--top-5);
  left: var(--left-3);
}
.customization-09079-6-1 {
  top: var(--top-6);
  left: var(--left-1);
}
.customization-09079-6-2 {
  top: var(--top-6);
  left: var(--left-2);
}
.customization-09079-6-3 {
  top: var(--top-6);
  left: var(--left-3);
}
.customization-09079-7-1 {
  top: var(--top-7);
  left: var(--left-1);
}
.customization-09079-7-2 {
  top: var(--top-7);
  left: var(--left-2);
}
.customization-09079-7-3 {
  top: var(--top-7);
  left: var(--left-3);
}

// Summoned Servitor
.tick-09080 {
  --top-0: 18.3%;
  --top-1: 27.8%;
  --top-2: 37.4%;
  --top-3: 49.8%;
  --top-4: 56.4%;
  --top-5: 66.0%;
  --top-6: 72.6%;
  --top-7: 82.0%;
  --left-1: 8.4%;
  --left-2: 11.2%;
  --left-3: 14.3%;
  --left-4: 17.3%;
  --left-5: 20.3%;
}

.index-09080-5-0 {
  top: 68.4%;
  height: 4%;
  border-radius: 40%;
  left: 36.5%;
  width: 11.5%;
}

.index-09080-5-1 {
  top: 68.4%;
  height: 4%;
  border-radius: 40%;
  left: 49.5%;
  width: 14%;
}

.customization-09080-0-1 {
  top: var(--top-0);
  left: var(--left-1);
}
.customization-09080-1-1 {
  top: var(--top-1);
  left: var(--left-1);
}
.customization-09080-2-1 {
  top: var(--top-2);
  left: var(--left-1);
}
.customization-09080-3-1 {
  top: var(--top-3);
  left: var(--left-1);
}
.customization-09080-4-1 {
  top: var(--top-4);
  left: var(--left-1);
}
.customization-09080-5-1 {
  top: var(--top-5);
  left: var(--left-1);
}
.customization-09080-5-2 {
  top: var(--top-5);
  left: var(--left-2);
}
.customization-09080-6-1 {
  top: var(--top-6);
  left: var(--left-1);
}
.customization-09080-6-2 {
  top: var(--top-6);
  left: var(--left-2);
}
.customization-09080-6-3 {
  top: var(--top-6);
  left: var(--left-3);
}
.customization-09080-7-1 {
  top: var(--top-7);
  left: var(--left-1);
}
.customization-09080-7-2 {
  top: var(--top-7);
  left: var(--left-2);
}
.customization-09080-7-3 {
  top: var(--top-7);
  left: var(--left-3);
}
.customization-09080-7-4 {
  top: var(--top-7);
  left: var(--left-4);
}
.customization-09080-7-5 {
  top: var(--top-7);
  left: var(--left-5);
}

// Power Word
.tick-09081 {
  --top-0: 18.5%;
  --top-1: 28.2%;
  --top-2: 37.6%;
  --top-3: 47.1%;
  --top-4: 56.7%;
  --top-5: 63.4%;
  --top-6: 72.9%;
  --top-7: 79.5%;
  --left-1: 8.4%;
  --left-2: 11.2%;
  --left-3: 14.2%;
}

.customization-09081-0-1 {
  top: var(--top-0);
  left: var(--left-1);
}
.customization-09081-1-1 {
  top: var(--top-1);
  left: var(--left-1);
}
.customization-09081-2-1 {
  top: var(--top-2);
  left: var(--left-1);
}
.customization-09081-3-1 {
  top: var(--top-3);
  left: var(--left-1);
}
.customization-09081-4-1 {
  top: var(--top-4);
  left: var(--left-1);
}
.customization-09081-4-2 {
  top: var(--top-4);
  left: var(--left-2);
}
.customization-09081-5-1 {
  top: var(--top-5);
  left: var(--left-1);
}
.customization-09081-5-2 {
  top: var(--top-5);
  left: var(--left-2);
}
.customization-09081-5-3 {
  top: var(--top-5);
  left: var(--left-3);
}
.customization-09081-6-1 {
  top: var(--top-6);
  left: var(--left-1);
}
.customization-09081-6-2 {
  top: var(--top-6);
  left: var(--left-2);
}
.customization-09081-6-3 {
  top: var(--top-6);
  left: var(--left-3);
}
.customization-09081-7-1 {
  top: var(--top-7);
  left: var(--left-1);
}
.customization-09081-7-2 {
  top: var(--top-7);
  left: var(--left-2);
}
.customization-09081-7-3 {
  top: var(--top-7);
  left: var(--left-3);
}

// Pocket Multi Tool
.tick-09099 {
  --top-0: 19.1%;
  --top-1: 30.0%;
  --top-2: 37.6%;
  --top-3: 45.0%;
  --top-4: 52.7%;
  --top-5: 60.1%;
  --top-6: 67.8%;
  --left-1: 8.4%;
  --left-2: 11.6%;
  --left-3: 15.2%;
  --left-4: 18.4%;
}

.customization-09099-0-1 {
  top: var(--top-0);
  left: var(--left-1);
}
.customization-09099-1-1 {
  top: var(--top-1);
  left: var(--left-1);
}
.customization-09099-2-1 {
  top: var(--top-2);
  left: var(--left-1);
}
.customization-09099-2-2 {
  top: var(--top-2);
  left: var(--left-2);
}
.customization-09099-3-1 {
  top: var(--top-3);
  left: var(--left-1);
}
.customization-09099-3-2 {
  top: var(--top-3);
  left: var(--left-2);
}
.customization-09099-4-1 {
  top: var(--top-4);
  left: var(--left-1);
}
.customization-09099-4-2 {
  top: var(--top-4);
  left: var(--left-2);
}
.customization-09099-5-1 {
  top: var(--top-5);
  left: var(--left-1);
}
.customization-09099-5-2 {
  top: var(--top-5);
  left: var(--left-2);
}
.customization-09099-5-3 {
  top: var(--top-5);
  left: var(--left-3);
}
.customization-09099-6-1 {
  top: var(--top-6);
  left: var(--left-1);
}
.customization-09099-6-2 {
  top: var(--top-6);
  left: var(--left-2);
}
.customization-09099-6-3 {
  top: var(--top-6);
  left: var(--left-3);
}
.customization-09099-6-4 {
  top: var(--top-6);
  left: var(--left-4);
}

// Hyperphysical Shotcaster
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
