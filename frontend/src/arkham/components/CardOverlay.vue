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

</style>
