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

const reversed = computed(() => {
  return hoveredElement.value?.classList.contains('Reversed') ?? false;
});

const overlayPosition = computed(() => {
  if (!hoveredElement.value) return { top: 0, left: 0 };
  return getPosition(hoveredElement.value);
});

const getRotated = (el: HTMLElement) => {
  var st = window.getComputedStyle(el, null);
  var tr = st.getPropertyValue("-webkit-transform") ||
           st.getPropertyValue("-moz-transform") ||
           st.getPropertyValue("-ms-transform") ||
           st.getPropertyValue("-o-transform") ||
           st.getPropertyValue("transform") ||
           "none"

  if (tr !== "none") {
    const [a, b] = tr.split('(')[1].split(')')[0].split(',');

    var angle = Math.round(Math.atan2(parseFloat(b), parseFloat(a)) * (180/Math.PI));

    return angle == 90
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
  const height = overlayWidth / ratio;

  // Calculate the top position, ensuring it doesn't go off the screen
  const top = rect.top + window.scrollY - 40;
  const bottom = top + height;
  const newTop = Math.max(0, bottom > window.innerHeight ? rect.bottom - height + window.scrollY - 40 : top);

  // Calculate the left position, adjusting for rotated cards
  const left = rect.left + window.scrollX + (rotated ? rect.height : rect.width) + 10;

  if (left + 300 >= window.innerWidth) {
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
  <div class="card-overlay" ref="cardOverlay" :style="{ top: overlayPosition.top + 'px', left: overlayPosition.left + 'px' }">
    <img v-if="card" :src="card" :class="{ reversed }" />
  </div>
</template>

<style lang="scss">
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
}


.reversed {
  transform: rotateZ(180deg);
}
</style>
