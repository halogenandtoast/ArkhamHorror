import { ref, onMounted, onUnmounted } from 'vue';

export function IsMobile() {
  const isMobile = ref(false);

  function updateIsMobile() {
    isMobile.value = window.innerWidth <= 800;
  }

  onMounted(() => {
    updateIsMobile();
    window.addEventListener('resize', updateIsMobile);
  });

  onUnmounted(() => {
    window.removeEventListener('resize', updateIsMobile);
  });

  return { isMobile };
}