<template>
  <div v-if="maintenanceActive" class="error-message maintenance container">
    <header><h2 class="title">{{ $t('maintenance.title') }}</h2></header>
    <section class="box">
      <p>{{ maintenanceMessage || $t('maintenance.body') }}</p>
      <p class="reassure">{{ $t('maintenance.reassure') }}</p>
      <button @click="retry">{{ $t('maintenance.retry') }}</button>
    </section>
  </div>
  <div v-else-if="!avifSupported" class="error-message container">
    <header><h2 class="title">Please update your browser</h2></header>
    <section class="box">
      <p>Your browser does not support AVIF images. Please update your browser or switch to a different one.</p>
    </section>
  </div>
  <template v-else>
    <NavBar/>
    <main class="router-container">
      <Suspense @resolve="hideAppLoading">
        <router-view v-slot="{ Component }">
          <transition name="fade">
            <component :is="Component" :key="routeKey" />
          </transition>
        </router-view>
        <template #fallback>
          Loading...
        </template>
      </Suspense>
    </main>
    <ModalsContainer />
  </template>
  <Toasts />
  <footer><a href="https://www.fantasyflightgames.com/en/products/arkham-horror-the-card-game/" rel="noreferrer" target="_blank" tabindex="-1">Arkham Horror: The Card Game™</a> and all related content © <a href="https://www.fantasyflightgames.com" rel="noreferrer" target="_blank" tabindex="-1">Fantasy Flight Games (FFG)</a>. This site is not produced, endorsed by or affiliated with FFG. <router-link to="/about">{{$t('nav.about')}}.</router-link></footer>
</template>

<script lang="ts" setup>
import { ModalsContainer } from 'vue-final-modal'
import { computed, ref, onMounted } from 'vue'
import { useRoute } from 'vue-router'
import { useSiteSettingsStore } from '@/stores/site_settings'
import { maintenanceActive, maintenanceMessage } from '@/maintenance'
import { checkImageExists } from '@/arkham/helpers'
import { hideAppLoading } from '@/appLoading'
import { primeWatermark } from '@/arkham/unwatermark'
import NavBar from '@/components/NavBar.vue'
import Toasts from '@/components/Toasts.vue'

const settingsStore = useSiteSettingsStore()

/* Vue Router reuses one component instance when only a route param changes, so
 * opening a second game in the same tab kept the first game's `Game.vue` alive
 * -- including a full-screen reveal and the `uiLock` it holds. The new game
 * then came up behind an overlay belonging to the old one, which is
 * indistinguishable from a stuck board. Keying on the game forces a remount;
 * routes without one keep a stable key so ordinary navigation is unaffected. */
const route = useRoute()
const routeKey = computed(() =>
  route.params.gameId ? `${String(route.name)}:${route.params.gameId}` : String(route.name)
)

/* A full reload rather than refetching: the window may have ended a build
 * later, and a client that sat through it holding stale chunks and a dead
 * socket is better off starting clean. */
function retry() {
  window.location.reload()
}

onMounted(async () => {
  /* 13KB, needed before any card can be un-watermarked; fetching it now keeps
   * it off the critical path of the first card someone looks at. */
  primeWatermark()
  await settingsStore.init()
  avifSupported.value = await checkAvifSupport();
  await checkImageExists()
  /* Maintenance and the unsupported-browser notice render INSTEAD of the router
   * view, so Suspense never resolves and would leave the splash sitting on top
   * of the very message the player needs to read. */
  if (maintenanceActive.value || !avifSupported.value) hideAppLoading()
})
const avifSupported = ref(true);
const checkAvifSupport = (): Promise<boolean> => {
  return new Promise((resolve) => {
    const image = new Image();
    image.onerror = () => resolve(false)
    image.onload = () => resolve(true)
    image.src =
      "data:image/avif;base64,AAAAIGZ0eXBhdmlmAAAAAGF2aWZtaWYxbWlhZk1BMUIAAADybWV0YQAAAAAAAAAoaGRscgAAAAAAAAAAcGljdAAAAAAAAAAAAAAAAGxpYmF2aWYAAAAADnBpdG0AAAAAAAEAAAAeaWxvYwAAAABEAAABAAEAAAABAAABGgAAAB0AAAAoaWluZgAAAAAAAQAAABppbmZlAgAAAAABAABhdjAxQ29sb3IAAAAAamlwcnAAAABLaXBjbwAAABRpc3BlAAAAAAAAAAIAAAACAAAAEHBpeGkAAAAAAwgICAAAAAxhdjFDgQ0MAAAAABNjb2xybmNseAACAAIAAYAAAAAXaXBtYQAAAAAAAAABAAEEAQKDBAAAACVtZGF0EgAKCBgANogQEAwgMg8f8D///8WfhwB8+ErK42A=";
  })
};
</script>

