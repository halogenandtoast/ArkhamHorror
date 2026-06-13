<template>
  <div v-if="!avifSupported" class="error-message container">
    <header><h2 class="title">Please update your browser</h2></header>
    <section class="box">
      <p>Your browser does not support AVIF images. Please update your browser or switch to a different one.</p>
    </section>
  </div>
  <template v-else>
    <NavBar/>
    <main class="router-container">
      <Suspense>
        <router-view v-slot="{ Component }">
          <transition name="fade">
            <component :is="Component" />
          </transition>
        </router-view>
        <template #fallback>
          Loading...
        </template>
      </Suspense>
    </main>
    <ModalsContainer />
  </template>
  <footer><a href="https://www.fantasyflightgames.com/en/products/arkham-horror-the-card-game/" rel="noreferrer" target="_blank" tabindex="-1">Arkham Horror: The Card Game™</a> and all related content © <a href="https://www.fantasyflightgames.com" rel="noreferrer" target="_blank" tabindex="-1">Fantasy Flight Games (FFG)</a>. This site is not produced, endorsed by or affiliated with FFG. <router-link to="/about">{{$t('nav.about')}}.</router-link></footer>
</template>

<script lang="ts" setup>
import { ModalsContainer } from 'vue-final-modal'
import { ref, onMounted } from 'vue'
import { useSiteSettingsStore } from '@/stores/site_settings'
import { checkImageExists } from '@/arkham/helpers'
import NavBar from '@/components/NavBar.vue'

const settingsStore = useSiteSettingsStore()

onMounted(async () => {
  await settingsStore.init()
  avifSupported.value = await checkAvifSupport();
  await checkImageExists()
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

