<template>
  <div id="app">
    <NavBar/>
    <Suspense>
      <router-view/>
      <template #fallback>
        Loading...
      </template>
    </Suspense>
  </div>
</template>

<script lang="ts" setup>
import { onMounted } from 'vue'
import { useUserStore } from '@/stores/user'
import NavBar from '@/components/NavBar.vue'

const store = useUserStore()
onMounted(async () => await store.loadUserFromStorage())

</script>

<style lang="scss">
@font-face {
  font-family: "Arkham";
  src: url("../public/fonts/arkham.ttf");
}

@font-face {
  font-family: "Teutonic";
  src: url("../public/fonts/teutonic.ttf");
}

body {
  margin: 0;
  padding: 0;
}

#app {
  font-family: Avenir, Helvetica, Arial, sans-serif;
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
  width: 100%;
}


body {
  background: #26283D;
}
</style>
