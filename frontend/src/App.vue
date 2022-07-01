<template>
  <div id="app">
    <NavBar/>
    <router-view/>
  </div>
</template>

<script lang="ts">
import { defineComponent, onMounted } from 'vue'
import { useStore } from 'vuex'
import NavBar from '@/components/NavBar.vue'

export default defineComponent({
  components: { NavBar },
  setup() {
    const store = useStore()
    onMounted(async () => await store.dispatch('loadUserFromStorage'))
  }
})
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
