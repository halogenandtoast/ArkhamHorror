<template>
  <div id="app">
    <Nav />
    <router-view/>
  </div>
</template>

<script lang="ts">
import { Component, Vue } from 'vue-property-decorator';
import { Action, Getter } from 'vuex-class';
import { User } from './types';
import Nav from './Nav.vue';

@Component({
  components: { Nav },
})
export default class App extends Vue {
  @Getter currentUser!: User | undefined;
  @Action loadUserFromStorage!: () => void;

  async mounted() {
    await this.loadUserFromStorage();
  }
}
</script>

<style lang="scss">
body {
  margin: 0;
  padding: 0;
}

#app {
  font-family: Avenir, Helvetica, Arial, sans-serif;
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#nav {
  height: 40px;
  box-sizing: border-box;
  padding: 10px 15px;
  text-align: right;
  background: #EEE;

  a {
    font-weight: bold;
    color: #2c3e50;
  }
}
</style>
