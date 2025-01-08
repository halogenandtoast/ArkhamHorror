<template>
  <div v-if="!avifSupported" class="error-message container">
    <header><h2 class="title">Please update your browser</h2></header>
    <section class="box">
      <p>Your browser does not support AVIF images. Please update your browser or switch to a different one.</p>
    </section>
  </div>
  <template v-else>
    <NavBar/>
    <Suspense>
      <router-view v-slot="{ Component }" class="router-container">
        <transition name="fade">
          <component :is="Component" />
        </transition>
      </router-view>
      <template #fallback>
        Loading...
      </template>
    </Suspense>
    <ModalsContainer />
  </template>
  <footer><a href="https://www.fantasyflightgames.com/en/products/arkham-horror-the-card-game/" rel="noreferrer" target="_blank" tabindex="-1">Arkham Horror: The Card Game™</a> and all related content © <a href="https://www.fantasyflightgames.com" rel="noreferrer" target="_blank" tabindex="-1">Fantasy Flight Games (FFG)</a>. This site is not produced, endorsed by or affiliated with FFG. <router-link to="/about">{{$t('about')}}.</router-link></footer>
</template>

<script lang="ts" setup>
import { ModalsContainer } from 'vue-final-modal'
import { ref, onMounted } from 'vue'
import { useUserStore } from '@/stores/user'
import NavBar from '@/components/NavBar.vue'
import 'floating-vue/dist/style.css'

const store = useUserStore()
onMounted(async () => {
  await store.loadUserFromStorage()
  avifSupported.value = await checkAvifSupport();
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

<style lang="scss">
html {
  color-scheme: dark;
  interpolate-size: allow-keywords;
}
* {
  padding: 0;
  margin: 0;
  font: inherit;
  min-width: 0;
}
*, *::before, *::after{
  box-sizing: border-box;
}

b, strong {
  font-weight: bold;
}

img, svg {
  max-width: 100%;
}

button {
  padding: 0px 5px;
  font-family: Arial;
  font-size: 13.3px;
}

@font-face {
  font-family: "Arkham";
  src: url("/fonts/arkham.ttf");
}

@font-face {
  font-family: "AboutDead";
  src: url("/fonts/AboutDead.ttf");
}

.about-dead {
  font-family: "AboutDead";
  text-align: center;
  font-size: 1.5em;
}

@font-face {
  font-family: "Noto Sans";
  src: url("/fonts/NotoSans.ttf");
}

@font-face {
  font-family: "ArkhamIcons";
  src: url("/fonts/arkhamicons.ttf");
}

@font-face {
  font-family: "ArkhamSlim";
  src: url("/fonts/arkhamslim.ttf");
}

@font-face {
  font-family: "Teutonic";
  src: url("/fonts/teutonic.ttf");
}

@font-face {
  font-family: "ArkhamCursive";
  src: url("/fonts/AquilineTwo.ttf");
}

@font-face {
  font-family: "ArkhamFlavor";
  src: url("/fonts/ArnoPro-ItalicCaption.ttf");
}

body {
  margin: 0;
  padding: 0;
  color: #222;
  min-height: 100vh;
  background: var(--background);
}

#app {
  font-family: "Noto Sans", Avenir, Helvetica, Arial, sans-serif;
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
  width: 100%;
  height: 100vh;
  display: flex;
  flex-direction: column;
}

.neutral-icon {
  &:before {
    font-family: "ArkhamSlim";
    content: "\0046";
  }
}

.guardian-icon {
  &:before {
    font-family: "Arkham";
    content: "\0051";
  }
}

.seeker-icon {
  &:before {
    font-family: "Arkham";
    content: "\0045";
  }
}

.rogue-icon {
  &:before {
    font-family: "Arkham";
    content: "\0054";
  }
}

.mystic-icon {
  &:before {
    font-family: "Arkham";
    content: "\0057";
  }
}

.survivor-icon {
  &:before {
    font-family: "Arkham";
    content: "\0052";
  }
}

.elder-sign {
  &:before {
    font-weight: normal;
    font-size: 1.3em;
    font-family: "Arkham";
    content: "\0058";
  }
}

.auto-fail {
  &:before {
    font-weight: normal;
    font-size: 1.3em;
    font-family: "Arkham";
    content: "\005A";
  }
}

.action-icon {
  &:before {
    font-family: "arkham";
    content: "\0049";
    margin-right: 5px;
  }
}

.willpower-icon {
  &:before {
    font-family: "Arkham";
    content: "\0041";
    color: inherit;
  }
}

.intellect-icon {
  &:before {
    font-family: "Arkham";
    content: "\0046";
  }
}

.combat-icon {
  &:before {
    font-family: "Arkham";
    content: "\0044";
  }
}

.agility-icon {
  &:before {
    font-family: "Arkham";
    content: "\0053";
  }
}

.skull-icon {
  &:before {
    font-family: "ArkhamIcons";
    content: "\e910";
  }
}

.cultist-icon {
  &:before {
    font-family: "ArkhamIcons";
    content: "\e911";
  }
}

.tablet-icon {
  &:before {
    font-family: "ArkhamIcons";
    content: "\e912";
  }
}

.elder-thing-icon {
  &:before {
    font-family: "ArkhamIcons";
    content: "\e913";
  }
}

.curse-icon {
  &:before {
    font-family: "ArkhamIcons";
    content: "\e929";
  }
}

.bless-icon {
  &:before {
    font-family: "ArkhamIcons";
    content: "\e92a";
  }
}

.frost-icon {
  &:before {
    font-family: "ArkhamSlim";
    content: "\0062";
  }
}

.wild-icon {
  &:before {
    font-family: "Arkham";
    content: "\0047";
  }
}

.fast-icon {
  &:before {
    font-family: "arkham";
    content: "\0075";
    margin-right: 5px;
  }
}

.reaction-icon {
  &:before {
    font-family: "arkham";
    content: "\0079";
    margin-right: 5px;
  }
}

.per-player{
  &:before {
    font-family: "ArkhamIcons";
    content: "\E915";
  }
}

:root {
  transition-behavior: allow-discrete;
  --willpower: #2c7fc0;
  --intellect: #7c3c85;
  --combat: #ae4236;
  --agility: #14854d;

  --willpower-light: #C5DEF2;
  --intellect-light: #EAD5EC;
  --combat-light: #F1D3D0;
  --agility-light: #8CEEBD;

  --wild: #8a7d5a;
  --guardian: #5cb4fd;
  --mystic: #ba81f2;
  --rogue: #48b14f;
  --seeker: #efa345;
  --survivor: #ee4a53;
  --multiclass: #e4d083;
  --mythos: #d8dee9;
  --neutral: #d8dee9;
  --guardian-dark: #1072c2;
  --guardian-extra-dark: #0c5693;
  --seeker-dark: #db7c07;
  --seeker-extra-dark: #aa6005;
  --rogue-dark: #219428;
  --rogue-extra-dark: #186a1d;
  --mystic-dark: #7554ab;
  --mystic-extra-dark: #5e4389;
  --survivor-dark: #cc3038;
  --survivor-extra-dark: #a3262d;
  --neutral-dark: #333;
  --neutral-extra-dark: #222;
  --multiclass-dark: #a38c46;
  --mythos-dark: #434c5e;
  --taboo: #9869f5;
  --health: #ae4236;
  --sanity: #2c7fc0;

  --blessed: #6a5720;
  --cursed: #270F31;
  --frost: #39394C;

  --delete: #c13131;
  --background: #2e3440;
  --background-dark: #242831;
  --background-mid: #515a68;
  --background-light: #c9ced8;
  --box-background: #353b49;
  --box-border: #434c5e;

  --title: #cecece;
  --spooky-green: #879C5A;
  --spooky-green-dark: #3A5144;

  --button-1: #6E8640;
  --button-1-highlight: #5a6e34;

  --button-2: #532e61;
  --button-2-highlight: #4d2b61;

  --card-width: 60px;
  --card-aspect: 0.705;
  --card-sideways-aspect: 1.41844;
  --card-tarot-aspect: 0.571429;

  --card-shadow: 1px 1px 6px rgba(0, 0, 0, 0.45);

  --select: #ff00ff;
  --select-dark: #d400d4;
  --select-dark-20: #cc00cc;
  --select-dark-30: #b200b2;

  --bullet-red: #391714;

  @media (max-width: 800px) {
    --card-width: 40px;
  }
}

h2.title {
  color: var(--title);
  font-size: 2em;
  text-transform: uppercase;
  font-family: Teutonic;
}

.box {
  background-color: var(--box-background);
  border: 1px solid var(--box-border);
  color: var(--title);
  padding: 10px;
  border-radius: 5px;
}

.page-container {
  height: 100%;
  overflow: auto;
  width: 100%;
  margin-block: 10px;
}

.page-content {
  width: 70vw;
  margin: 0 auto;
  padding-bottom: 10px;
}

.fade-leave-active,
.fade-enter-active {
  transition: opacity 0.3s;
}

.fade-enter-active {
  transition-delay: 0.3s;
  position: absolute;
  inset: 0;
}

.fade-leave-to,
.fade-enter-from {
  opacity: 0;
}

.router-container {
  position: relative;
}

footer {
  margin: 0 auto;
  font-size: 0.5em;
  color: var(--title);
  position: fixed;
  bottom: 0;
  width: 100%;
  text-align: center;
  z-index: 100;
}

.column {
  display: flex;
  flex-direction: column;
  gap: 10px;
}

.row {
  display: flex;
  flex-direction: row;
  gap: 10px;
}

.card {
  box-shadow: var(--card-shadow);
}

:nth-child(1) {
  --index: 1;
}
:nth-child(2) {
  --index: 2;
}
:nth-child(3) {
  --index: 3;
}
:nth-child(4) {
  --index: 4;
}

:nth-last-child(1) {
  --rev-index: 1;
}
:nth-last-child(2) {
  --rev-index: 2;
}

.buttons {
  display: flex;
  flex-direction: row;
  gap: 10px;
}

.error-message {
  margin: 50px;
  header {
    text-align: center;
    margin-bottom: 10px;
  }
}
</style>
