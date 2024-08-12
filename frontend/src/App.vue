<template>
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

<script lang="ts" setup>
import { ModalsContainer } from 'vue-final-modal'
import { onMounted } from 'vue'
import { useUserStore } from '@/stores/user'
import NavBar from '@/components/NavBar.vue'
import 'floating-vue/dist/style.css'

const store = useUserStore()
onMounted(async () => await store.loadUserFromStorage())

</script>

<style lang="scss">
@font-face {
  font-family: "Arkham";
  src: url("/fonts/arkham.ttf");
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


body {
  background: $background;
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

:root {
  --willpower: #2c7fc0;
  --intellect: #7c3c85;
  --combat: #ae4236;
  --agility: #14854d;
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


  --delete: #c13131;
  --background: #2e3440;
  --background-dark: #242831;
  --background-mid: #515a68;
  --background-light: #c9ced8;
  --box-background: #353b49;
  --box-border: #434c5e;

  --title: #cecece;
  --spooky-green: #879C5A;

  --button-1: #6E8640;
  --button-1-highlight: #5a6e34;

  --button-2: #532e61;
  --button-2-highlight: #4d2b61;

  --card-width: 60px;

  @media (max-width: 800px) {
    --card-width: 40px;
  }

  color-scheme: dark;
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
  box-sizing: border-box;
}

.page-container {
  width: 70vw;
  margin: 0 auto;
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
</style>
