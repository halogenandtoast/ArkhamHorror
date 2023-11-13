<template>
  <NavBar/>
  <Suspense>
    <router-view/>
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
}

#app {
  font-family: Avenir, Helvetica, Arial, sans-serif;
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
  width: 100%;
  height: 100vh;
  display: flex;
  flex-direction: column;
}


body {
  background: #26283D;
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
</style>
