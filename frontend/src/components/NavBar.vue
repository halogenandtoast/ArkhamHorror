<script lang="ts" setup>
import { computed, ref } from 'vue'
import { useUserStore } from '@/stores/user'
import { useRouter } from 'vue-router'
import type { User } from '@/types'
import { OnClickOutside } from '@vueuse/components'

const expanded = ref(false);
const router = useRouter()
const store = useUserStore()
const currentUser = computed<User | null>(() => store.getCurrentUser)

async function logout() {
  await store.logout()
  router.push({ path: '/' })
}
</script>

<template>
  <div id="nav">
    <span class="main-links">
      <router-link to="/" class="home-link">{{$t('home')}}</router-link>{{' '}}
      <router-link v-if="currentUser" to="/decks">{{$t('myDecks')}}</router-link>
      <router-link v-if="currentUser" to="/cards">{{$t('cards')}}</router-link>
      <router-link v-if="currentUser" to="/about">{{$t('About')}}</router-link>
    </span>

    <OnClickOutside @trigger="expanded = false">
      <span class="user-links"> 
        <template v-if="currentUser">
          <label for="dropdown-toggle" @click="expanded = !expanded">
            <span>{{currentUser.username}}</span>
            <font-awesome-icon icon="angle-down" class="user-links--dropdown-icon" />
          </label>
          <div v-if="expanded" class="user-links--dropdown">
            <router-link @click="expanded = false" to="/settings">{{$t('settings')}}</router-link>{{' '}}
            <a href="#" @click="logout">Logout</a>
          </div>
        </template>
        <template v-else>
          <router-link to="/sign-in">Login</router-link>{{' '}}
          <router-link to="/sign-up">Register</router-link>
        </template>
      </span>
    </OnClickOutside>
  </div>
</template>

<style lang="scss" scoped>
#nav {
  background-color: var(--background-dark);
  color: #f2f2f2;
  height: 40px;
  box-sizing: border-box;
  display: flex;
  align-items: center;
  padding-right: 10px;
  flex-shrink: 0;

  a {
    font-weight: bold;
    color: $spooky-green;
    text-decoration: none;
    &:hover {
      color: #6E8640;
    }
  }
}

.main-links {
  flex-grow: 1;
  a {
    margin-left: 10px;
  }
}

.gravatar {
  height: 30px;
  margin-right: 10px;
  border-radius: 5px;
}

input[type=checkbox] {
  display: none;
}

input[type=checkbox]:checked ~ .user-links .user-links--dropdown {
  display: block;
}

input[type=checkbox]:checked ~ .user-links {
  border-radius: 5px 5px 0 0;
}

.user-links {
  display: flex;
  align-items: center;
  position: relative;
  padding-right: 10px;
  border-radius: 5px;
  z-index: 100000;

  label {
    user-select: none;
  }

  a {
    margin-left: 10px;
  }
}

.user-links--dropdown {
  position: absolute;
  top: 100%;
  width: 100%;
  text-align: right;
  background: var(--background-dark);
  padding: 10px;
  box-sizing: border-box;
  border-radius: 0 0 5px 5px;
}

.user-links--dropdown-icon {
  margin-left: 10px;
  cursor: pointer;
}

label {
  display: flex;
  align-items: center;
}
</style>
