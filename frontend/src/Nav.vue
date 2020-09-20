<template>
  <div id="nav">
    <router-link to="/" class="home-link">Home</router-link>

    <span>
      <template v-if="currentUser">
        Welcome {{currentUser.username}}
        <a href="#" @click="logout">Logout</a>
      </template>
      <template v-else>
        <router-link to="/sign-in">Login</router-link> |
        <router-link to="/sign-up">Register</router-link>
      </template>
    </span>
  </div>
</template>

<script lang="ts">
import { Component, Vue } from 'vue-property-decorator';
import { Action, Getter } from 'vuex-class';
import { User } from './types';

@Component
export default class Nav extends Vue {
  @Getter currentUser!: User | void;
  @Action logout!: () => Promise<void>
}
</script>
