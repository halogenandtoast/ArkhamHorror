<template>
  <div class="home">
    <div v-if="currentUser">
      <router-link to="/new-campaign">New Campaign</router-link>
    </div>
  </div>
</template>

<script lang="ts">
import { Component, Vue } from 'vue-property-decorator';
import { Getter } from 'vuex-class';
import { User } from '../types';

@Component
export default class Home extends Vue {
  @Getter currentUser!: User | undefined;
}
</script>
