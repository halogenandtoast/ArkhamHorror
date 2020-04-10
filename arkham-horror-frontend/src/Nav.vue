<template>
  <div id="nav">
    <router-link to="/">Home</router-link> |
    <router-link to="/sign-in">Login</router-link> |
    <router-link to="/sign-up">Register</router-link>
  </div>
</template>

<script lang="ts">
import { Component, Vue } from 'vue-property-decorator';

@Component
export default class Nav extends Vue { }
</script>
