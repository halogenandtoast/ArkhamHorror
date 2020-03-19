<template>
  <form @submit.prevent="authenticate(credentials)">
    <div>
      <input
        v-model="credentials.email"
        type="email"
        placeholder="Email"
      />
    </div>
    <div>
      <input
        v-model="credentials.password"
        type="password"
        placeholder="Password"
      />
    </div>
    <div>
      <button>Submit</button>
    </div>
  </form>
</template>

<script lang="ts">
import { Vue, Component } from 'vue-property-decorator';
import { Action } from 'vuex-class';
import { Credentials } from '../types';

@Component
export default class SignIn extends Vue {
  @Action authenticate!: (credentials: Credentials) => void;

  credentials: Credentials = {
    email: '',
    password: '',
  };
}
</script>
