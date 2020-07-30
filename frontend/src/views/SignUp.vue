<template>
  <form @submit.prevent="register(registration)">
    <div>
      <input
        v-model="registration.username"
        type="text"
        placeholder="Username"
      />
    </div>
    <div>
      <input
        v-model="registration.email"
        type="email"
        placeholder="Email"
      />
    </div>
    <div>
      <input
        v-model="registration.password"
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
import { Registration } from '../types';

@Component
export default class SignUp extends Vue {
  @Action register!: (registration: Registration) => void;

  registration: Registration = {
    username: '',
    email: '',
    password: '',
  };
}
</script>
