<script lang="ts" setup>
import { computed, ref } from 'vue';
import { useUserStore } from '@/stores/user';
import type { User } from '@/types';
import api from '@/api';
import SettingsForm from '@/components/SettingsForm.vue';

const store = useUserStore()
const currentUser = computed<User | null>(() => store.getCurrentUser)

const currentLanguage = localStorage.getItem('language') ?? 'en'

const language = ref(currentLanguage)

const updateBeta = async (beta: boolean) => {
  await api.put<User>('settings', { beta })
  store.setCurrentUser()
}

const updateLanguage = () => {
  localStorage.setItem('language', language.value)
}
</script>

<template>
  <div>
    <SettingsForm v-if="currentUser" :user="currentUser" :updateBeta="updateBeta" />
    <select v-model="language" @change="updateLanguage">
      <option value="en">English</option>
      <option value="it">Italian</option>
    </select>
  </div>
</template>
