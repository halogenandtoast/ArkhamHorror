<script lang="ts" setup>
import { useUserStore } from '@/stores/user';
import { storeToRefs } from 'pinia';
import type { User } from '@/types';
import api from '@/api';
import SettingsForm from '@/components/SettingsForm.vue';
import { useRouter } from 'vue-router';

const store = useUserStore()
const { currentUser } = storeToRefs(store)
const router = useRouter()

const updateSettings = async (settings: { beta: boolean; phaseTransitionNotifications: boolean }) => {
  await api.put<User>('settings', settings)
  await store.setCurrentUser()
}

const deleteAccount = async () => {
  await store.deleteAccount()
  router.push('/sign-in')
}

</script>

<template>
  <SettingsForm v-if="currentUser" :user="currentUser" :updateSettings="updateSettings" :deleteAccount="deleteAccount" />
</template>
