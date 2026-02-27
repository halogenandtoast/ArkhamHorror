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

const updateBeta = async (beta: boolean) => {
  await api.put<User>('settings', { beta })
  store.setCurrentUser()
}

const deleteAccount = async () => {
  await store.deleteAccount()
  router.push('/sign-in')
}

</script>

<template>
  <div>
    <SettingsForm v-if="currentUser" :user="currentUser" :updateBeta="updateBeta" :deleteAccount="deleteAccount" />
  </div>
</template>
