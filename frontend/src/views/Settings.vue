<script lang="ts" setup>
import { computed } from 'vue';
import { useUserStore } from '@/stores/user';
import type { User } from '@/types';
import api from '@/api';
import SettingsForm from '@/components/SettingsForm.vue';

const store = useUserStore()
const currentUser = computed<User | null>(() => store.getCurrentUser)

const updateBeta = async (beta: boolean) => {
  await api.put<User>('settings', { beta })
  store.setCurrentUser()
}

</script>

<template>
  <div>
    <SettingsForm v-if="currentUser" :user="currentUser" :updateBeta="updateBeta" />
  </div>
</template>
