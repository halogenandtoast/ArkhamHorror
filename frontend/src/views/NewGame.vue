<script lang="ts" setup>
import { useRouter } from 'vue-router';
import NewCampaign from '@/arkham/views/NewCampaign.vue';

const router = useRouter()

/* A push rather than a step back through history, because this is a page of its
 * own now: it can be arrived at by URL, where there is nothing to go back to. */
const cancel = () => router.push({ path: '/' })
</script>

<template>
  <NewCampaign>
    <template #cancel>
      <button type="button" class="cancel-new-game-button" @click="cancel">
        <span>{{ $t('cancel') }}</span>
      </button>
    </template>
  </NewCampaign>
</template>

<style scoped>
.cancel-new-game-button {
  align-self: center;
  background-color: var(--survivor);
  border: 0;
  border-radius: 3px;
  color: white;
  font-size: 1em;
  font-weight: bolder;
  height: fit-content;
  outline: 0;
  padding: 10px 15px;
  text-transform: uppercase;
  transition: background-color 0.3s linear;
  width: fit-content;

  &:hover {
    background-color: var(--survivor-extra-dark);
  }
}
</style>
