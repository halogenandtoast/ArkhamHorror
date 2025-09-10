<script setup lang="ts">
import api from '@/api';
import type { GameDetails } from '@/arkham/types/Game';
import GameRow from '@/arkham/components/GameRow.vue';

interface AdminData {
  currentUsers: number;
  activeUsers: number;
}

const request = await api.get<AdminData>('admin')
const data = request.data
const activeGames = data.activeGames.filter(g => g.error == undefined && g.gameState.tag !== 'IsOver')
const finishedGames = data.activeGames.filter(g => g.error == undefined && g.gameState.tag === 'IsOver')
</script>

<template>
  <div class="admin-page">
    <dl>
      <dt>Current Users</dt>
      <dd>{{data.currentUsers}}</dd>
      <dt>Active Users (last 14 days)</dt>
      <dd>{{data.activeUsers}}</dd>
    </dl>

    <section>
      <header><h2>{{$t('activeGames')}}</h2></header>
      <div v-if="activeGames.length === 0" class="box">
        <p>No active games.</p>
      </div>
      <GameRow
        v-for="game in activeGames"
        :key="game.id"
        :game="game"
        :admin="true"
        :deleteGame="() => deleteGameEvent(game)"
      />
    </section>

    <section>
      <header><h2 v-if="finishedGames.length > 0">{{$t('finishedGames')}}</h2></header>
      <GameRow
        v-for="game in finishedGames"
        :key="game.id"
        :game="game"
        :admin="true"
        :deleteGame="() => deleteGameEvent(game)"
      />

    </section>
  </div>
</template>

<style scoped>
.admin-page {
  padding: 20px;
  background-color: #f9f9f9;
  border-radius: 8px;
  margin: 20px;
  background: black;
  color: white;
}
</style>
