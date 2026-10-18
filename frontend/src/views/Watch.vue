<script lang="ts" setup>
import { ref, Ref } from 'vue';
import { fetchSpectatableGames } from '@/arkham/api';
import type { GameDetails } from '@/arkham/types/Game';
import GameRow from '@/arkham/components/GameRow.vue';

/* Other people's games, whose hosts have put them on the watch list. Nothing
 * here is yours, and the only thing to do with one is open it read-only, which
 * is why this is a page of its own rather than a tab of Your Games. */
const games: Ref<GameDetails[]> = ref([])
const loading = ref(true)

/* A quiet failure: the page says nobody is hosting, which is what a failed
 * fetch looks like from the outside anyway. */
fetchSpectatableGames()
  .then((result) => (games.value = result))
  .catch(() => (games.value = []))
  .finally(() => (loading.value = false))
</script>

<template>
  <div class="page-container">
    <div class="watch page-content">
      <div class="container">
        <section>
          <header>
            <div class="title-group">
              <h2>{{ $t('watch.title') }}</h2>
            </div>
            <div class="header-actions">
              <RouterLink class="secondary-cta" to="/">{{ $t('watch.back') }}</RouterLink>
            </div>
          </header>
          <p class="watch-blurb">{{ $t('watch.blurb') }}</p>
          <div v-if="loading" class="box">
            <p>{{ $t('watch.loading') }}</p>
          </div>
          <div v-else-if="games.length === 0" class="box">
            <p>{{ $t('watch.empty') }}</p>
          </div>
          <GameRow v-for="game in games" :key="game.id" :game="game" spectate />
        </section>
      </div>
    </div>
  </div>
</template>

<style scoped>
/* The list grows as games open up and close, so hold the gutter open rather
   than letting the column shift sideways when it does. */
.page-container {
  scrollbar-gutter: stable;
}

.watch {
  max-width: 98vw;
  min-width: 60vw;
  margin: 0 auto;
  @media (max-width: 768px) {
    min-width: unset;
    width: 100%;
    padding: 20px 12px 10px;
    box-sizing: border-box;
  }
}

header {
  display: flex;
  margin-bottom: 10px;
  align-items: center;
  gap: 12px;
  flex-wrap: wrap;
}

.title-group {
  display: flex;
  flex: 1;
  align-items: center;
  gap: 12px;
}

h2 {
  color: var(--title);
  font-size: 2em;
  text-transform: uppercase;
  font-family: teutonic, sans-serif;
  margin: 0;
  @media (max-width: 768px) {
    font-size: 1.5em;
  }
}

.header-actions {
  display: flex;
  align-items: center;
  gap: 8px;
  justify-content: flex-end;
}

.secondary-cta {
  align-self: center;
  background: transparent;
  border: 1px solid var(--box-border);
  border-radius: 3px;
  color: var(--title);
  cursor: pointer;
  font-size: 0.85em;
  font-weight: 700;
  opacity: 0.8;
  outline: 0;
  padding: 8px 12px;
  text-decoration: none;
  text-transform: uppercase;

  &:hover {
    background: rgba(255, 255, 255, 0.06);
    opacity: 1;
  }
}

/* One line of context, because nothing else on the site shows games that are
   not yours. */
.watch-blurb {
  color: var(--title);
  opacity: 0.7;
  margin: 0 0 10px;
}

p {
  margin: 0;
  padding: 0;
}

.box {
  background-color: var(--box-background);
  border: 1px solid var(--box-border);
  color: var(--title);
  padding: 10px;
  border-radius: 5px;
}
</style>
