<template lang="html">
  <div class="player-info">
    <ul class='tabs__header'>
      <li v-for='(player, index) in players'
        :key='player.contents.name.title'
        @click='selectTab(index)'
        :class='tabClass(index)'
      >
        {{ player.contents.name.title }}
      </li>
    </ul>
    <Tab
      v-for="(player, index) in players"
      :key="index"
      :index="index"
      :selectedTab="selectedTab"
      :playerClass="player.contents.class"
      :title="player.contents.name.title"
      :investigatorId="index"
      :activePlayer="player.contents.id == activePlayerId"
    >
      <Player
        :game="game"
        :investigatorId="investigatorId"
        :player="player"
        @choose="$emit('choose', $event)"
      />
    </Tab>
  </div>
</template>

<script lang="ts">
import { defineComponent, ref } from 'vue';
import { Game } from '@/arkham/types/Game';
import Tab from '@/arkham/components/Tab.vue';
import Player from '@/arkham/components/Player.vue';
import { Investigator } from '@/arkham/types/Investigator';

export default defineComponent({
  components: { Tab, Player },
  props: {
    game: { type: Object as () => Game, required: true },
    investigatorId: { type: String, required: true },
    players: { type: Object as () => Record<string, Investigator>, required: true },
    activePlayerId: { type: String, required: true }
  },
  setup(props) {
    const selectedTab = ref(props.investigatorId)

    function tabClass(index: string) {
      return [
        {
          'tab--selected': index === selectedTab.value,
          'tab--active-player': props.players[index].contents.id == props.activePlayerId,
        },
        `tab--${props.players[index].contents.class}`,
      ]
    }

    function selectTab(i: string) {
      selectedTab.value = i
    }

    return { selectedTab, selectTab, tabClass }
  }
})
</script>

<style lang="scss">
ul.tabs__header {
  display: block;
  list-style: none;
  padding: 0;
  margin: 0;
}

ul.tabs__header > li {
  margin: 0;
  display: inline-block;
  margin-right: 5px;
  cursor: pointer;
  color: white;
  padding: 5px 10px;
  filter: contrast(50%);
  border-radius: 5px 5px 0 0;
}

ul.tabs__header > li.tab--selected {
  font-weight: bold;
  filter: contrast(100%);
}

.tab--Guardian {
  background-color: #3A6BA0;
}

.tab--Seeker {
  background-color: #B4793B;
}

.tab--Rogue {
  background-color: #265035;
}

.tab--Mystic {
  background-color: #443D72;
}

.tab--Survivor {
  background-color: #6B2F2E;
}

.tab--Neutral {
  background-color: #7B7A72;
}

.tab--active-player {
  &:before {
    font-weight: normal;
    font-family: "Arkham";
    content: "\0058";
    margin-right: 5px;
  }
}

.player-info {
  flex-grow: 1;
}
</style>
