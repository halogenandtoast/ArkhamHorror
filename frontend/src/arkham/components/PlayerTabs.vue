<template lang="html">
  <div class="player-info">
    <ul class='tabs__header'>
      <li v-for='iid in playerOrder'
        :key='players[iid].contents.name.title'
        @click.exact='selectTab(iid)'
        @click.shift='selectTabExtended(iid)'
        :class='tabClass(iid)'
      >
        {{ players[iid].contents.name.title }}
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
import { defineComponent, ref, watchEffect, inject, Ref } from 'vue';
import { Game } from '@/arkham/types/Game';
import Tab from '@/arkham/components/Tab.vue';
import Player from '@/arkham/components/Player.vue';
import * as ArkhamGame from '@/arkham/types/Game';
import { Investigator } from '@/arkham/types/Investigator';

export default defineComponent({
  components: { Tab, Player },
  props: {
    game: { type: Object as () => Game, required: true },
    investigatorId: { type: String, required: true },
    players: { type: Object as () => Record<string, Investigator>, required: true },
    playerOrder: { type: Array as () => string[], required: true },
    activePlayerId: { type: String, required: true }
  },
  setup(props) {
    const selectedTab = ref(props.investigatorId)

    const solo = inject<Ref<boolean>>('solo')
    const switchInvestigator = inject<((i: string) => void)>('switchInvestigator')

    const hasChoices = (iid: string) => ArkhamGame.choices(props.game, iid).length > 0

    function tabClass(index: string) {
      const pid = props.players[index].contents.id
      return [
        {
          'tab--selected': index === selectedTab.value,
          'tab--active-player': pid == props.activePlayerId,
          'tab--has-actions': pid !== props.investigatorId && hasChoices(props.players[index].contents.id),
        },
        `tab--${props.players[index].contents.class}`,
      ]
    }

    function selectTab(i: string) {
      selectedTab.value = i
    }

    function selectTabExtended(i: string) {
      selectedTab.value = i
      if (solo?.value && props.investigatorId !== i && switchInvestigator) {
        switchInvestigator(i)
      }
    }


    watchEffect(() => selectedTab.value = props.investigatorId)

    return { hasChoices, selectedTab, selectTab, selectTabExtended, tabClass }
  }
})
</script>

<style lang="scss">
ul.tabs__header {
  display: block;
  list-style: none;
  padding: 0;
  margin: 0;
  user-select: none;
  padding-left: 5px;
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
  background-color: $guardian;
}

.tab--Seeker {
  background-color: $seeker;
}

.tab--Rogue {
  background-color: $rogue;
}

.tab--Mystic {
  background-color: $mystic;
}

.tab--Survivor {
  background-color: $survivor;
}

.tab--Neutral {
  background-color: $neutral;
}

.tab--active-player {
  &:before {
    font-weight: normal;
    font-family: "Arkham";
    content: "\0058";
    margin-right: 5px;
  }
}

.tab--has-actions {
  box-shadow: 0 0 5px #ff00ff;
}

.player-info {
  flex-grow: 1;
}
</style>
