<template lang="html">
  <div>
    <ul class='tabs__header'>
      <li v-for='(tab, index) in tabs'
        :key='tab.title'
        @click='selectTab(index)'
        :class='tabClass(tab, index)'
      >
        {{ tab.title }}
      </li>
    </ul>
    <slot></slot>
  </div>
</template>

<script lang="ts">
import { Vue, Prop, Component } from 'vue-property-decorator';
import Tab from '@/arkham/components/Tab.vue';

@Component
export default class PlayerTabs extends Vue {
  @Prop(String) readonly investigatorId!: string

  selectedIndex = 0
  tabs: Tab[] = []

  created() {
    this.tabs = this.$children as Tab[];
  }

  tabClass(tab: Tab, index: number) {
    return [
      {
        'tab--selected': index === this.selectedIndex,
        'tab--active-player': tab.activePlayer,
      },
      `tab--${tab.playerClass}`,
    ];
  }

  mounted() {
    const idx = this
      .tabs
      .findIndex((tab) => tab.$props.investigatorId === this.investigatorId);
    this.selectTab(idx === -1 ? 0 : idx);
  }

  selectTab(i: number) {
    this.selectedIndex = i;

    this.tabs.forEach((tab, index) => {
      tab.isActive = (index === i) // eslint-disable-line
    });
  }
}
</script>

<style lang="scss">
ul.tabs__header {
  display: block;
  list-style: none;
  padding: 0;
}

ul.tabs__header > li {
  margin: 0;
  display: inline-block;
  margin-right: 5px;
  cursor: pointer;
  color: white;
  padding: 5px 10px;
  filter: contrast(50%);
  border-radius: 5px;
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
</style>
