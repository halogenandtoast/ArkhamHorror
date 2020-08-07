<template lang="html">
  <div>
    <ul class='tabs__header'>
      <li v-for='(tab, index) in tabs'
        :key='tab.title'
        @click='selectTab(index)'
        :class='{"tab__selected": (index == selectedIndex), "tab__active-player": tab.activePlayer}'
      >
        {{ tab.title }}
      </li>
    </ul>
    <slot></slot>
  </div>
</template>

<script lang="ts">
import { Vue, Component } from 'vue-property-decorator';
import Tab from '@/arkham/components/Tab.vue';

@Component
export default class PlayerTabs extends Vue {
  selectedIndex = 0
  tabs: Tab[] = []

  created() {
    this.tabs = this.$children as Tab[];
  }

  mounted() {
    this.selectTab(0);
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
  background: #99CCFF;
  padding: 5px 10px;
  border-radius: 5px;
}

ul.tabs__header > li.tab__selected {
  background: #336699;
  font-weight: bold;
}

.tab__active-player {
  &:before {
    font-weight: normal;
    font-family: "Arkham";
    content: "\0058";
    margin-right: 5px;
  }
}
</style>
