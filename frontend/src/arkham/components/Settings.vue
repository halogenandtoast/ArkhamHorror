<script lang="ts" setup>
import { ref, computed, watch } from 'vue';
import { type Game } from '@/arkham/types/Game'
import { useDebug } from '@/arkham/debug'

const props = defineProps<{
  game: Game
  playerId: string
  closeSettings: () => void
}>()

const debug = useDebug()
const investigator = computed(() => {
  return Object.values(props.game.investigators).find(i => i.playerId === props.playerId)
})

const skipReactions = ref(investigator.value.settings.globalSettings.ignoreUnrelatedSkillTestReactions)

watch(() => skipReactions.value, (value) => {
  if (investigator.value) {
    debug.send(props.game.id,
      ({ tag: 'UpdateGlobalSetting'
       , contents: [investigator.value.id, {tag: "SetIgnoreUnrelatedSkillTestReactions", contents: value}]
       }
      )
    )
  }
})

</script>
<template>
  <div class="settings">
    <div class="options box">
      <h2 class="title">Settings for {{investigator.name.title}}</h2>
      <label>Skip reactions during skill tests that don't affect the outcome</label>
      <input type="checkbox" v-model="skipReactions" />
    </div>
    <div>
      <button @click="closeSettings">Close</button>
    </div>
  </div>
</template>

<style scoped lang="scss">
.box {
  margin: 10px;
}

label {
  margin-right: 10px;
}

button {
  width: 100%;
}
</style>
