<script setup lang="ts">
/* An investigator's round token, named, with their initials standing in when the
art has not loaded -- the same pair the prompt and the map use. */
import { computed } from 'vue'
import { img, isBroken, markBroken } from '@/assets'
import { useGame } from '@/game/context'

const props = defineProps<{ iid: string }>()
const ctx = useGame()
const src = computed(() => img(`investigators/${props.iid}/token.webp`))
const name = computed(() => ctx.invName(props.iid))
</script>

<template>
  <span v-if="isBroken(src)" class="chip inv" :title="name">{{ ctx.initials(iid) }}</span>
  <img v-else class="inv-tok" :src="src" :alt="name" :title="name" @error="markBroken(src)" />
</template>
