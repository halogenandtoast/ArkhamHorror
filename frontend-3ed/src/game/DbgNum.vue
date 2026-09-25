<script setup lang="ts">
import { useGame } from '@/game/context'

const props = defineProps<{ tag: string; iid: string | null; value: number; title: string; min?: number }>()
const ctx = useGame()
function change(e: Event) {
  const v = +(e.target as HTMLInputElement).value
  void ctx.debugAction(props.tag, props.iid === null ? v : [props.iid, v])
}
</script>

<template>
  <input class="dbg-num" type="number" :min="min" :value="value" :title="title" @change="change" />
</template>
