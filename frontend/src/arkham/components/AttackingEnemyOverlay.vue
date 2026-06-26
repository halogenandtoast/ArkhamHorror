<script lang="ts" setup>
import { computed } from 'vue'
import { Game } from '@/arkham/types/Game'
import { cardImage } from '@/arkham/cardImages'

const props = defineProps<{
  game: Game
  targetId: string
}>()

const attackingEnemy = computed(() => {
  const entry = props.game.enemyAttackTargets.find((e) => e.target.contents === props.targetId)
  if (!entry) return null
  return props.game.enemies[entry.enemy] ?? null
})

const image = computed(() => {
  const enemy = attackingEnemy.value
  if (!enemy) return null
  return cardImage(enemy.cardCode, enemy.flipped ? 'b' : '')
})
</script>

<template>
  <img v-if="image" :src="image" class="attacking-enemy-overlay" alt="" />
</template>

<style scoped lang="scss">
.attacking-enemy-overlay {
  position: absolute;
  top: 50%;
  left: 50%;
  width: 70%;
  max-width: 120px;
  height: auto;
  transform: translate(-50%, -50%) rotate(-6deg);
  opacity: 0.55;
  pointer-events: none;
  border-radius: 6px;
  box-shadow: 0 2px 10px rgba(0, 0, 0, 0.5);
  z-index: 5;
}
</style>
