<script setup lang="ts">
import { OnClickOutside } from '@vueuse/components'
import { ref, watch, computed, onMounted, nextTick } from 'vue';
import type { AbilityMessage } from '@/arkham/types/Message';
import AbilityButton from '@/arkham/components/AbilityButton.vue'

const props = withDefaults(defineProps<{
  abilities: AbilityMessage[];
  frame: HTMLElement | null;
  position?: 'top' | 'bottom' | 'left' | 'right';
  showMove?: boolean
}>(), {showMove: true});

const emits = defineEmits<{
  (e: 'choose', index: number): void;
}>();

const abilitiesRef = ref<HTMLElement | null>(null);
const showAbilities = defineModel()
const abilitiesPosition = ref({ bottom: '0px', top: '0px', left: '0px' });
const positionClass = computed(() => props.position || 'top');
onMounted(() => {
  nextTick(() => {
    calculatePosition();
  });
});

function calculatePosition() {
  if (props.frame) {
    const rect = props.frame.getBoundingClientRect();
    const positionStyle: Record<string, string> = {};

    switch (positionClass.value) {
      case 'bottom':
        positionStyle.top = `${rect.bottom + window.scrollY}px`;
        positionStyle.left = `${rect.left + window.scrollX}px`;
        break;
      case 'left':
        positionStyle.top = `${rect.top + window.scrollY}px`;
        positionStyle.right = `${window.innerWidth - rect.left}px`;
        break;
      case 'right':
        positionStyle.top = `${rect.top + window.scrollY}px`;
        positionStyle.left = `${rect.right}px`;
        break;
      case 'top':
      default:
        positionStyle.bottom = `${window.innerHeight - rect.top}px`;
        positionStyle.left = `${rect.left + window.scrollX}px`;
        break;
    }

    abilitiesPosition.value = positionStyle;
  }
}

function chooseAbility(index: number) {
  emits('choose', index);
}

watch(
  () => props.abilities,
  (newAbilities) => {
    if (newAbilities.some(a => 'ability' in a.contents && a.contents.ability.type.tag === 'ForcedAbility')) {
      showAbilities.value = true;
      calculatePosition();
    } else if (newAbilities.length === 0) {
      showAbilities.value = false;
    }
  },
  { immediate: true }
);

watch(showAbilities, (newValue) => {
  if (newValue) {
    calculatePosition();
  }
});
</script>

<template>
  <Teleport to="body">
    <OnClickOutside @trigger="showAbilities = false" v-if="showAbilities" :options="{ ignore: [frame] }">
      <div
        class="abilities"
        :class="position"
        :style="abilitiesPosition"
        ref="abilitiesRef"
      >
        <AbilityButton
          v-for="ability in abilities"
          :key="ability.index"
          :ability="ability.contents"
          :show-move="showMove"
          @click="chooseAbility(ability.index)"
        />
      </div>
    </OnClickOutside>
  </Teleport>
</template>


<style scoped>
.abilities {
  position: absolute;
  padding: 10px;
  background: rgba(0, 0, 0, 0.8);
  border-radius: 10px;
  display: flex;
  flex-direction: column;
  gap: 5px;
  z-index: 1000;
}
</style>
