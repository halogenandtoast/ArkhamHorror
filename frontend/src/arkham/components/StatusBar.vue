<script lang="ts" setup>
import { useI18n } from 'vue-i18n';
import { ref, computed } from 'vue';
import type { Game } from '@/arkham/types/Game';
import * as ArkhamGame from '@/arkham/types/Game';
import { replaceIcons } from '@/arkham/helpers';
import { MessageType } from '@/arkham/types/Message';
import { QuestionType } from '@/arkham/types/Question';
import Draggable from '@/components/Draggable.vue';
import Question from '@/arkham/components/Question.vue';


const showChoices = computed(() => !props.game.skillTest && searchedCards.value.length == 0 && focusedCards.value.length == 0 && choices.value.some((c) => { return c.tag === MessageType.DONE || c.tag === MessageType.LABEL || c.tag === MessageType.SKILL_LABEL || c.tag === MessageType.SKILL_LABEL_WITH_LABEL || c.tag == MessageType.PORTRAIT_LABEL }))



</script>

<template>
    <section class="status-bar">
    </section>
  </Draggable>
</template>

<style scoped lang="scss">
</style>
