<script setup lang="ts">
/* A report body or reply, as written.
 *
 * Rendered as text, never as markup: every word of it was typed by a player.
 * Links are picked out and emitted as real elements rather than by building a
 * string of HTML, so there is no path from what someone writes to something the
 * browser will run.
 *
 * No markdown. The imported GitHub bodies carry some, and it shows through as
 * the characters it is, which is the honest reading of a plain-text field.
 */
import { computed } from 'vue'

const props = defineProps<{ text: string }>()

const URL_PATTERN = /(https?:\/\/[^\s<>"')\]]+)/g

type Piece = { kind: 'text' | 'link'; value: string }

/* The split pattern is global, so it cannot also be the test: `lastIndex` moves
 * between calls and every other piece would come back wrong. */
const isLink = (part: string) => part.startsWith('http://') || part.startsWith('https://')

const pieces = computed<Piece[]>(() =>
  props.text
    .split(URL_PATTERN)
    .filter((part) => part !== '')
    .map((part) => ({ kind: isLink(part) ? 'link' : 'text', value: part })),
)
</script>

<template>
  <div class="report-text">
    <template v-for="(piece, index) in pieces" :key="index">
      <a
        v-if="piece.kind === 'link'"
        :href="piece.value"
        rel="noreferrer noopener"
        target="_blank"
      >{{ piece.value }}</a>
      <template v-else>{{ piece.value }}</template>
    </template>
  </div>
</template>

<style scoped>
.report-text {
  line-height: 1.55;
  overflow-wrap: anywhere;
  white-space: pre-wrap;
}

a {
  color: var(--guardian);
  text-decoration: underline;
  text-underline-offset: 2px;
}
</style>
