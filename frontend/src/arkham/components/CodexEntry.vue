<template>
  <svg
    :viewBox="`0 0 1113 ${svgHeight}`"
    preserveAspectRatio="xMidYMid meet"
    class="codex-svg"
    xmlns="http://www.w3.org/2000/svg"
    xmlns:xlink="http://www.w3.org/1999/xlink"
  >
    <g style="mix-blend-mode: multiply">
      <mask id="codex-mask" maskUnits="userSpaceOnUse" maskContentUnits="userSpaceOnUse" mask-type="luminance">
        <image x="1" y="-4" width="1104" :height="svgHeight + 3" :xlink:href="maskHref" preserveAspectRatio="none"/>
      </mask>

      <!-- Texture (full viewBox) -->
      <rect x="0" y="0" width="1113" :height="svgHeight" fill="#685c4c" fill-opacity="0.4" mask="url(#codex-mask)"/>

      <!-- Cream interior (matches path1 in source SVG: x=19, y=60, bottom inset=67) -->
      <rect x="19" y="60" width="1064" :height="svgHeight - 127" fill="#fcf8e7"/>

      <!-- Outer page border (path46): top=48.75, bottom inset=66.5 -->
      <rect x="19" y="48.75" width="1064" :height="svgHeight - 115.25" fill="none" stroke="#685c4c" stroke-width="23.125"/>

      <!-- Inner page border (path47): top=79.575, bottom inset=97.325 -->
      <rect x="49.887497" y="79.574951" width="1002.725052" :height="svgHeight - 176.9" fill="none" stroke="#685c4c" stroke-width="7.725"/>

      <!-- Top decorations: corners + edge curls + top medallion (fixed at top) -->
      <g v-html="topDecorations"></g>

      <!-- Bottom decorations: translated to new bottom -->
      <g :transform="`translate(0, ${svgHeight - 1282})`" v-html="bottomDecorations"></g>
    </g>

    <foreignObject x="80" y="80" width="953" :height="foreignHeight">
      <div xmlns="http://www.w3.org/1999/xhtml" class="codex-content" ref="contentRef">
        <slot></slot>
      </div>
    </foreignObject>
  </svg>
</template>

<script lang="ts">
import { defineComponent, ref, computed, onMounted, onBeforeUnmount } from 'vue'
import frameSvg from '@/arkham/assets/codex_frame.svg?raw'

const maskHrefMatch = frameSvg.match(/xlink:href="(data:image\/png;base64,[^"]+)"/)
const maskHref = maskHrefMatch ? maskHrefMatch[1] : ''

function extractPathRange(idStart: number, idEnd: number): string {
  const out: string[] = []
  for (let id = idStart; id <= idEnd; id++) {
    const re = new RegExp(`<path id="path${id}"[^>]*?/>`)
    const match = frameSvg.match(re)
    if (match) out.push(match[0])
  }
  return out.join('\n')
}

// Top: paths 48-59 (corners + edge curls + top medallion) plus the duplicate layer 72-83
const topDecorations = `${extractPathRange(48, 59)}\n${extractPathRange(72, 83)}`
// Bottom: paths 60-71 (corners + edge curls + bottom medallion) plus duplicate layer 84-95
const bottomDecorations = `${extractPathRange(60, 71)}\n${extractPathRange(84, 95)}`

// y=160 above content (matches foreignObject y) + 102 below content (1282 - 1180) = 262
const VERTICAL_PADDING = 262
const MIN_HEIGHT = 500

export default defineComponent({
  setup() {
    const contentRef = ref<HTMLDivElement | null>(null)
    const svgHeight = ref(1282)

    const foreignHeight = computed(() => Math.max(50, svgHeight.value - VERTICAL_PADDING))

    let resizeObserver: ResizeObserver | null = null

    function updateHeight() {
      if (!contentRef.value) return
      // Inside foreignObject, HTML CSS pixels map 1:1 to SVG user units,
      // so scrollHeight is already in viewBox units.
      const contentUnits = contentRef.value.scrollHeight
      if (contentUnits === 0) return
      svgHeight.value = Math.max(MIN_HEIGHT, contentUnits + VERTICAL_PADDING)
    }

    onMounted(() => {
      updateHeight()
      if (contentRef.value) {
        resizeObserver = new ResizeObserver(updateHeight)
        resizeObserver.observe(contentRef.value)
      }
    })

    onBeforeUnmount(() => {
      resizeObserver?.disconnect()
    })

    return {
      contentRef,
      svgHeight,
      foreignHeight,
      maskHref,
      topDecorations,
      bottomDecorations
    }
  }
})
</script>

<style scoped>
.codex-svg {
  display: block;
  width: 100%;
  max-width: 600px;
  height: auto;
  margin-inline: auto;
}

.codex-content {
  --codex-ink: #685c4c;
  color: var(--codex-ink);
  font-family: 'ArkhamFlavor';
  font-size: 32px;
  line-height: 1.4;
  text-align: justify;
  hyphens: auto;
  overflow: visible;
}

.codex-content :deep(header) {
  font-family: 'Teutonic';
  font-size: 1em;
  display: block;
  color: var(--codex-ink);
}

.codex-content :deep(h1) {
  font-family: 'Teutonic';
  font-weight: 600;
  text-align: center;
  font-size: 1.4em;
  color: var(--codex-ink);
  border-bottom: 3px solid var(--codex-ink);
  &:not(.no-underline) {
    border-bottom: 3px solid var(--codex-ink);
  }
  padding-bottom: 4px;
  margin-block: 0 0.8em;
  margin-inline: 10%;
}

.codex-content :deep(h1)::after {
  display: none;
}

.codex-content :deep(h3) {
  font-family: 'Teutonic';
  text-align: center;
  color: var(--codex-ink);
  font-size: 1.1em;
  margin-block: 0.4em;
}

.codex-content :deep(p) {
  margin-block: 0.4em;
  text-indent: 1.5em;
  word-break: auto-phrase;
  color: var(--codex-ink);
}

.codex-content :deep(p:first-of-type) {
  text-indent: 0;
}

.codex-content :deep(.composite) {
  display: contents;
}
</style>
