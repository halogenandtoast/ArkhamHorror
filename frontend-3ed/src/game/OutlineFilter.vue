<script setup lang="ts">
// the piece's own alpha, clipped to the space, eroded and subtracted to leave a ring on its real edge
defineProps<{ id: string; stroke: string; fill: string }>()
</script>

<template>
  <filter :id="id" x="-10%" y="-10%" width="120%" height="120%" color-interpolation-filters="sRGB">
    <feComponentTransfer in="SourceAlpha" result="solid"><feFuncA type="linear" slope="3" /></feComponentTransfer>
    <feMorphology in="solid" operator="erode" radius="3" result="inner" />
    <feComposite in="solid" in2="inner" operator="out" result="ring" />
    <feFlood :style="{ floodColor: stroke }" result="edge" />
    <feComposite in="edge" in2="ring" operator="in" result="stroke" />
    <feFlood :flood-color="fill" flood-opacity=".18" />
    <feComposite in2="inner" operator="in" result="fill" />
    <feGaussianBlur in="stroke" stdDeviation="4" result="glow" />
    <feMerge><feMergeNode in="glow" /><feMergeNode in="fill" /><feMergeNode in="stroke" /></feMerge>
  </filter>
</template>
