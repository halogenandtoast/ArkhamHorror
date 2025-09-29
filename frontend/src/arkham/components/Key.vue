<script lang="ts" setup>

import {computed, ref} from 'vue'
import {imgsrc} from '@/arkham/helpers'
import { type ArkhamKey } from '@/arkham/types/Key'
import type { Message } from '@/arkham/types/Message';
import { MessageType } from '@/arkham/types/Message';
import type { Game } from '@/arkham/types/Game';
import * as ArkhamGame from '@/arkham/types/Game';
import { chaosTokenImage } from '@/arkham/types/ChaosToken'

const props = defineProps<{
  name: ArkhamKey
  game?: Game
  playerId?: string
}>()

const emit = defineEmits<{
  choose: [value: number]
}>()

const keyToImage = (k: ArkhamKey): string => {
  switch (k.tag) {
    case "TokenKey": return chaosTokenImage(k.contents.face)
    case "BlueKey": return imgsrc("keys/blue-key.jpg")
    case "GreenKey": return imgsrc("keys/green-key.jpg")
    case "RedKey": return imgsrc("keys/red-key.jpg")
    case "YellowKey": return imgsrc("keys/yellow-key.jpg")
    case "PurpleKey": return imgsrc("keys/purple-key.jpg")
    case "BlackKey": return imgsrc("keys/black-key.jpg")
    case "WhiteKey": return imgsrc("keys/white-key.jpg")
    case "UnrevealedKey": return imgsrc("keys/key-back.jpg")
  }
}

const keyImage = computed<string>(() => keyToImage(props.name))
const dragging = ref(false)
const choices = computed(() => {
  if (!props.game || !props.playerId) return []
  return ArkhamGame.choices(props.game, props.playerId)
})

function canInteract(c: Message): boolean {
  if (c.tag === MessageType.KEY_LABEL) {
    if (c.key.tag === 'TokenKey') {
      if (props.name.tag !== 'TokenKey') return false
      return c.key.contents.id === props.name.contents.id
    } else {
      return c.key.tag === props.name.tag
    }
  }

  return false
}

const keyAction = computed(() => {
  return choices.value.findIndex(canInteract)
})

function choose() {
  if (keyAction.value !== -1) {
    emit('choose', keyAction.value)
  }
}

function startDrag(event: DragEvent) {
  dragging.value = true
  if (event.dataTransfer) {
    event.dataTransfer.effectAllowed = 'move'
    if (props.name.tag == 'TokenKey') {
      event.dataTransfer.setData('text/plain', JSON.stringify({ tag: "KeyTarget", contents: { tag: "TokenKey", contents: {chaosTokenId: props.name.contents.id, chaosTokenFace: props.name.contents.face, chaosTokenRevealedBy: null, chaosTokenCancelled: false, chaosTokenSealed: false } } }))
    } else {
      event.dataTransfer.setData('text/plain', JSON.stringify({ tag: "KeyTarget", contents: { tag: props.name } }))
    }
  }
}

</script>

<template>
  <img :src="keyImage" class="key" :class="{'key--can-interact': keyAction !== -1 }" @dragstart="startDrag($event)" @click="choose" />
</template>

<style lang="scss" scoped>
.key {
  width: 20px;
  box-shadow: 1px 1px 6px rgba(0, 0, 0, 0.45);
  border-radius: 20px;
}
.key--can-interact {
  border: 2px solid var(--select);
  cursor: pointer;
}
</style>
