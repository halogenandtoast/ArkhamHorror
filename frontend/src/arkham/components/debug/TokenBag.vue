<script lang="ts" setup>
import { useI18n } from 'vue-i18n'


import { computed, ref } from 'vue'
import { useDebug } from '@/arkham/debug'
import { localizedTokenFace } from '@/arkham/tokenLabels'
import { chaosTokenImage, compareTokenFaces, tokenOrder, type TokenFace } from '@/arkham/types/ChaosToken'
import { homebrewTotalsTokens } from '@/arkham/homebrewData'
import { type TokenBag } from '@/arkham/types/TokenBag'

const { t: uiT } = useI18n()

const props = defineProps<{ gameId: string; bag: TokenBag; storyId?: string; bagKey?: string }>()
const debug = useDebug()
const addedFace = ref<TokenFace>('Skull')
const sections = computed(() => [
  { label: uiT('ui.inBag'), key: 'bag', tokens: props.bag.tokens, hint: uiT('ui.clickATokenToDrawItsFaceNext') },
  { label: uiT('ui.currentReveal'), key: 'current', tokens: props.bag.currentToken ? [props.bag.currentToken] : [], hint: uiT('ui.lockedUntilItsQueuedEffectsFinish') },
  { label: uiT('ui.setAside'), key: 'aside', tokens: props.bag.setAside, hint: uiT('ui.notAvailableToDraw') },
].map(section => ({ ...section, tokens: [...section.tokens].sort((a, b) => compareTokenFaces(a.face, b.face)) })))
const faces = computed(() => [...new Set([
  ...tokenOrder, ...homebrewTotalsTokens.map(t => t.face),
  ...props.bag.tokens.map(t => t.face), ...props.bag.setAside.map(t => t.face),
])].sort(compareTokenFaces))
const label = (face: string) => localizedTokenFace(face, uiT)

function send(choice: TokenFace | null | { action: string; id?: string; face?: TokenFace }) {
  if (!debug.active) return
  const message = { tag: 'ScenarioSpecific', contents: ['debugTokenBag', choice] }
  if (props.storyId) {
    debug.send(props.gameId, { tag: 'SendMessage', contents: [{ tag: 'StoryTarget', contents: props.storyId }, message] })
  } else if (props.bagKey) {
    debug.send(props.gameId, { tag: 'ScenarioSpecific', contents: ['debugTokenBag', { key: props.bagKey, next: choice }] })
  }
}
</script>

<template>
  <div class="token-bag">
    <div class="draw-status">
      <div><span class="eyebrow">{{ $t('ui.nextDraw') }}</span><strong>{{ bag.debugNext ? label(bag.debugNext) : $t('ui.random') }}</strong></div>
      <button v-if="debug.active && bag.debugNext" type="button" @click="send(null)">{{ $t('ui.clearOverride') }}</button>
    </div>
    <p v-if="bag.cancelNext" class="notice">{{ $t('ui.theNextTestIsCancelledItsDrawOverrideWill') }}</p>
    <section v-for="section in sections" :key="section.key" class="bag-pile">
      <header><h4>{{ section.label }} <span class="count">{{ section.tokens.length }}</span></h4>
        <button v-if="debug.active && section.key === 'aside'" type="button" :disabled="!section.tokens.length" @click="send({ action: 'returnSetAside' })">{{ $t('ui.returnAll') }}</button>
      </header>
      <p class="hint" v-if="debug.active">{{ section.hint }}</p>
      <div class="tokens">
        <div v-for="token in section.tokens" :key="token.id" class="token-slot">
          <button type="button" class="token-face" :disabled="!debug.active || section.key !== 'bag'"
            :class="{ selected: section.key === 'bag' && token.face === bag.debugNext }"
            :title="section.key === 'bag' && debug.active ? $t('ui.drawTokenNext', { token: label(token.face) }) : label(token.face)"
            :aria-label="section.key === 'bag' ? $t('ui.drawTokenNext', { token: label(token.face) }) : label(token.face)"
            @click="send(token.face)"><img :src="chaosTokenImage(token.face)" :alt="label(token.face)" /></button>
          <div v-if="debug.active && section.key !== 'current'" class="token-actions">
            <button v-if="section.key === 'bag'" type="button" :title="$t('ui.setTokenAside', { token: label(token.face) })" :aria-label="$t('ui.setTokenAside', { token: label(token.face) })" @click="send({ action: 'setAside', id: token.id })">↓</button>
            <button v-else type="button" :title="$t('ui.returnTokenToBag', { token: label(token.face) })" :aria-label="$t('ui.returnTokenToBag', { token: label(token.face) })" @click="send({ action: 'return', id: token.id })">↑</button>
            <button v-if="section.key === 'bag'" type="button" class="remove" :title="$t('ui.removeToken', { token: label(token.face) })" :aria-label="$t('ui.removeToken', { token: label(token.face) })" @click="send({ action: 'remove', id: token.id })">−</button>
          </div>
        </div>
        <span v-if="!section.tokens.length" class="empty">{{ section.key === 'current' ? $t('ui.noRevealInProgress') : $t('ui.noTokens') }}</span>
      </div>
    </section>
    <form v-if="debug.active" class="add-token" @submit.prevent="send({ action: 'add', face: addedFace })">
      <label><span class="eyebrow">{{ $t('ui.addAToken') }}</span><select v-model="addedFace" :aria-label="$t('ui.tokenToAdd')"><option v-for="face in faces" :key="face" :value="face">{{ label(face) }}</option></select></label>
      <img :src="chaosTokenImage(addedFace)" :alt="label(addedFace)" />
      <button type="submit" class="primary">{{ $t('ui.add2') }}</button>
    </form>
  </div>
</template>

<style scoped>
.token-bag { display: flex; flex-direction: column; color: var(--text); font-size: .85rem; font-weight: normal; text-transform: none; }
button, select { font: inherit; color: inherit; border: 1px solid var(--box-border); border-radius: 4px; background: var(--background-dark); padding: 6px 10px; }
button { cursor: pointer; transition: background .15s, border-color .15s; }
button:hover:not(:disabled) { background: var(--background-mid); }
button:focus-visible, select:focus-visible { outline: 2px solid var(--spooky-green); outline-offset: 2px; }
button:disabled { cursor: default; opacity: .45; }
.draw-status, .bag-pile header { display: flex; align-items: center; justify-content: space-between; gap: 12px; }
.draw-status { padding-bottom: 14px; }
.draw-status > div { display: flex; flex-direction: column; gap: 4px; }
.eyebrow { display: block; font-size: .7rem; text-transform: uppercase; letter-spacing: .08em; color: var(--title); }
.draw-status strong { font-size: .95rem; font-weight: 600; }
.bag-pile { padding: 14px 0; border-top: 1px solid var(--box-border); }
h4, p { margin: 0; }
h4 { font-size: .85rem; font-weight: 600; display: flex; align-items: center; gap: 8px; }
.count { font-size: .75rem; font-variant-numeric: tabular-nums; background: var(--background-dark); border-radius: 4px; padding: 2px 7px; color: var(--title); }
.hint, .empty { color: var(--title); font-size: .75rem; }
.hint { margin-top: 5px; }
.tokens { display: flex; flex-wrap: wrap; gap: 10px; margin-top: 12px; align-items: flex-start; }
.empty { padding: 4px 0; font-style: italic; }
.token-slot { display: flex; flex-direction: column; align-items: center; gap: 5px; }
.token-face { padding: 3px; border: 2px solid transparent; border-radius: 50%; background: none; }
.token-face:disabled { opacity: 1; }
.token-face.selected { border-color: var(--spooky-green); }
.token-face img { display: block; width: 38px; height: 38px; }
.token-actions { display: flex; gap: 3px; }
.token-actions button { padding: 0; min-width: 24px; height: 24px; line-height: 1; }
.token-actions .remove:hover { background: var(--delete); }
.add-token { display: flex; align-items: flex-end; gap: 10px; padding-top: 14px; border-top: 1px solid var(--box-border); }
.add-token label { flex: 1; min-width: 0; }
.add-token select { width: 100%; margin-top: 6px; }
.add-token img { width: 32px; height: 32px; }
.primary { background: var(--button-1); min-height: 34px; }
.primary:hover:not(:disabled) { background: var(--button-1-highlight); }
.notice { padding: 10px; margin-bottom: 10px; border-left: 3px solid var(--spooky-green); background: var(--background-dark); font-size: .8rem; }
</style>
