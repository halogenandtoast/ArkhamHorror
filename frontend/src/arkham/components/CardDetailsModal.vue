<script lang="ts" setup>
import { computed, onBeforeUnmount, onMounted, ref, watch } from 'vue'
import { localizeArkhamDBBaseUrl } from '@/arkham/helpers'
import { altFrontImage, cardBackImage, cardFrontImage, hasCardBackArt } from '@/arkham/cardArt'
import { cardCost, cardIcons, cardSetText, cardTraits, cardType, levelText } from '@/arkham/cardDetails'
import type { CardDef } from '@/arkham/types/CardDef'

const props = defineProps<{ card: CardDef, unimplemented?: boolean, hasPrev?: boolean, hasNext?: boolean }>()
const emit = defineEmits<{ close: [], prev: [], next: [] }>()

const backImage = computed(() => hasCardBackArt(props.card) ? cardBackImage(props.card) : null)

const frontImage = computed(() => cardFrontImage(props.card))
const frontSrc = ref(frontImage.value)
const backFailed = ref(false)

watch(frontImage, (src) => { frontSrc.value = src; backFailed.value = false })

function onFrontError() {
  if (frontSrc.value !== frontImage.value) return
  const alt = altFrontImage(frontImage.value)
  if (alt) frontSrc.value = alt
}

const title = computed(() => props.unimplemented ? `#${props.card.art}` : props.card.name.title)
const type = computed(() => props.card.cardType ? cardType(props.card) : '')
const cost = computed(() => cardCost(props.card))
const icons = computed(() => cardIcons(props.card))
const traits = computed(() => cardTraits(props.card))
const setText = computed(() => cardSetText(props.card))

const arkhamDbUrl = computed(() => `${localizeArkhamDBBaseUrl()}/card/${props.card.art}`)
const arkhamBuildUrl = computed(() => `https://arkham.build/card/${props.card.art}`)

function onKeydown(event: KeyboardEvent) {
  if (event.key === 'Escape') {
    emit('close')
    return
  }

  // Swallow the arrows either way, so an end-of-list press doesn't scroll the
  // grid behind the modal.
  if (event.key === 'ArrowUp') {
    event.preventDefault()
    if (props.hasPrev) emit('prev')
  }

  if (event.key === 'ArrowDown') {
    event.preventDefault()
    if (props.hasNext) emit('next')
  }
}

onMounted(() => window.addEventListener('keydown', onKeydown))
onBeforeUnmount(() => window.removeEventListener('keydown', onKeydown))
</script>

<template>
  <Teleport to="body">
    <div class="backdrop" @click.self="emit('close')">
      <div class="details" role="dialog" aria-modal="true" :aria-label="title">
        <div class="controls">
          <button
            class="step"
            type="button"
            :disabled="!hasPrev"
            :aria-label="$t('cardDetails.previous')"
            v-tooltip="$t('cardDetails.previous')"
            @click="emit('prev')"
          >&#9650;</button>
          <button
            class="step"
            type="button"
            :disabled="!hasNext"
            :aria-label="$t('cardDetails.next')"
            v-tooltip="$t('cardDetails.next')"
            @click="emit('next')"
          >&#9660;</button>
          <button class="close" type="button" :aria-label="$t('cardDetails.close')" @click="emit('close')">&times;</button>
        </div>

        <div class="faces">
          <img class="face" :src="frontSrc" :alt="title" @error="onFrontError" />
          <img
            v-if="backImage && !backFailed"
            class="face"
            :src="backImage"
            :alt="`${title} — ${$t('cardDetails.back')}`"
            @error="backFailed = true"
          />
        </div>

        <div class="info">
          <h2>{{ title }}<span v-if="levelText(card)" class="level">{{ levelText(card) }}</span></h2>
          <p v-if="card.name.subtitle" class="subtitle">{{ card.name.subtitle }}</p>
          <p v-if="unimplemented" class="not-implemented">{{ $t('cardDetails.notImplemented') }}</p>

          <dl>
            <template v-if="type">
              <dt>{{ $t('cardsList.type') }}</dt>
              <dd>{{ type }}</dd>
            </template>
            <template v-if="card.classSymbols.length > 0">
              <dt>{{ $t('cardsList.class') }}</dt>
              <dd>
                <span v-for="(sym, i) in card.classSymbols" :key="sym" :class="`${sym.toLowerCase()}-sym`">
                  {{ sym }}{{ i < card.classSymbols.length - 1 ? ', ' : '' }}
                </span>
              </dd>
            </template>
            <template v-if="cost !== null">
              <dt>{{ $t('cardsList.cost') }}</dt>
              <dd>{{ cost }}</dd>
            </template>
            <template v-if="icons.length > 0">
              <dt>{{ $t('cardsList.icons') }}</dt>
              <dd><i v-for="(icon, i) in icons" :key="i" :class="[icon, `${icon}-icon`]"></i></dd>
            </template>
            <template v-if="traits">
              <dt>{{ $t('cardsList.traits') }}</dt>
              <dd>{{ traits }}</dd>
            </template>
            <dt>{{ $t('cardsList.set') }}</dt>
            <dd>{{ setText }}</dd>
            <dt>{{ $t('cardDetails.code') }}</dt>
            <dd>{{ card.art }}</dd>
          </dl>

          <div class="links">
            <a :href="arkhamDbUrl" target="_blank" rel="noopener">{{ $t('cardDetails.openInArkhamDB') }}</a>
            <a :href="arkhamBuildUrl" target="_blank" rel="noopener">{{ $t('cardDetails.openInArkhamBuild') }}</a>
          </div>
        </div>
      </div>
    </div>
  </Teleport>
</template>

<style scoped>
.backdrop {
  position: fixed;
  inset: 0;
  z-index: var(--z-modal-overlay);
  display: grid;
  place-items: center;
  padding: 20px;
  background: rgba(0, 0, 0, 0.72);
}

/* The faces and the details are one wrapping row: when the window can't fit
   them side by side the details drop underneath, and the faces themselves
   stack once there's no room for two. */
.details {
  position: relative;
  display: flex;
  flex-wrap: wrap;
  gap: 20px;
  max-width: min(1320px, 100%);
  max-height: 100%;
  padding: 20px;
  overflow: auto;
  background: color-mix(in srgb, var(--background) 92%, black 8%);
  border: 1px solid rgba(255, 255, 255, 0.12);
  border-radius: 12px;
  box-shadow: 0 18px 50px rgba(0, 0, 0, 0.55);

  @media (max-width: 768px) {
    gap: 14px;
    padding: 14px;
  }
}

.controls {
  position: absolute;
  top: 8px;
  right: 10px;
  display: flex;
  align-items: center;
  gap: 6px;

  button {
    padding: 0;
    color: #aaa;
    line-height: 1;
    background: transparent;
    border: 0;
    cursor: pointer;

    &:hover:not(:disabled) { color: #fff; }

    &:disabled {
      opacity: 0.3;
      cursor: default;
    }
  }
}

.step {
  font-size: 0.9rem;
}

.close {
  font-size: 1.6rem;
}

.faces {
  display: flex;
  flex-wrap: wrap;
  justify-content: center;
  gap: 12px;
  align-items: flex-start;
  margin: 0 auto;
}

.face {
  width: clamp(260px, 32vw, 470px);
  max-width: 100%;
  max-height: calc(100vh - 140px);
  border-radius: 10px;
  box-shadow: 0 4px 14px rgba(0, 0, 0, 0.4);
}

.info {
  flex: 1 1 260px;
  min-width: 260px;
  max-width: 380px;
  color: #ccc;

  h2 {
    margin: 0;
    /* Clear of the prev/next/close cluster. */
    padding-right: 64px;
    color: #f0e2c0;
    font-size: 1.2rem;
  }

  .level {
    color: #ccc;
    font-weight: 400;
  }

  .subtitle {
    margin: 2px 0 0;
    color: #aaa;
    font-size: 0.86rem;
    font-style: italic;
  }

  .not-implemented {
    display: inline-block;
    margin: 10px 0 0;
    padding: 2px 8px;
    color: #d8c48a;
    background: rgba(200, 169, 110, 0.16);
    border: 1px solid rgba(200, 169, 110, 0.4);
    border-radius: 999px;
    font-size: 0.72rem;
    font-weight: 700;
  }
}

dl {
  display: grid;
  grid-template-columns: auto 1fr;
  gap: 4px 12px;
  margin: 14px 0 0;
  font-size: 0.84rem;
}

dt {
  color: #888;
  font-weight: 600;
}

dd {
  margin: 0;
}

.links {
  display: flex;
  gap: 8px;
  margin-top: 16px;

  a {
    padding: 6px 12px;
    color: #ccc;
    font-size: 0.8rem;
    font-weight: 600;
    text-decoration: none;
    background: rgba(255, 255, 255, 0.06);
    border: 1px solid rgba(255, 255, 255, 0.14);
    border-radius: 8px;

    &:hover {
      color: var(--spooky-green);
      border-color: var(--spooky-green);
    }
  }
}

.guardian-sym  { color: var(--guardian); }
.seeker-sym    { color: var(--seeker); }
.rogue-sym     { color: var(--rogue); }
.mystic-sym    { color: var(--mystic); }
.survivor-sym  { color: var(--survivor); }
.neutral-sym   { color: var(--neutral); }
</style>
