<script lang="ts" setup>
import { computed, ref } from 'vue'
import { useDebug } from '@/arkham/debug'
import type { Game } from '@/arkham/types/Game'
import type { Scenario } from '@/arkham/types/Scenario'
import { chaosTokenImage, type TokenFace } from '@/arkham/types/ChaosToken'

const props = defineProps<{
  game: Game
  scenario: Scenario
}>()

const emit = defineEmits<{ close: [] }>()

const debug = useDebug()

const realityAcidDebugToken1 = ref<TokenFace>('Skull')
const realityAcidDebugToken2 = ref<TokenFace>('Cultist')
const realityAcidDebugTokens: TokenFace[] = [
  'PlusOne', 'Zero',
  'MinusOne', 'MinusTwo', 'MinusThree', 'MinusFour', 'MinusFive', 'MinusSix', 'MinusSeven', 'MinusEight',
  'Skull', 'Cultist', 'Tablet', 'ElderThing',
  'AutoFail', 'ElderSign',
]

const realityAcidTokenLabel = (token: TokenFace) => {
  switch (token) {
    case 'PlusOne': return '+1'
    case 'Zero': return '0'
    case 'MinusOne': return '−1'
    case 'MinusTwo': return '−2'
    case 'MinusThree': return '−3'
    case 'MinusFour': return '−4'
    case 'MinusFive': return '−5'
    case 'MinusSix': return '−6'
    case 'MinusSeven': return '−7'
    case 'MinusEight': return '−8'
    case 'AutoFail': return 'Auto-fail'
    case 'ElderSign': return 'Elder sign'
    case 'ElderThing': return 'Elder thing'
    default: return token
  }
}

const tokenOptionClass = (token: TokenFace, selectedToken?: TokenFace) => ({
  'reality-acid-token-option--selected': token === selectedToken,
})

const openRealityAcidDropdown = ref<1 | 2 | null>(null)
const selectRealityAcidToken = (slot: 1 | 2, token: TokenFace) => {
  if (slot === 1) realityAcidDebugToken1.value = token
  else realityAcidDebugToken2.value = token
  openRealityAcidDropdown.value = null
}

const isNegative = (token: TokenFace) => token.startsWith('Minus')
const negativeValue = (token: TokenFace) => {
  switch (token) {
    case 'MinusOne': return 1
    case 'MinusTwo': return 2
    case 'MinusThree': return 3
    case 'MinusFour': return 4
    case 'MinusFive': return 5
    case 'MinusSix': return 6
    case 'MinusSeven': return 7
    case 'MinusEight': return 8
    default: return null
  }
}
const isNegative4To8 = (token: TokenFace) => (negativeValue(token) ?? 0) >= 4
const isNegative1Or2 = (token: TokenFace) => token === 'MinusOne' || token === 'MinusTwo'
const isNegative2Or3 = (token: TokenFace) => token === 'MinusTwo' || token === 'MinusThree'
const isSkullCultist = (token: TokenFace) => token === 'Skull' || token === 'Cultist'
const isTabletElderThing = (token: TokenFace) => token === 'Tablet' || token === 'ElderThing'
const isSymbol = (token: TokenFace) => token === 'Skull' || token === 'Cultist' || token === 'Tablet' || token === 'ElderThing'
const isZeroOrPlusOne = (token: TokenFace) => token === 'Zero' || token === 'PlusOne'
const tokenMatch = (matchA: (token: TokenFace) => boolean, matchB: (token: TokenFace) => boolean) => {
  const token1 = realityAcidDebugToken1.value
  const token2 = realityAcidDebugToken2.value
  return (matchA(token1) && matchB(token2)) || (matchA(token2) && matchB(token1))
}

const realityAcidPreview = computed(() => {
  if (tokenMatch((t) => t === 'ElderSign', isSkullCultist)) return '…The non-Elite enemy nearest to you.'
  if (tokenMatch((t) => t === 'ElderSign', isTabletElderThing)) return '…A treachery at your location.'
  if (tokenMatch((t) => t === 'ElderSign', isNegative)) return '…1 horror and 1 damage from your investigator card.'
  if (tokenMatch((t) => t === 'ElderSign', isZeroOrPlusOne)) return '…Your greatest flaw. Search your deck for any 1 weakness card.'
  if (tokenMatch((t) => t === 'ElderSign', (t) => t === 'AutoFail')) return '…The Elder Sign token just revealed.'
  if (tokenMatch((t) => t === 'PlusOne', isSkullCultist)) return '…Your caution. Resolve Reality Acid three more times.'
  if (tokenMatch((t) => t === 'PlusOne', isTabletElderThing)) return '…Your ignorance. Discover 1 clue at your location.'
  if (tokenMatch((t) => t === 'PlusOne', isNegative)) return '…Friendships. Investigators cannot commit cards to each others’ skill tests this round.'
  if (tokenMatch((t) => t === 'Zero', (t) => t === 'Zero')) return '…1 per-investigator clues from your location.'
  if (tokenMatch((t) => t === 'Zero', isNegative1Or2)) return '…Itself, then regurgitates itself. Deal 1 damage to Subject 8L-08.'
  if (tokenMatch((t) => t === 'Zero', (t) => t === 'MinusThree')) return '…All damage from each Manifold enemy.'
  if (tokenMatch((t) => t === 'Zero', isNegative4To8)) return '…All supplies, ammo, charges, and secrets among assets you control.'
  if (tokenMatch((t) => t === 'MinusOne', (t) => t === 'Skull')) return '…Your hope. Set your base willpower to 0.'
  if (tokenMatch((t) => t === 'MinusOne', (t) => t === 'Cultist')) return '…Your curiosity. Set your base intellect to 0.'
  if (tokenMatch((t) => t === 'MinusOne', (t) => t === 'Tablet')) return '…Your precision. Set your base combat to 0.'
  if (tokenMatch((t) => t === 'MinusOne', (t) => t === 'ElderThing')) return '…Your attention. Set your base agility to 0.'
  if (tokenMatch((t) => t === 'MinusOne', (t) => t === 'MinusOne')) return '…Your versatility. You cannot play off-class cards this round.'
  if (tokenMatch((t) => t === 'MinusOne', (t) => t === 'MinusTwo')) return '…Your cell phone.'
  if (tokenMatch((t) => t === 'MinusOne', (t) => t === 'MinusThree')) return '…The chaos bag.'
  if (tokenMatch((t) => t === 'MinusOne', isNegative4To8)) return '…Level 1–5 cards of your choice with at least 5 total levels.'
  if (tokenMatch((t) => t === 'MinusTwo', isSkullCultist)) return '…The top 3 cards of your deck.'
  if (tokenMatch((t) => t === 'MinusTwo', isTabletElderThing)) return '…The top 3 cards of your discard pile.'
  if (tokenMatch((t) => t === 'MinusTwo', (t) => t === 'MinusTwo')) return '…The deckbox you store your deck in.'
  if (tokenMatch((t) => t === 'MinusTwo', (t) => t === 'MinusThree')) return '…Your investigator’s card sleeve.'
  if (tokenMatch((t) => t === 'MinusTwo', isNegative4To8)) return '…Your party’s teamwork. Each investigator loses 1 action.'
  if (tokenMatch((t) => t === 'MinusThree', isSymbol)) return '…A Talent, Connection, or Condition asset you control.'
  if (tokenMatch((t) => t === 'MinusThree', isNegative4To8)) return '…Your sense of time.'
  if (tokenMatch((t) => t === 'Skull', (t) => t === 'Skull')) return '…The highest-cost Ally asset you control.'
  if (tokenMatch((t) => t === 'Skull', (t) => t === 'Cultist')) return '…All event cards in your hand.'
  if (tokenMatch((t) => t === 'Skull', (t) => t === 'Tablet')) return '…All of your resources.'
  if (tokenMatch((t) => t === 'Skull', (t) => t === 'ElderThing')) return '…All skill cards in your hand.'
  if (tokenMatch((t) => t === 'Cultist', (t) => t === 'Cultist')) return '…All cards that have been exiled.'
  if (tokenMatch((t) => t === 'Cultist', (t) => t === 'Tablet')) return '…Your identity.'
  if (tokenMatch((t) => t === 'Cultist', (t) => t === 'ElderThing')) return '…The concept of speed.'
  if (tokenMatch((t) => t === 'Tablet', (t) => t === 'Tablet')) return '…Your sense of urgency. You cannot move this round.'
  if (tokenMatch((t) => t === 'Tablet', (t) => t === 'ElderThing')) return '…Your potential. Your skills cannot be increased this round.'
  if (tokenMatch((t) => t === 'ElderThing', (t) => t === 'ElderThing')) return '…Your patience. Place 1 doom on the current agenda.'
  if (tokenMatch((t) => t === 'AutoFail', isSkullCultist)) return '…All Spell and Ritual assets you control.'
  if (tokenMatch((t) => t === 'AutoFail', isTabletElderThing)) return '…All Item assets you control.'
  if (tokenMatch((t) => t === 'AutoFail', (t) => t === 'PlusOne')) return '…The concept of a “discard pile.”'
  if (tokenMatch((t) => t === 'AutoFail', (t) => t === 'Zero')) return '…One of your hands.'
  if (tokenMatch((t) => t === 'AutoFail', (t) => t === 'MinusOne')) return '…The concept of success.'
  if (tokenMatch((t) => t === 'AutoFail', isNegative2Or3)) return '…1 countermeasure.'
  if (tokenMatch((t) => t === 'AutoFail', isNegative4To8)) return '…The concept of easiness.'
  if (tokenMatch((t) => t === 'PlusOne', (t) => t === 'Zero')) return '…Your player reference card.'
  if (tokenMatch(isNegative4To8, isSkullCultist)) return '…Your investigator mini card.'
  if (tokenMatch(isNegative4To8, isTabletElderThing)) return '…Your house.'
  if (tokenMatch(isNegative4To8, isNegative4To8)) return '…Your soul.'
  if (tokenMatch((t) => t === 'Zero', (t) => t === 'Skull')) return '…Your voice.'
  if (tokenMatch((t) => t === 'Zero', (t) => t === 'Cultist')) return '…Your group’s food and drinks.'
  if (tokenMatch((t) => t === 'Zero', (t) => t === 'Tablet')) return '…The concept of language.'
  if (tokenMatch((t) => t === 'Zero', (t) => t === 'ElderThing')) return '…Light.'
  return 'Combination is not listed, or may be unavailable; Reality Acid will reveal two new chaos tokens.'
})

const realityAcidDebugChoice = computed(() => {
  const tokens = props.scenario.meta?.debugRealityAcidTokens
  return Array.isArray(tokens) && tokens.length === 2 ? tokens as TokenFace[] : null
})

const realityAcidDebugChoiceLabel = computed(() => realityAcidDebugChoice.value?.map(realityAcidTokenLabel).join(' + '))

const setRealityAcidDebugTokens = () => {
  debug.send(props.game.id, {
    tag: 'ScenarioSpecific',
    contents: ['blobSetDebugRealityAcidTokens', [realityAcidDebugToken1.value, realityAcidDebugToken2.value]],
  })
}

const clearRealityAcidDebugTokens = () => {
  debug.send(props.game.id, { tag: 'ScenarioSpecific', contents: ['blobClearDebugRealityAcidTokens', null] })
}
</script>

<template>
  <div class="scenario-debug-modal-overlay" @click.self="emit('close')">
    <div class="scenario-debug-modal" @click="openRealityAcidDropdown = null">
      <header class="scenario-debug-modal-header">
        <h2>Scenario Debug</h2>
        <button type="button" @click="emit('close')">×</button>
      </header>
      <div class="scenario-debug-options">
        <section v-if="scenario.id === 'c85001'" class="scenario-debug-section">
          <span class="scenario-debug-label">Reality Acid tokens</span>
          <div class="scenario-debug-row">
            <div class="reality-acid-token-dropdown" @click.stop>
              <button
                type="button"
                class="reality-acid-token-trigger"
                :class="tokenOptionClass(realityAcidDebugToken1)"
                :title="realityAcidTokenLabel(realityAcidDebugToken1)"
                @click="openRealityAcidDropdown = openRealityAcidDropdown === 1 ? null : 1"
              >
                <img class="reality-acid-token-image" :src="chaosTokenImage(realityAcidDebugToken1)" :alt="realityAcidTokenLabel(realityAcidDebugToken1)" />
              </button>
              <div v-if="openRealityAcidDropdown === 1" class="reality-acid-token-menu">
                <button
                  v-for="token in realityAcidDebugTokens"
                  :key="`acid-1-${token}`"
                  type="button"
                  class="reality-acid-token-option"
                  :class="tokenOptionClass(token, realityAcidDebugToken1)"
                  :title="realityAcidTokenLabel(token)"
                  @click="selectRealityAcidToken(1, token)"
                >
                  <img class="reality-acid-token-image" :src="chaosTokenImage(token)" :alt="realityAcidTokenLabel(token)" />
                </button>
              </div>
            </div>
            <div class="reality-acid-token-dropdown" @click.stop>
              <button
                type="button"
                class="reality-acid-token-trigger"
                :class="tokenOptionClass(realityAcidDebugToken2)"
                :title="realityAcidTokenLabel(realityAcidDebugToken2)"
                @click="openRealityAcidDropdown = openRealityAcidDropdown === 2 ? null : 2"
              >
                <img class="reality-acid-token-image" :src="chaosTokenImage(realityAcidDebugToken2)" :alt="realityAcidTokenLabel(realityAcidDebugToken2)" />
              </button>
              <div v-if="openRealityAcidDropdown === 2" class="reality-acid-token-menu">
                <button
                  v-for="token in realityAcidDebugTokens"
                  :key="`acid-2-${token}`"
                  type="button"
                  class="reality-acid-token-option"
                  :class="tokenOptionClass(token, realityAcidDebugToken2)"
                  :title="realityAcidTokenLabel(token)"
                  @click="selectRealityAcidToken(2, token)"
                >
                  <img class="reality-acid-token-image" :src="chaosTokenImage(token)" :alt="realityAcidTokenLabel(token)" />
                </button>
              </div>
            </div>
          </div>
          <div class="reality-acid-preview">
            <span class="reality-acid-preview-label">Will devour</span>
            <span>{{ realityAcidPreview }}</span>
          </div>
          <button type="button" @click="setRealityAcidDebugTokens">Use for next Reality Acid</button>
          <button v-if="realityAcidDebugChoice" type="button" @click="clearRealityAcidDebugTokens">
            Clear (currently {{ realityAcidDebugChoiceLabel }})
          </button>
        </section>

      </div>
    </div>
  </div>
</template>

<style scoped>
.scenario-debug-modal-overlay {
  position: fixed;
  inset: 0;
  z-index: var(--z-modal-overlay, 10000);
  background: rgba(0, 0, 0, 0.5);
  display: flex;
  align-items: center;
  justify-content: center;
  padding: 16px;
}

.scenario-debug-modal {
  width: min(520px, 100%);
  max-height: 90vh;
  overflow: visible;
  border: 1px solid rgba(255, 255, 255, 0.18);
  border-radius: 8px;
  background: rgba(20, 24, 32, 0.96);
  box-shadow: 0 12px 40px rgba(0, 0, 0, 0.45);
  padding: 14px;
}

.scenario-debug-modal-header {
  display: flex;
  align-items: center;
  justify-content: space-between;
  gap: 12px;
  color: white;
  margin-bottom: 12px;
}

.scenario-debug-modal-header h2 {
  margin: 0;
  font-size: 1rem;
}

.scenario-debug-modal-header button {
  border: 0;
  background: transparent;
  color: white;
  cursor: pointer;
  font-size: 1.5rem;
  line-height: 1;
}

.scenario-debug-options {
  display: flex;
  flex-direction: column;
  gap: 12px;
  color: white;
  font-size: 0.75rem;
  font-weight: 700;
  text-transform: uppercase;
}

.scenario-debug-options button {
  min-width: 0;
  border: 1px solid rgba(255, 255, 255, 0.25);
  border-radius: 4px;
  background: rgba(0, 0, 0, 0.65);
  color: white;
  font-size: 0.8rem;
  padding: 4px 6px;
  cursor: pointer;
}

.scenario-debug-options > button,
.scenario-debug-section > button {
  background: var(--button);
}

.reality-acid-token-dropdown {
  position: relative;
  min-width: 0;
}

.scenario-debug-options .reality-acid-token-trigger {
  width: 100%;
  min-height: 40px;
  display: flex;
  align-items: center;
  justify-content: center;
  background: rgba(0, 0, 0, 0.65);
  font-size: 1rem;
}

.reality-acid-token-trigger::after {
  content: "▾";
  position: absolute;
  right: 8px;
  font-family: sans-serif;
  font-size: 0.7rem;
  opacity: 0.7;
}

.reality-acid-token-menu {
  position: absolute;
  z-index: calc(var(--z-modal-overlay, 10000) + 1);
  top: calc(100% + 4px);
  left: 0;
  width: max-content;
  min-width: 100%;
  display: grid;
  grid-template-columns: repeat(4, 40px);
  gap: 4px;
  padding: 6px;
  border: 1px solid rgba(255, 255, 255, 0.25);
  border-radius: 6px;
  background: rgba(10, 12, 16, 0.98);
  box-shadow: 0 8px 20px rgb(0 0 0 / 55%);
}

.scenario-debug-options .reality-acid-token-option {
  width: 40px;
  height: 40px;
  display: flex;
  align-items: center;
  justify-content: center;
  background: rgba(255, 255, 255, 0.08);
  padding: 3px;
  font-size: 1rem;
}

.scenario-debug-options .reality-acid-token-option:hover,
.scenario-debug-options .reality-acid-token-option--selected {
  border-color: rgba(167, 184, 58, 0.75);
  background: rgba(95, 141, 46, 0.45);
}

.reality-acid-token-image {
  width: 30px;
  height: 30px;
  object-fit: contain;
  display: block;
}

.reality-acid-token-trigger .reality-acid-token-image {
  width: 34px;
  height: 34px;
}

.scenario-debug-section {
  display: flex;
  flex-direction: column;
  gap: 8px;
}

.scenario-debug-row {
  display: grid;
  grid-template-columns: 1fr 1fr;
  gap: 6px;
}

.reality-acid-preview {
  border: 1px solid rgba(167, 184, 58, 0.45);
  border-radius: 6px;
  background: rgba(95, 141, 46, 0.18);
  padding: 8px;
  text-transform: none;
  line-height: 1.25;
}

.reality-acid-preview-label {
  display: block;
  margin-bottom: 3px;
  opacity: 0.75;
  font-size: 0.68rem;
  text-transform: uppercase;
}

.scenario-debug-label {
  opacity: 0.85;
  text-transform: none;
}
</style>
