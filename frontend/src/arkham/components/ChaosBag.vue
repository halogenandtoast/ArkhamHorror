<script lang="ts" setup>
import { computed, ref, watch } from 'vue';
import { useDebug } from '@/arkham/debug';
import { Game } from '@/arkham/types/Game';
import { imgsrc, formatContent } from '@/arkham/helpers';
import * as ArkhamGame from '@/arkham/types/Game';
import { ChaosTokenValueEntry, SkillTest } from '@/arkham/types/SkillTest';
import { MessageType } from '@/arkham/types/Message';
import { ChaosBag } from '@/arkham/types/ChaosBag';
import Token from '@/arkham/components/Token.vue';
import ChaosBagChoice from '@/arkham/components/ChaosBagChoice.vue';
import { chaosTokenEffectKey } from '@/arkham/types/Scenario';
import { useI18n } from 'vue-i18n';
import { Dropdown } from 'floating-vue';
import { chanceOfSuccess } from '@/arkham/chaosBagOdds';

const props = defineProps<{
  game: Game
  skillTest: SkillTest | null
  chaosBag: ChaosBag
  playerId: string
}>()

const emit = defineEmits<{
  choose: [value: number]
}>()

function imageFor(tokenFace: string) {
  switch (tokenFace) {
    case 'PlusOne':
      return imgsrc("chaos-tokens/ct_plus1.png");
    case 'Zero':
      return imgsrc("chaos-tokens/ct_0.png");
    case 'MinusOne':
      return imgsrc("chaos-tokens/ct_minus1.png");
    case 'MinusTwo':
      return imgsrc("chaos-tokens/ct_minus2.png");
    case 'MinusThree':
      return imgsrc("chaos-tokens/ct_minus3.png");
    case 'MinusFour':
      return imgsrc("chaos-tokens/ct_minus4.png");
    case 'MinusFive':
      return imgsrc("chaos-tokens/ct_minus5.png");
    case 'MinusSix':
      return imgsrc("chaos-tokens/ct_minus6.png");
    case 'MinusSeven':
      return imgsrc("chaos-tokens/ct_minus7.png");
    case 'MinusEight':
      return imgsrc("chaos-tokens/ct_minus8.png");
    case 'AutoFail':
      return imgsrc("chaos-tokens/ct_autofail.png");
    case 'ElderSign':
      return imgsrc("chaos-tokens/ct_eldersign.png");
    case 'Skull':
      return imgsrc("chaos-tokens/ct_skull.png");
    case 'Cultist':
      return imgsrc("chaos-tokens/ct_cultist.png");
    case 'Tablet':
      return imgsrc("chaos-tokens/ct_tablet.png");
    case 'ElderThing':
      return imgsrc("chaos-tokens/ct_elderthing.png");
    case 'BlessToken':
      return imgsrc("chaos-tokens/ct_bless.png");
    case 'CurseToken':
      return imgsrc("chaos-tokens/ct_curse.png");
    case 'FrostToken':
      return imgsrc("chaos-tokens/ct_frost.png");
    default: {
      if (tokenFace.includes(':')) {
        const [, campaign, key] = tokenFace.split(':')
        if (campaign && key) return imgsrc(`homebrew/${campaign}/chaos-tokens/${key}.png`)
      }
      return imgsrc("chaos-tokens/ct_blank.png");
    }
  }
}

const revealedChaosTokens = computed(() => {
  if (props.skillTest) return props.game.skillTestChaosTokens

  if (props.game.focusedChaosTokens.length > 0) {
    const tokens = [...props.game.skillTestChaosTokens, ...props.game.focusedChaosTokens]
    return Array.from(new Set(tokens.map((token) => JSON.stringify(token))))
      .map((token) => JSON.parse(token) as (typeof tokens)[number]);
  }

  return props.game.skillTestChaosTokens;
})

const choices = computed(() => ArkhamGame.choices(props.game, props.playerId))
const tokenAction = computed(() => choices.value.findIndex((c) => c.tag === MessageType.START_SKILL_TEST_BUTTON))
const debug = useDebug()
const { t } = useI18n()
const allTokenFaces = computed(() => props.chaosBag.chaosTokens.map(t => t.face).sort(sortTokenFaces))
const tokenOrder = ['PlusOne', 'Zero', 'MinusOne', 'MinusTwo', 'MinusThree', 'MinusFour', 'MinusFive', 'MinusSix', 'MinusSeven', 'MinusEight', 'Skull', 'Cultist', 'Tablet', 'ElderThing', 'AutoFail', 'ElderSign', 'CurseToken', 'BlessToken', 'FrostToken']

function sortTokenFaces(a: string, b: string) {
  return tokenOrder.indexOf(a) - tokenOrder.indexOf(b)
}

// Scenario effect text for the symbol tokens. `false` disables `v-tooltip` on the
// faces that have none.
function tokenTooltip(tokenFace: string) {
  const scenario = props.game.scenario
  if (!scenario) return false

  const key = chaosTokenEffectKey(scenario, tokenFace)
  if (!key) return false

  const text = t(key)
  if (text === key) return false

  return { content: formatContent(text), html: true }
}

// Only supplied while a skill test is running; absent means no values and no stats bar.
const breakdown = computed(() => props.game.skillTest?.valueBreakdown ?? null)

// Odds belong to the test being taken, so the bar is skill-test-window only. The
// chaos bag window still gets the per-token value pills.
const showStatsBar = computed(() => !!breakdown.value && props.skillTest !== null)

const whatIfSkill = ref(0)
const whatIfDifficulty = ref(0)

const seedWhatIf = () => {
  const b = breakdown.value
  if (!b) return
  whatIfSkill.value = b.skillValue
  whatIfDifficulty.value = b.difficulty
}

watch(breakdown, seedWhatIf, { immediate: true })

const isModified = computed(() => {
  const b = breakdown.value
  return !!b && (whatIfSkill.value !== b.skillValue || whatIfDifficulty.value !== b.difficulty)
})

// Opt-in per skill test: seeing the odds should be a deliberate act, so this
// resets whenever a new test starts.
const showOdds = ref(false)

watch(() => props.game.skillTest?.id, () => { showOdds.value = false })

const oddsPercent = computed(() => {
  const b = breakdown.value
  return b ? Math.round(chanceOfSuccess(b, whatIfSkill.value, whatIfDifficulty.value) * 100) : null
})

const breakdownRows = computed(() =>
  [...(breakdown.value?.tokens ?? [])].sort((a, b) => sortTokenFaces(a.face, b.face))
)

const canForceDraw = computed(() => debug.active && tokenAction.value !== -1)

const forceDraw = (tokenFace: string) =>
  debug.send(props.game.id, {tag: 'ChaosBagMessage', contents: {tag: 'ForceChaosTokenDraw_', contents: tokenFace}})

// Faces whose art already states their value; the rest get a label beneath them.
const numericFaces = tokenOrder.slice(0, tokenOrder.indexOf('Skull'))

const valuesByFace = computed(
  () => new Map((breakdown.value?.tokens ?? []).map((e) => [e.face, e]))
)

function faceValueLabel(tokenFace: string) {
  if (numericFaces.includes(tokenFace) || tokenFace === 'AutoFail') return null
  const entry = valuesByFace.value.get(tokenFace)
  if (!entry || entry.value === null) return null
  return entry.value > 0 ? `+${entry.value}` : `${entry.value}`
}

function entryValueLabel(entry: ChaosTokenValueEntry) {
  if (entry.autoFail) return t('gameBar.chaosBagStats.autoFail')
  if (entry.autoSuccess) return t('gameBar.chaosBagStats.autoSuccess')
  if (entry.value === null) return '?'
  return entry.value > 0 ? `+${entry.value}` : `${entry.value}`
}

const choose = (idx: number) => emit('choose', idx)
</script>

<template>
  <div class="chaos-bag">
    <div class="chaos-bag-contents">
      <Token v-for="revealedToken in revealedChaosTokens" :key="revealedToken.id" :token="revealedToken" :playerId="playerId" :game="game" @choose="choose" />
      <img
        v-if="tokenAction !== -1"
        class="token token--can-draw"
        :src="imgsrc('chaos-tokens/ct_blank.png')"
        @click="choose(tokenAction)"
      />
      <ChaosBagChoice v-if="chaosBag.choice && 'step' in chaosBag.choice && !game.skillTestResults" :choice="chaosBag.choice.step" :game="game" :playerId="playerId" @choose="choose" />
    </div>

    <div class="token-preview" :class="{ 'token-preview--debug': canForceDraw }">
      <div
        v-for="(tokenFace, idx) in allTokenFaces"
        :key="`${tokenFace}${idx}`"
        class="token-slot"
        v-tooltip="tokenTooltip(tokenFace)"
        @click="canForceDraw && forceDraw(tokenFace)"
      >
        <span v-if="faceValueLabel(tokenFace)" class="count-pill token-slot__value">{{ faceValueLabel(tokenFace) }}</span>
        <img
          class="token"
          :class="{'token-big': skillTest === null}"
          :src="imageFor(tokenFace)"
        />
      </div>
    </div>

    <Dropdown
      v-if="showStatsBar"
      class="stats-bar-anchor"
      placement="top"
      :distance="6"
      theme="chaos-bag-stats-popover"
    >
      <div class="stats-bar" role="button" tabindex="0">
        <span class="stats-bar__count">{{ $t('gameBar.chaosBagStats.tokens', { count: allTokenFaces.length }) }}</span>
        <span v-if="showOdds" class="stats-bar__odds" :class="{ 'stats-bar__odds--whatif': isModified }">{{ oddsPercent }}%</span>
        <button v-else type="button" class="stats-bar__reveal" @click.stop="showOdds = true">
          {{ $t('gameBar.chaosBagStats.showOdds') }}
        </button>
        <span class="stats-bar__caret" aria-hidden="true">&#9652;</span>
      </div>

      <template #popper>
        <div class="stats">
          <div class="stats__headline">
            <template v-if="showOdds">
              <span class="stats__percent">{{ oddsPercent }}%</span>
              <span class="stats__label">{{ $t('gameBar.chaosBagStats.chanceOfSuccess') }}</span>
            </template>
            <button v-else type="button" class="stats__reveal" @click="showOdds = true">
              {{ $t('gameBar.chaosBagStats.showOdds') }}
            </button>
          </div>

          <div class="stats__whatif">
            <label>
              {{ $t('gameBar.chaosBagStats.skill') }}
              <input v-model.number="whatIfSkill" type="number" />
            </label>
            <label>
              {{ $t('gameBar.chaosBagStats.difficulty') }}
              <input v-model.number="whatIfDifficulty" type="number" />
            </label>
            <button v-if="isModified" type="button" class="stats__reset" @click="seedWhatIf">
              {{ $t('gameBar.chaosBagStats.reset') }}
            </button>
          </div>

          <div class="stats__table">
            <div v-for="entry in breakdownRows" :key="entry.face" class="stats__row">
              <img class="stats__token" :src="imageFor(entry.face)" />
              <span class="stats__multiplier">&times;{{ entry.count }}</span>
              <span class="stats__value">{{ entryValueLabel(entry) }}</span>
            </div>
          </div>

          <p class="stats__caveat">{{ $t('gameBar.chaosBagStats.caveat') }}</p>
        </div>
      </template>
    </Dropdown>
  </div>
</template>

<style scoped>
.token--can-draw {
  border: min(5px, 1vw) solid var(--select);
  border-radius: 500px;
  cursor: pointer;
}

.token {
  width: min(100px, 20vw);
  height: auto;
  margin-bottom: 10px;
}

.portrait {
  width: var(--card-width);
  height: auto;
}

.chaos-bag-contents {
  display: flex;
  align-items: center;
  justify-content: center;
  flex-wrap: wrap;
   @media (max-width: 800px) and (orientation: portrait) {
    position:absolute;
    width: 100%;
    left: 0;
   }
}

.token-preview {
  display: flex;
  gap: 5px;
  flex-wrap: wrap;
  flex-direction: row;
  justify-content: center;
  /* slots with a pill are taller; align on the token edge, not the slot top */
  align-items: flex-end;
  @media (max-width: 800px) and (orientation: portrait) {
    display: grid;
    grid-template-columns: 1fr 1fr 1fr 3fr 1fr 1fr 1fr;
    .token-slot {
      margin: 0 auto;
    }
    .token-slot:nth-child(6n+1) {
      grid-column: 1;
    }

    .token-slot:nth-child(6n+2) {
      grid-column: 2;
    }

    .token-slot:nth-child(6n+3) {
      grid-column: 3;
    }

    .token-slot:nth-child(6n+4) {
      grid-column: 5;
    }

    .token-slot:nth-child(6n+5) {
      grid-column: 6;
    }

    .token-slot:nth-child(6n) {
      grid-column: 7;
    }
  }
  
  img {
    width: 30px;
    height: auto;
    transition: transform 0.2s;
    &:hover {
      transform: scale(1.2);
    }
    &.token-big {
      width: 50px;
      border-radius: 50px;
    }
    border: 1px solid rgba(255,255,255,0.4);
    border-radius: 30px;
    box-shadow: 0 4px 4px rgba(0,0,0,0.5);
  }
}

.chaos-bag {
  padding: 10px;
  background: rgba(0,0,0,0.5);
  display: flex;
  flex-direction: column;
  gap: 10px;
}

.stats-bar-anchor {
  margin: -6px -10px -10px;
}

.stats-bar {
  display: flex;
  align-items: center;
  gap: 8px;
  padding: 5px 10px;
  border-top: 1px solid rgba(255, 255, 255, 0.12);
  background: rgba(0, 0, 0, 0.35);
  color: rgba(255, 255, 255, 0.7);
  font-family: sans-serif;
  font-size: 11px;
  line-height: 16px;
  cursor: pointer;
  user-select: none;
  &:hover {
    background: rgba(0, 0, 0, 0.55);
    color: rgba(255, 255, 255, 0.95);
  }
}

.stats-bar__count {
  flex: 1;
}

.stats-bar__odds {
  font-variant-numeric: tabular-nums;
  font-weight: 600;
}

.stats-bar__odds--whatif {
  color: var(--important);
  font-style: italic;
}

.stats-bar__reveal {
  padding: 0 6px;
  border: 1px solid rgba(255, 255, 255, 0.25);
  border-radius: 999px;
  background: transparent;
  color: inherit;
  font: inherit;
  line-height: 14px;
  cursor: pointer;
  &:hover {
    border-color: rgba(255, 255, 255, 0.5);
  }
}

.stats-bar__caret {
  opacity: 0.6;
  font-size: 9px;
}

.stats {
  display: flex;
  flex-direction: column;
  gap: 10px;
  padding: 12px;
  max-width: min(320px, 85vw);
  font-family: sans-serif;
  font-size: 12px;
}

.stats__headline {
  display: flex;
  align-items: baseline;
  gap: 8px;
}

.stats__percent {
  font-size: 22px;
  font-weight: 700;
  font-variant-numeric: tabular-nums;
}

.stats__label {
  color: rgba(255, 255, 255, 0.7);
}

.stats__reveal {
  padding: 4px 12px;
  border: 1px solid rgba(255, 255, 255, 0.25);
  border-radius: 999px;
  background: transparent;
  color: rgba(255, 255, 255, 0.9);
  font: inherit;
  cursor: pointer;
  &:hover {
    border-color: rgba(255, 255, 255, 0.5);
  }
}

.stats__whatif {
  display: flex;
  align-items: center;
  gap: 10px;
  flex-wrap: wrap;
  padding-bottom: 8px;
  border-bottom: 1px solid rgba(255, 255, 255, 0.12);

  label {
    display: flex;
    align-items: center;
    gap: 4px;
    color: rgba(255, 255, 255, 0.7);
  }

  input {
    width: 46px;
    padding: 2px 4px;
    border: 1px solid rgba(255, 255, 255, 0.2);
    border-radius: 4px;
    background: rgba(0, 0, 0, 0.4);
    color: #fff;
    font-variant-numeric: tabular-nums;
  }
}

.stats__reset {
  padding: 2px 8px;
  border: 1px solid rgba(255, 255, 255, 0.2);
  border-radius: 999px;
  background: transparent;
  color: rgba(255, 255, 255, 0.8);
  cursor: pointer;
}

.stats__table {
  display: grid;
  grid-template-columns: repeat(2, 1fr);
  gap: 4px 14px;
}

.stats__row {
  display: flex;
  align-items: center;
  gap: 6px;
}

.stats__token {
  width: 22px;
  height: auto;
  border-radius: 50%;
  border: 1px solid rgba(255, 255, 255, 0.3);
}

.stats__multiplier {
  color: rgba(255, 255, 255, 0.5);
  font-variant-numeric: tabular-nums;
}

.stats__value {
  margin-left: auto;
  font-weight: 600;
  font-variant-numeric: tabular-nums;
}

.stats__caveat {
  margin: 0;
  color: rgba(255, 255, 255, 0.5);
  font-size: 10px;
  line-height: 1.4;
}

.token-slot {
  display: flex;
  flex-direction: column;
  align-items: center;
  gap: 4px;

  /* with the row bottom-aligned, this is what lifts the token off the baseline */
  img {
    margin-bottom: 9px;
  }
}

/* the shared pill, scaled down and dialled back so it reads as an annotation */
.token-slot__value {
  min-width: 0;
  /* symmetric vertical padding on a 1.0 line box centres the glyphs */
  padding: 2px 5px;
  border-color: rgba(255, 255, 255, 0.12);
  background: rgba(0, 0, 0, 0.3);
  box-shadow: none;
  color: rgba(255, 255, 255, 0.6);
  font-size: 8px;
  font-weight: 400;
  line-height: 1;
}

.token-preview--debug .token-slot {
  cursor: pointer;
  img {
    border-color: var(--select);
  }
}

</style>

<style>
.v-popper--theme-chaos-bag-stats-popover .v-popper__inner {
  background: rgba(15, 15, 20, 0.94);
  backdrop-filter: blur(8px);
  border: 1px solid rgba(255, 255, 255, 0.12);
  border-radius: 10px;
  color: #fff;
  box-shadow: 0 8px 30px rgba(0, 0, 0, 0.5);
}

.v-popper--theme-chaos-bag-stats-popover .v-popper__arrow-outer {
  border-color: rgba(255, 255, 255, 0.12);
}

.v-popper--theme-chaos-bag-stats-popover .v-popper__arrow-inner {
  border-color: rgba(15, 15, 20, 0.94);
}
</style>
