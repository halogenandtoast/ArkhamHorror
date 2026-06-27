<script lang="ts" setup>
import { useSettings } from '@/stores/settings';
import { storeToRefs } from 'pinia';
import { onUnmounted, onMounted, computed, inject, ref, watch } from 'vue'
import type { Ref } from 'vue'
import Draggable from '@/components/Draggable.vue';
import CardView from '@/arkham/components/Card.vue';
import Modifiers from '@/arkham/components/Modifiers.vue';
import PendingDamageTokens from '@/arkham/components/PendingDamageTokens.vue';
import AiTargetMenu from '@/arkham/components/AiTargetMenu.vue';
import { useDebug } from '@/arkham/debug'
import { useAi } from '@/arkham/ai'
import { ForwardIcon, PaperClipIcon } from '@heroicons/vue/20/solid'
import type { Game } from '@/arkham/types/Game'
import { imgsrc } from '@/arkham/helpers'
import { cardArt, cardImage, portraitImage, sourceCardCode } from '@/arkham/cardImages'
import * as Arkham from '@/arkham/types/Investigator'
import type { AbilityLabel, AbilityMessage, Message } from '@/arkham/types/Message'
import { MessageType } from '@/arkham/types/Message'
import { cardId, toCardContents } from '@/arkham/types/Card'
import Token from '@/arkham/components/Token.vue';
import AbilityButton from '@/arkham/components/AbilityButton.vue'
import { useMenu } from '@/composable/menu';
import { useI18n } from 'vue-i18n';
import useEmitter from '@/composable/useEmitter';
import useHighlighter from '@/composable/useHighlighter';
import Resources from '@/arkham/components/Resources.vue';
import Draw from '@/arkham/components/Draw.vue';
import { IsMobile } from '@/arkham/isMobile';
const { t } = useI18n();

export interface Props {
  choices: readonly Message[]
  investigator: Arkham.Investigator
  playerId: string
  game: Game
  portrait?: boolean
}

const props = withDefaults(defineProps<Props>(), { portrait: false })
const emit = defineEmits(['showCards', 'hideCards', 'choose'])

const id = computed(() => props.investigator.id)
const highlighter = useHighlighter()
const isHighlighted = computed(() => highlighter.highlighted.value === props.investigator.id)
const isAttackTarget = computed(() => props.game.enemyAttackTargets.some((e) => e.target.contents === props.investigator.id))
const debug = useDebug()
const choose = (idx: number) => emit('choose', idx)

const ai = useAi()
const aiMenuOpen = ref(false)
const frame = ref<HTMLElement | null>(null)
const aiTarget = computed(() => ({ tag: 'InvestigatorTarget', contents: id.value }))

// In "AI targeting mode" a click opens the directive menu instead of selecting
// the investigator; normal play is untouched when targeting is off.
function clicked() {
  if (ai.targeting) {
    aiMenuOpen.value = true
    return
  }
  emit('choose', investigatorAction.value)
}

const { addEntry, removeEntry } = useMenu()
const settingsStore = useSettings()
const { toggleShowBonded } = settingsStore
const { showBonded } = storeToRefs(settingsStore)
const { isMobile } = IsMobile();

const doShowBonded = computed(() => {
  return showBonded.value && props.playerId == props.investigator.playerId
})

watch(() => props.playerId, () => {
  if (!props.portrait) {
    if (props.playerId == props.investigator.playerId) {
      addEntry({
        id: `viewBonded-${props.investigator.playerId}`,
        icon: PaperClipIcon,
        content: t('gameBar.viewBonded'),
        shortcut: "b",
        nested: 'view',
        action: () => toggleShowBonded()
      })
    } else {
      removeEntry(`viewBonded-${props.investigator.playerId}`)
    }
  }
}, { immediate: true })

function canActivateAbility(c: Message): boolean {
  if (c.tag  === MessageType.ABILITY_LABEL) {
    if ("contents" in c.ability.source) {
      return c.ability.source.contents === id.value
    }
  }
  return false
}
const activateAbilityAction = computed(() => props.choices.findIndex(canActivateAbility))

const labelAction = computed(() => {
  return props.choices
    .findIndex((c) => c.tag === MessageType.TARGET_LABEL
      && c.target.tag === "InvestigatorTarget" && c.target.contents === id.value)
})

const investigatorAction = computed(() => {
  if (labelAction.value !== -1) {
    return labelAction.value
  }

  return activateAbilityAction.value
})

const choices = computed(() => props.choices)

function isAbility(v: Message): v is AbilityLabel {
  if (v.tag !== MessageType.ABILITY_LABEL) {
    return false
  }

  const { source } = v.ability;

  if (source.sourceTag === 'ProxySource') {
    if ("contents" in source.source) {
      return source.source.contents === id.value
    }
  } else if (source.tag === 'InvestigatorSource') {
    return source.contents === id.value
  }

  return false
}

const abilities = computed(() => {
  return choices
    .value
    .reduce<AbilityMessage[]>((acc, v, i) => {
      if (isAbility(v)) {
        return [...acc, { contents: v, displayAsAction: false, index: i }];
      }

      return acc;
    }, []);
})

const endTurnAction = computed(() => {
  return props.choices
    .findIndex((c) => c.tag === MessageType.END_TURN_BUTTON && c.investigatorId === id.value);
})

const skipTriggersAction = computed(() => {
  return props.choices
    .findIndex((c) => c.tag === MessageType.SKIP_TRIGGERS_BUTTON && c.investigatorId === id.value);
})

const skipAllTriggers = inject<(() => void)>('skipAllTriggers')
const skipAllAvailable = inject<Ref<boolean>>('skipAllAvailable')
const skipAllInProgress = inject<Ref<boolean>>('skipAllInProgress')
const solo = inject<Ref<boolean>>('solo')
const isCurrentPlayersInvestigator = computed(() => props.investigator.playerId === props.playerId)
const showSkipAll = computed(() => {
  if (solo?.value === true) {
    return skipTriggersAction.value !== -1 && skipAllAvailable?.value === true
  }

  return isCurrentPlayersInvestigator.value && skipAllAvailable?.value === true
})
// "Skip Triggers" skips only the current player's own window; it is greyed out
// when this investigator has no window of their own. "Skip All" (showSkipAll) is
// a separate button that handles the other players' windows.
const canSkipTriggers = computed(() => skipTriggersAction.value !== -1)

function skipTriggers() {
  if (skipTriggersAction.value === -1) return
  emit('choose', skipTriggersAction.value)
}

const investigatorClass = computed(() => {
  return ['c03006', 'c90087'].includes(props.investigator.cardCode) && props.investigator.meta !== 'Neutral' ? (props.investigator.meta ?? props.investigator.class) : props.investigator.class
})

const image = computed(() => {
  if (props.investigator.form.tag === 'YithianForm') {
    return imgsrc("cards/04244.avif");
  }

  if (props.investigator.form.tag === 'HomunculusForm') {
    return imgsrc("cards/11068b.avif");
  }

  if (props.investigator.form.tag === 'ShatteredForm') {
    return imgsrc("cards/10661.avif");
  }

  if (props.investigator.form.tag === "TransfiguredForm") {
    return cardImage(props.investigator.form.contents)
  }

  const mutated = props.investigator.mutated ? `_${props.investigator.mutated}` : ''
  const classVariant = ['c03006', 'c90087'].includes(props.investigator.cardCode) && props.investigator.meta !== 'Neutral' ? (props.investigator.meta ? `_${props.investigator.meta}` : '') : ''
  return cardImage(props.investigator.art, `${classVariant}${mutated}`)
})

const investigatorPortraitImage = computed(() => {
  const suffix = props.investigator.endedTurn ? 'b' : ''
  if (props.investigator.form.tag === "YithianForm" || props.investigator.form.tag === "HomunculusForm" || props.investigator.form.tag === "ShatteredForm") {
    return portraitImage(id.value, suffix)
  }

  return portraitImage(props.investigator.cardCode, suffix)
})

const miniCardDevoured = computed(() => {
  const devouredMiniCards = props.game.scenario?.meta?.devouredMiniCards
  return Array.isArray(devouredMiniCards) && devouredMiniCards.includes(id.value)
})

const replacementMiniCardInitials = computed(() => {
  const name = props.investigator.name.title
    .replace(/["“”']/g, '')
    .replace(/\([^)]*\)/g, '')
    .trim()
  const words = name.split(/\s+/).filter(Boolean)
  if (words.length === 0) return '?'
  if (words.length === 1) return words[0].slice(0, 2).toUpperCase()
  return `${words[0][0]}${words[words.length - 1][0]}`.toUpperCase()
})

const replacementMiniCardStyle = computed(() => ({
  '--replacement-class-color': `var(--${investigatorClass.value.toLowerCase()})`,
}))

const portraitClasses = computed(() => ({
  'investigator--can-interact--portrait': investigatorAction.value !== -1,
  ethereal: ethereal.value,
  dragging: dragging.value,
  captured: captured.value,
  'ai-target-hover': ai.targeting,
}))

const emitter = useEmitter()
const cardsUnderneath = computed(() => props.investigator.cardsUnderneath)
const cardsUnderneathLabel = computed(() => t('investigator.underneathCards', {count: cardsUnderneath.value.length}))
const devoured = computed(() => props.investigator.devoured)

onMounted(() => {
  emitter.on('showUnder', (id: string) => {
    if (id === props.investigator.id) {
      showCardsUnderneath(new Event('click'))
    }
  })
})

onUnmounted(() => {
  if (!props.portrait) removeEntry(`viewBonded-${props.investigator.playerId}`)
  emitter.off('showUnder')
})


const showCardsUnderneath = (e: Event) => emit('showCards', e, cardsUnderneath, "Cards Underneath", false)
const showDevoured = (e: Event) => emit('showCards', e, devoured, "Devoured", false)
const forcedShowUnder = ref(false)

watch(() => choices.value, () => {
  const isUnderChoice = (c: Message) => {
    if (c.tag !== "TargetLabel") return false
    if (c.target.tag !== "CardIdTarget") return false
    return props.investigator.cardsUnderneath.some(card => cardId(card) === c.target.contents)
  }
  const showUnder = choices.value.some(isUnderChoice)
  if (showUnder) {
    showCardsUnderneath(new Event('click'))
    forcedShowUnder.value = true
  } else {
    emit('hideCards')
    forcedShowUnder.value = false
  }
})

const modifiers = computed(() => props.investigator.modifiers)

const blankedModifier = computed(() => {
  return modifiers.value?.find(
    (m) =>
      m.type.tag === "Blank"
      || (m.type.tag === "OtherModifier" && m.type.contents === "Blank")
  ) ?? null
})

const isBlanked = computed(() => blankedModifier.value !== null)

const blankedCardCode = computed<string | null>(() => {
  const m = blankedModifier.value
  if (!m) return null
  if (m.card) return cardArt(toCardContents(m.card).cardCode)
  return sourceCardCode(m.source, props.game)
})

const captured = computed(() => {
  return modifiers.value?.some((m) => m.type.tag === "ScenarioModifier" && m.type.contents === "captured") ?? false
})

const ethereal = computed(() => {
  return modifiers.value?.some((m) => m.type.tag === "UIModifier" && m.type.contents === "Ethereal") ?? false
})

// While taking an immediate (granted) action there is no fast player window, so
// fast/free abilities can't be used. The engine marks this with AsIfTurn (see
// handlePlayerWindow), which reaches the client as an OtherModifier.
// Once the game is resolving an action (gameInAction), the choice has already
// been made, so there's nothing to warn about — hide the indicator then.
const isTakingImmediateAction = computed(() => {
  if (props.game.inAction) return false
  return modifiers.value?.some(
    (m) => m.type.tag === "OtherModifier" && m.type.contents === "AsIfTurn"
  ) ?? false
})

function useEffectAction(action: { contents: string[] }) {
  const choice = choices.value.findIndex((c) => c.tag === 'EffectActionButton' && c.effectId == action.contents[1])
  if (choice !== -1) {
    emit('choose', choice)
  }
}

function isActiveEffectAction(action: { tag?: "EffectAction"; contents: any }) {
  const choice = choices.value.findIndex((c) => c.tag === 'EffectActionButton' && c.effectId == action.contents[1])
  return choice !== -1
}

const skills = computed(() => {
  const st = props.game.skillTest
  const activeSkills = st ? new Set(st.skills) : null
  const action = st?.action

  const base = {
    SkillWillpower: props.investigator.willpower,
    SkillIntellect: props.investigator.intellect,
    SkillCombat: props.investigator.combat,
    SkillAgility: props.investigator.agility,
  }

  const baseOverride: Record<string, number|undefined> = {}
  const plus: Record<string, number> = {
    SkillWillpower: 0, SkillIntellect: 0, SkillCombat: 0, SkillAgility: 0
  }
  const finalSet: Record<string, number|undefined> = {}

  for (const m of (props.investigator.modifiers ?? [])) {
    const t = m.type
    switch (t.tag) {
      case 'BaseSkillOf':
        baseOverride[t.skillType] = t.value
        break
      case 'BaseSkill':
        if (activeSkills)
          for (const k of activeSkills) baseOverride[k] = t.contents
        break
      case 'SkillModifier':
        plus[t.skillType] += t.value
        break
      case 'AnySkillValue':
        if (activeSkills) for (const k of activeSkills) plus[k] += t.contents
        break
      case 'ActionSkillModifier':
        if (t.action === action) plus[t.skillType] += t.value
        break
      case 'SetSkillValue':
        finalSet[t.skillType] = t.value
        break
    }
  }

  const calc = (k:'SkillWillpower'|'SkillIntellect'|'SkillCombat'|'SkillAgility') =>
    Math.max(0, finalSet[k] ?? ((baseOverride[k] ?? base[k]) + plus[k]))

  return {
    willpower: calc('SkillWillpower'),
    intellect: calc('SkillIntellect'),
    combat: calc('SkillCombat'),
    agility: calc('SkillAgility'),
  }
})

const willpower = computed(() => skills.value.willpower)
const intellect = computed(() => skills.value.intellect)
const combat = computed(() => skills.value.combat)
const agility = computed(() => skills.value.agility)

const dragging = ref(false)
const showModifiers = ref(false)
function startDrag(event: DragEvent) {
  dragging.value = true
  if (event.dataTransfer) {
    event.dataTransfer.effectAllowed = 'move'
    event.dataTransfer.setData('text/plain', JSON.stringify({ "tag": "InvestigatorTarget", "contents": id.value }))
  }
}

function endDrag() { dragging.value = false }

const dragover = (e: DragEvent) => {
  e.preventDefault()
  if (e.dataTransfer) {
    e.dataTransfer.dropEffect = 'move'
  }
}

function onDrop(event: DragEvent) {
  event.preventDefault()
  if (event.dataTransfer) {
    const data = event.dataTransfer.getData('text/plain')
    if (data) {
      const json = JSON.parse(data)
      if (json.tag === "KeyTarget") {
        debug.send(props.game.id, {tag: 'PlaceKey', contents: [{ tag: "InvestigatorTarget", contents: id.value }, json.contents]})
      }

      if (json.tag === "EnemyTarget") {
        debug.send(props.game.id, {tag: 'EngageMessage', contents: {tag: 'EnemyEngageInvestigator_', contents: [json.contents, id.value]}})
      }
    }
  }
}

const heartInjury = computed(() => {
  return modifiers.value?.some((m) => m.type.tag === "ScenarioModifier" && m.type.contents === "heartInjury") ?? false
})

const diamondInjury = computed(() => {
  return modifiers.value?.some((m) => m.type.tag === "ScenarioModifier" && m.type.contents === "diamondInjury") ?? false
})

const clubInjury = computed(() => {
  return modifiers.value?.some((m) => m.type.tag === "ScenarioModifier" && m.type.contents === "clubInjury") ?? false
})

const spadeInjury = computed(() => {
  return modifiers.value?.some((m) => m.type.tag === "ScenarioModifier" && m.type.contents === "spadeInjury") ?? false
})
</script>

<template>
  <div v-if="portrait" class="portrait-container" ref="frame">
    <span v-if="isMobile">
      <i class="action" v-for="n in investigator.remainingActions" :key="n"></i>
      <template v-for="action in investigator.additionalActions" :key="action">
        <button @click="useEffectAction(action)" v-if="action.tag === 'EffectAction'" v-tooltip="action.contents[0]" :class="[{ activeButton: isActiveEffectAction(action)}, `${investigatorClass.toLowerCase()}ActionButton`]">
          <i class="action"></i>
        </button>
        <i v-else class="action" :class="`${investigatorClass.toLowerCase()}Action`"></i>
      </template>
    </span>
    <span
      v-if="isMobile && isTakingImmediateAction"
      class="no-free-abilities"
      v-tooltip="{ content: $t('investigator.freeAbilitiesUnavailable'), html: true }"
    >
      <span class="fast-icon"></span>
      <svg class="no-sign" viewBox="0 0 24 24" aria-hidden="true">
        <circle cx="12" cy="12" r="10" />
        <line x1="5" y1="5" x2="19" y2="19" />
      </svg>
    </span>
    <div
      v-if="miniCardDevoured"
      class="portrait portrait--replacement-marker portrait--devoured-mini-card"
      :class="portraitClasses"
      :style="replacementMiniCardStyle"
      :draggable="debug.active"
      v-tooltip="investigator.name.title"
      @click="clicked"
      @dragstart="startDrag($event)"
      @dragstop="endDrag"
      @drop="onDrop($event)"
      @dragover.prevent="dragover($event)"
      @dragenter.prevent
    >
      {{ replacementMiniCardInitials }}
      <img class="portrait--blob-overlay" :src="imgsrc('extra/the-blob-that-ate-everything/blob-overlay.png')" alt="" aria-hidden="true" />
    </div>
    <img
      v-else
      :src="investigatorPortraitImage"
      class="portrait"
      :class="portraitClasses"
      :draggable="debug.active"
      @click="clicked"
      @dragstart="startDrag($event)"
      @dragstop="endDrag"
      @drop="onDrop($event)"
      @dragover.prevent="dragover($event)"
      @dragenter.prevent
    />
  </div>
  <div v-else class="player-container">
    <div class="player-area">
      <div class="player-card">
        <div class="stats">
          <div class="willpower willpower-icon">{{willpower}}</div>
          <div class="intellect intellect-icon">{{intellect}}</div>
          <div class="combat combat-icon">{{combat}}</div>
          <div class="agility agility-icon">{{agility}}</div>
        </div>
        <div class="investigator-image" ref="frame">
          <img
            :class="{ 'investigator--can-interact': investigatorAction !== -1, 'ability-target': isHighlighted || isAttackTarget, 'ai-target-hover': ai.targeting }"
            class="card card--sideways"
            :src="image"
            @click="clicked"
            @drop="onDrop($event)"
            @dragover.prevent="dragover($event)"
            @dragenter.prevent
          />
          <span v-if="isBlanked" class="blanked-badge" :data-image-id="blankedCardCode"><font-awesome-icon icon="ban" /></span>
          <PendingDamageTokens
            v-if="!portrait"
            :game="game"
            :playerId="investigator.playerId"
            class="investigator-pending-tokens"
          />
          <Token v-for="sealedToken in investigator.sealedChaosTokens" :key="sealedToken.id" :token="sealedToken" :playerId="playerId" :game="game" @choose="choose" class="sealed" />
        </div>
      </div>
      <div>
        <div class="player-buttons">
          <div class="button-group" :class="{ 'button-group--skip-all-pending': isCurrentPlayersInvestigator && skipAllInProgress }">
            <span v-if="!isMobile" class="action-container">
              <i class="spade" v-if="spadeInjury"></i>
              <i class="heart" v-if="heartInjury"></i>
              <i class="diamond" v-if="diamondInjury"></i>
              <i class="club" v-if="clubInjury"></i>
              <i class="action" v-for="n in investigator.remainingActions" :key="n"></i>
              <template v-for="action in investigator.additionalActions" :key="action">
                <button @click="useEffectAction(action)" v-if="action.tag === 'EffectAction'" v-tooltip="action.contents[0]" :class="[{ activeButton: isActiveEffectAction(action)}, `${investigatorClass.toLowerCase()}ActionButton`]">
                  <i class="action"></i>
                </button>
                <i v-else class="action" :class="`${investigatorClass.toLowerCase()}Action`"></i>
              </template>
              <span
                v-if="isTakingImmediateAction"
                class="no-free-abilities"
                v-tooltip="{ content: $t('investigator.freeAbilitiesUnavailable'), html: true }"
              >
                <span class="fast-icon"></span>
                <svg class="no-sign" viewBox="0 0 24 24" aria-hidden="true">
                  <circle cx="12" cy="12" r="10" />
                  <line x1="5" y1="5" x2="19" y2="19" />
                </svg>
              </span>
            </span>
            <template v-if="debug.active">
              <button
                @click.exact="debug.send(game.id, {tag: 'GainActions', contents: [id, {tag: 'TestSource', contents: []}, 1]})"
                @click.shift="debug.send(game.id, {tag: 'GainActions', contents: [id, {tag: 'TestSource', contents: []}, 5]})"
              >+</button>
            </template>
            <AbilityButton
              v-for="ability in abilities"
              :key="ability.index"
              :ability="ability.contents"
              :game="game"
              @click="$emit('choose', ability.index)"
              />
            <button
            :class="{ active: endTurnAction !== -1 && investigator.remainingActions === 0 }"
            :disabled="endTurnAction == -1"
            @click="$emit('choose', endTurnAction)"
            >{{ isMobile ? $t('investigator.endTurnShort') : $t('investigator.endTurn') }}</button>

            <button
              v-if="devoured && devoured.length > 0"
              @click="showDevoured"
            >{{ $t('investigator.devouredCards', {count: devoured.length}) }}</button>

            <span class="skip-triggers-group" :class="{ 'skip-triggers-group--paired': showSkipAll }">
              <button
                :disabled="!canSkipTriggers || skipAllInProgress"
                @click="skipTriggers"
                class="skip-triggers-button"
              >{{ isMobile ? t('skip') : $t('investigator.skipTriggers') }}</button>
              <button
                v-if="showSkipAll"
                @click="skipAllTriggers && skipAllTriggers()"
                class="skip-all-triggers-button"
                v-tooltip="$t('investigator.skipAllTriggers')"
                :aria-label="$t('investigator.skipAllTriggers')"
              ><ForwardIcon class="skip-all-triggers-icon" aria-hidden="true" /></button>
            </span>

            <button
              v-if="debug && debug.active && (investigator.modifiers ?? []).length > 0"
              @click="showModifiers = true"
              >{{ $t('investigatorRow.showModifiers') }}</button>

            <Modifiers v-if="investigator.modifiers && showModifiers" :game="game" :modifiers="investigator.modifiers" @close="showModifiers = false" />

            <button v-if="cardsUnderneath.length > 0" class="view-discard-button" @click="showCardsUnderneath">{{cardsUnderneathLabel}}</button>
          </div>
          <Draw
            v-if="isMobile"
            :game="game"
            :playerId="playerId"
            :investigator="investigator"
            @choose="$emit('choose', $event)"
          />
        </div>
        <Resources
          v-if="isMobile"
          :game="game"
          :investigator="investigator"
          :choices="choices"
          :playerId="playerId"
          @choose="$emit('choose', $event)"
        />
      </div>
    </div>
    <Resources
      v-if="!isMobile"
      :game="game"
      :investigator="investigator"
      :choices="choices"
      :playerId="playerId"
      @choose="$emit('choose', $event)"
    />

    <Draggable v-if="doShowBonded">
      <template #handle><header><h2>{{$t('gameBar.bonded')}}</h2></header></template>
      <div class="card-row-cards">
        <div v-for="card in investigator.bondedCards" :key="cardId(card)" class="card-row-card">
          <CardView :game="game" :card="card" :playerId="playerId" />
        </div>
      </div>
      <button class="close button" @click="toggleShowBonded">{{$t('close')}}</button>
    </Draggable>
  </div>
  <AiTargetMenu
    v-model="aiMenuOpen"
    :frame="frame"
    kind="investigator"
    :target="aiTarget"
    :seat="ai.selectedSeat"
    :game-id="game.id"
  />
</template>

<style scoped>
i.action {
  font-family: 'Arkham';
  font-style: normal;
  font-weight: normal;
  font-variant: normal;
  text-transform: none;
  line-height: 1;
  -webkit-font-smoothing: antialiased;
  position: relative;
  color: #EEE;

  &:before {
    font-family: "Arkham";
    content: "\0049";
  }
}

/* "no free abilities" indicator: the fast/free trigger glyph with a red
   prohibition slash, shown while taking an immediate (granted) action. */
.no-free-abilities {
  position: relative;
  display: inline-flex;
  align-items: center;
  margin-left: 6px;
  cursor: help;
  line-height: 1;
}

.no-free-abilities :deep(.fast-icon) {
  font-size: 15px;
  color: #cfcfd6;
}

.no-free-abilities :deep(.fast-icon)::before {
  margin-right: 0;
}

.no-free-abilities .no-sign {
  position: absolute;
  left: 50%;
  top: 50%;
  width: 20px;
  height: 20px;
  transform: translate(-55%, -50%);
  pointer-events: none;
}

.no-free-abilities .no-sign circle,
.no-free-abilities .no-sign line {
  fill: none;
  stroke: #e0454d;
  stroke-width: 2.2;
}

.turn-info {
  display: flex;
  align-self: center;
  align-items: center;
}

.investigator--can-interact {
  border: 2px solid var(--select);
  border-radius: 2px;
  cursor: pointer;
}


.investigator--can-interact--portrait {
  cursor: pointer;
  border: 3px solid var(--select);
}

.card {
  width: auto;
  height: var(--card-width);
}

.guardianAction {
  color: var(--guardian-extra-dark) !important;
}

.survivorAction {
  color: var(--survivor-extra-dark) !important;
}

.mysticAction {
  color: var(--mystic-extra-dark) !important;
}

.seekerAction {
  color: var(--seeker-extra-dark) !important;
}

.rogueAction {
  color: var(--rogue-extra-dark) !important;
}

.neutralAction {
  color: var(--neutral) !important;
}

.guardianActionButton {
  background-color: var(--guardian) !important;
  border: 0;
  border-radius: 2px;
  margin: 0 2px;
}

.seekerActionButton {
  background-color: var(--seeker) !important;
  border: 0;
  border-radius: 2px;
  margin: 0 2px;
}

.rogueActionButton {
  background-color: var(--rogue) !important;
  border: 0;
  border-radius: 2px;
  margin: 0 2px;
}

.mysticActionButton {
  background-color: var(--mystic) !important;
  border: 0;
  border-radius: 2px;
  margin: 0 2px;
}

.survivorActionButton {
  background-color: var(--survivor) !important;
  border: 0;
  border-radius: 2px;
  margin: 0 2px;
}

.neutralActionButton {
  background-color: var(--neutral) !important;
  border: 0;
  border-radius: 2px;
  margin: 0 2px;
}

.player-card {
  display: flex;
  flex-direction: column;
  align-items: flex-start;
  gap: 2px;
  width: min-content;

  @media (max-width: 800px) and (orientation: portrait) {
    width: 48%;
    display: flex;
    flex-direction: row;
    gap: 2px;

    :deep(.card) {
      width: auto;
      height: calc(var(--card-width) * 3);
    }
  }
}

.portrait-container{
  position: relative;
  display: flex;
  justify-content: center;
  align-items: center;
  flex-direction: column;
  overflow: visible;
  :deep(span) {
    height: 0.87rem;
    overflow: visible;
    z-index: var(--z-index-10);
  }
  :deep(.action) {
    font-size: 0.35rem;
  }
}

.portrait {
  border-radius: 3px;
  width: calc(var(--card-width) * 0.6);
}

.portrait--replacement-marker {
  aspect-ratio: 2 / 3;
  border: 2px dashed color-mix(in srgb, var(--replacement-class-color) 70%, white);
  background:
    linear-gradient(135deg, color-mix(in srgb, var(--replacement-class-color) 82%, black), var(--replacement-class-color));
  color: white;
  display: flex;
  align-items: center;
  justify-content: center;
  font-size: calc(var(--card-width) * 0.24);
  font-weight: 800;
  letter-spacing: 0.04em;
  text-shadow: 0 1px 2px rgb(0 0 0 / 75%);
  box-shadow: inset 0 0 0 1px rgb(255 255 255 / 20%);
  box-sizing: border-box;
  user-select: none;
}

.portrait--devoured-mini-card {
  position: relative;
  overflow: visible;
}

.portrait--blob-overlay {
  position: absolute;
  inset: -2px;
  width: calc(100% + 4px);
  height: calc(100% + 4px);
  border-radius: inherit;
  pointer-events: none;
}

.supplies {
  & ul {
    display: flex;
    flex-direction: row;
    list-style: none;
  }
}

.stats {
  display: inline-flex;
  width: max-content;
  border-radius: 5px;
  overflow: hidden;

  > div {
    position: relative;
    display: flex;
    align-items: center;
    justify-content: center;
    gap: 0.06rem;
    padding-inline: 0.36rem;
    white-space: nowrap;
  }

  > div + div::after {
    content: "";
    position: absolute;
    left: 0;
    top: 0;
    bottom: 0;
    width: 1px;
    background: rgba(255, 255, 255, 0.22);
  }

  @media (max-width: 800px) and (orientation: portrait) {
    display: flex;
    flex-direction: column;
    width: max-content;

    > div {
      padding-inline: 0.36rem;
    }

    > div + div::after {
      left: 0;
      right: 0;
      top: 0;
      bottom: auto;
      width: auto;
      height: 1px;
    }
  }

}

.willpower {
  background-color: var(--guardian-dark);
  color: white;
  text-align: center;
  border-top-left-radius: 5px;

  @media (max-width: 800px) and (orientation: portrait) {
    border-top-right-radius: 5px;
  }
}

.intellect {
  background-color: var(--mystic-dark);
  color: white;
  text-align: center;
}

.combat {
  background-color: var(--survivor-dark);
  color: white;
  text-align: center;
}

.agility {
  background-color: var(--rogue-dark);
  color: white;
  text-align: center;
  border-top-right-radius: 5px;

  @media (max-width: 800px) and (orientation: portrait) {
    border-top-right-radius: 0;
    border-bottom-left-radius: 5px;
    border-bottom-right-radius: 5px;
  }
}

.activeButton {
  border: 1px solid var(--select);
}

@keyframes become-ghost {
  100% {
    filter: drop-shadow(0px 0 20px #FF0099) invert(75%);
  }
}

@keyframes ghost {
  0% {
    filter: drop-shadow(0px 0 20px #FF0099) invert(75%);
  }

  50% {
    filter: drop-shadow(0px 0 10px #FF0099) invert(70%);
  }

  100% {
    filter: drop-shadow(0px 0 20px #FF0099) invert(75%);
  }
}

@keyframes rotate {
  100% {
    transform: rotate(-360deg);
  }
}

.ethereal {
  height: fit-content;
  will-change: filter;
  transition: filter .2s ease-out;
  animation-direction: forwards;
  animation: become-ghost 1s linear, ghost 3s linear 1s infinite;
}

.player-container{
  display: flex;
  flex-direction: column;
  gap: 2px;

  @media (max-width: 800px) and (orientation: portrait) {
    width: 100%;
  }
}

.player-area {
  display: flex;
}

.button-group {
  display: flex;
  flex-direction: column;
  gap: 2px;
  @media (max-width: 800px) and (orientation: portrait) {
    flex-direction: column;
    align-items: flex-start;
    gap: 4px;
    :deep(button) {
      width: 100%;
      height: fit-content;
      font-size: small;
    }
  }
}

.button-group--skip-all-pending > :not(.skip-triggers-group) {
  opacity: 0.35;
  filter: grayscale(1);
  pointer-events: none;
}

.button-group--skip-all-pending .skip-triggers-button {
  opacity: 0.55;
  pointer-events: none;
}

.player-buttons {
  margin-left: 10px;
  display: flex;
  gap: 2px;
  flex-direction: column;

  @media (prefers-color-scheme: light) {
    color: #efefef;
  }
  @media (max-width: 800px) and (orientation: portrait) {
    margin-left: 0;
    flex-direction: row;
    align-items: flex-start;
    gap: 8px;
    :deep(img) {
      width: calc(var(--pool-token-width) * 1.2);
    }
  }
}

.skip-triggers-group {
  display: inline-flex;
  align-items: stretch;
}

.skip-triggers-button {
  transition: all 0.2s ease-in;
  background-color: var(--select);
  color: white;
  border: 0;
  border-radius: 2px;

  &[disabled] {
    background-color: #999;
    color: #666;
  }

  &:not([disabled]):hover {
    background-color: var(--select-dark);
  }
}

.skip-triggers-group--paired .skip-triggers-button {
  border-radius: 2px 0 0 2px;
}

.skip-all-triggers-button {
  transition: all 0.2s ease-in;
  background-color: var(--select);
  color: white;
  border: 0;
  border-left: 1px solid rgba(0, 0, 0, 0.25);
  border-radius: 0 2px 2px 0;
  padding-inline: 6px;
  display: inline-flex;
  align-items: center;
  justify-content: center;
  cursor: pointer;

  &:hover {
    background-color: var(--select-dark);
  }
}

.skip-all-triggers-icon {
  width: 14px;
  height: 14px;
}

.investigator-pending-tokens {
  position: absolute;
  bottom: 6px;
  left: 6px;
  z-index: var(--z-index-10);
  pointer-events: none;
}

.investigator-image {
  position: relative;
  align-self: stretch;
  min-width: 0;

  > .card {
    display: block;
    width: 100%;
    min-width: 0;
    height: auto;
    border-radius: 5px;
  }

  @media (max-width: 800px) and (orientation: portrait) {
    width: auto;

    > .card {
      width: auto;
      min-width: 0;
      height: calc(var(--card-width) * 3);
    }
  }
}

.blanked-badge {
  position: absolute;
  right: 6px;
  bottom: 22px;
  width: 24px;
  height: 24px;
  color: #e05252;
  filter: drop-shadow(0 1px 4px rgba(0,0,0,0.7));
  cursor: default;
  z-index: var(--z-index-2);
  display: flex;
  align-items: center;
  justify-content: center;
}

img.card {
  transition: box-shadow 120ms ease;
}

img.card.ability-target {
  box-shadow: 0 0 0 2px var(--highlight), 0 0 6px 1px var(--highlight), var(--card-shadow);
}

/* Dev-only "AI targeting mode": class is only bound while targeting is on, so
   normal play is untouched. Green border + pale green wash on hover. Applies to
   both the full investigator card and the small portrait. */
.ai-target-hover {
  cursor: pointer;
  transition: box-shadow 120ms ease, filter 120ms ease;
}

.ai-target-hover:hover {
  border: 2px solid var(--ai-target);
  border-radius: 3px;
  box-shadow: 0 0 0 2px var(--ai-target), 0 0 12px 3px rgba(74, 222, 128, 0.55);
  filter: brightness(1.05) sepia(0.35) hue-rotate(55deg) saturate(1.3);
}

.card-row-cards {
  display: flex;
  flex-direction: row;
  gap: 5px;
  flex-wrap: wrap;
  padding: 10px;
}

.close {
  width: 100%;
  background: var(--button-2);
  display: inline;
  border: 0;
  color: white;
  padding: 0.5em;
  text-transform: uppercase;

  &:hover {
    background: var(--button-2-highlight);
  }
}

.sealed {
  position: absolute;
  width: calc(var(--card-width) / 2);
  left: 0;
  top: calc(var(--card-width) / 2);
}

.captured {
  rotate: 90deg;
}

button.active {
  background-color: var(--select-dark-20);
  border-color: var(--select-dark-20);
  border-radius: 2px;
  border-style: solid;
  color: white;
}

i.spade {
  font-style: normal;
  font-weight: normal;
  font-variant: normal;
  text-transform: none;
  line-height: 1;
  -webkit-font-smoothing: antialiased;
  position: relative;
  color: #EEE;
  margin-right: 4px;
  filter: drop-shadow(0 0 2px white);

  &:before {
    content: "♠️";
  }
}

i.club {
  font-style: normal;
  font-weight: normal;
  font-variant: normal;
  text-transform: none;
  line-height: 1;
  -webkit-font-smoothing: antialiased;
  position: relative;
  color: #EEE;
  margin-right: 4px;
  filter: drop-shadow(0 0 2px white);

  &:before {
    content: "♣️";
  }
}

i.heart {
  font-style: normal;
  font-weight: normal;
  font-variant: normal;
  text-transform: none;
  line-height: 1;
  -webkit-font-smoothing: antialiased;
  position: relative;
  color: #EEE;
  margin-right: 4px;
  filter: drop-shadow(0 0 2px white);

  &:before {
    content: "♥️";
  }
}

i.diamond {
  font-style: normal;
  font-weight: normal;
  font-variant: normal;
  text-transform: none;
  line-height: 1;
  -webkit-font-smoothing: antialiased;
  position: relative;
  color: #EEE;
  margin-right: 4px;
  filter: drop-shadow(0 0 2px white);

  &:before {
    content: "♦️";
  }
}
</style>
