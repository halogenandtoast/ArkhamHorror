<script lang="ts" setup>
import { computed, ref, watch } from 'vue'
import { useI18n } from 'vue-i18n'
import type { Game } from '@/arkham/types/Game'
import type { Location } from '@/arkham/types/Location'
import { TokenType } from '@/arkham/types/Token'
import { cardArt, cardImage } from '@/arkham/cardImages'
import { useGameChoices } from '@/arkham/composables/useGameChoices'
import { useCardNames } from '@/arkham/composables/useCardNames'
import { choicesFor, selectLocationIndex, type EntityChoice } from '@/arkham/locationChoices'
import { useDbCardStore } from '@/stores/dbCards'
import AbilityButton from '@/arkham/components/AbilityButton.vue'
import { ownInvestigator } from '@/arkham/ownInvestigator'
import HandCard from '@/arkham/components/HandCard.vue'

/* The room you are standing in, drawn as a place rather than a card: a backdrop, the investigators and
 * enemies at the location standing on its floor, and doors to the locations next to it.
 * Everything clickable only opens a menu of choices the server already offers; walking around the
 * floor is cosmetic and costs nothing. */
const props = defineProps<{
  game: Game
  playerId: string
  location: Location
  /** Locations connected to this one that are in play. */
  neighbours: Location[]
  /** Whether the viewed location is the one the investigator is at. */
  mine: boolean
}>()
const emit = defineEmits<{ (e: 'choose', index: number): void; (e: 'select', id: string): void }>()

const { t } = useI18n()
const nameOf = useCardNames()
const dbCards = useDbCardStore()
const choices = useGameChoices(() => props.game, () => props.playerId)

// Backdrops shipped in public/img/scenes, named by printed card code. Any other location falls back to its card art.
const SCENE_ART = new Set(['01111', '01112', '01113', '01114', '01115'])
const artCode = computed(() => cardArt(props.location.cardCode))
const sceneArt = computed(() => SCENE_ART.has(artCode.value) ? `img/scenes/${artCode.value}.webp` : null)
const cardArtUrl = computed(() => cardImage(props.location.cardCode, props.location.revealed ? '' : 'b'))

const clues = computed(() => props.location.tokens[TokenType.Clue] ?? 0)
const doom = computed(() => props.location.tokens[TokenType.Doom] ?? 0)

// --- who is standing here ---
type Meter = { left: number; max: number }
type Figure = {
  id: string; code: string; name: string; kind: 'investigator' | 'enemy'; x: number; y: number
  exhausted: boolean; you: boolean; note: string | null
  sprite: string | null; health: Meter | null; sanity: Meter | null
  resources: number | null; clues: number | null
}

// Cut-out standees shipped in public/img/scenes/actors, taken from the offline demo. A figure without
// one is drawn from a crop of its card art instead.
const SPRITE_BY_CODE: Record<string, string> = {
  '01001': 'roland', '01159': 'rats', '01116': 'priest', '01157': 'umordhoth', '01172': 'nightgaunt', '01175': 'nightgaunt',
}
function spriteFor(code: string): string | null {
  const art = cardArt(code)
  const named = SPRITE_BY_CODE[art]
  if (named) return `img/scenes/actors/${named}.webp`
  const traits = dbCards.getDbCard(art)?.traits ?? ''
  if (traits.includes('Cultist')) return 'img/scenes/actors/cultist.webp'
  if (traits.includes('Ghoul')) return 'img/scenes/actors/ghoul.webp'
  return null
}
const meter = (max: number, taken: number): Meter => ({ max, left: Math.max(0, max - taken) })

const ENEMY_SPOTS: [number, number][] = [[72, 80], [84, 72], [78, 90], [60, 72], [88, 88], [62, 90]]
const youAt = ref<[number, number]>([46, 82])
watch(() => props.location.id, () => { youAt.value = [46, 82]; menu.value = null })

const me = computed(() => ownInvestigator(props.game, props.playerId))
const hand = computed(() => me.value?.hand ?? [])

const figures = computed<Figure[]>(() => {
  const result: Figure[] = []
  let others = 0
  for (const id of props.location.investigators) {
    const investigator = props.game.investigators[id]
    if (!investigator) continue
    const you = id === me.value?.id
    const [x, y] = you ? youAt.value : [30 + 9 * others++, 86]
    result.push({
      id, code: investigator.cardCode, name: nameOf(investigator.cardCode), kind: 'investigator', x, y, exhausted: false, you, note: null,
      sprite: spriteFor(investigator.cardCode),
      health: meter(investigator.health, investigator.tokens[TokenType.Damage] ?? 0),
      sanity: meter(investigator.sanity, investigator.tokens[TokenType.Horror] ?? 0),
      resources: investigator.tokens[TokenType.Resource] ?? 0,
      clues: investigator.tokens[TokenType.Clue] ?? 0,
    })
  }
  let slot = 0
  for (const id of props.location.enemies) {
    const enemy = props.game.enemies[id]
    if (!enemy || enemy.placement.tag !== 'AtLocation') continue
    const [x, y] = ENEMY_SPOTS[slot++ % ENEMY_SPOTS.length]
    const damage = enemy.tokens[TokenType.Damage] ?? 0
    const maxHealth = enemy.health?.tag === 'Fixed' ? enemy.health.contents : null
    result.push({
      id, code: enemy.cardCode, name: nameOf(enemy.cardCode), kind: 'enemy', x, y, exhausted: enemy.exhausted, you: false,
      note: maxHealth === null && damage ? `♥ −${damage}` : null,
      sprite: spriteFor(enemy.cardCode), health: maxHealth === null ? null : meter(maxHealth, damage), sanity: null, resources: null, clues: null,
    })
  }
  return result
})

function walk(event: MouseEvent) {
  const floor = event.currentTarget as HTMLElement
  const rect = floor.getBoundingClientRect()
  const x = ((event.clientX - rect.left) / rect.width) * 100
  const y = ((event.clientY - rect.top) / rect.height) * 100
  youAt.value = [Math.min(88, Math.max(12, x)), Math.min(92, Math.max(66, y))]
}

// --- doors: one per connected location, spread along the back wall ---
const doors = computed(() => props.neighbours.map((l, i, all) => ({
  location: l,
  x: 12 + (76 * (i + 1)) / (all.length + 1),
})))

// --- the menu a hotspot opens; its entries are recomputed from the live choices, never kept ---
type Menu = { title: string; detail: string | null; entityId: string; location: Location | null; lookAt: string | null }
const menu = ref<Menu | null>(null)
const menuEntries = computed<EntityChoice[]>(() => menu.value ? choicesFor(choices.value, menu.value.entityId) : [])
const menuSelectIndex = computed(() => menu.value?.location ? selectLocationIndex(choices.value, menu.value.location) : -1)

function openMenu(title: string, entityId: string, opts: { detail?: string | null; location?: Location; lookAt?: string } = {}) {
  menu.value = { title, detail: opts.detail ?? null, entityId, location: opts.location ?? null, lookAt: opts.lookAt ?? null }
}

function pick(index: number) {
  menu.value = null
  emit('choose', index)
}
</script>

<template>
  <section class="scene" :aria-label="t('locationMap.sceneLabel', { name: nameOf(location.cardCode) })">
    <div class="backdrop" :class="{ painted: sceneArt, plain: !sceneArt }">
      <img v-if="sceneArt" :src="sceneArt" alt="" />
      <img v-else :src="cardArtUrl" alt="" class="from-card" />
    </div>
    <div class="vignette" aria-hidden="true"></div>

    <header class="heading">
      <span class="eyebrow">{{ mine ? t('locationMap.yourLocation') : t('locationMap.viewing') }}</span>
      <h2>{{ nameOf(location.cardCode) }}</h2>
      <p>
        <span v-if="location.shroud !== null">{{ t('locationMap.shroud', { count: location.shroud }) }}</span>
        <span v-if="clues"> · ◇ {{ t('locationMap.cluesCount', { count: clues }) }}</span>
        <span v-if="doom"> · ☠ {{ doom }}</span>
        <span v-if="!location.revealed"> · {{ t('locationMap.unrevealed') }}</span>
      </p>
    </header>

    <div class="floor" :title="t('locationMap.walkHint')" @click="walk"></div>

    <button v-for="d in doors" :key="d.location.id" class="hotspot door" :style="{ left: d.x + '%' }"
      @click.stop="openMenu(nameOf(d.location.cardCode), d.location.id, { location: d.location, lookAt: d.location.id })">
      <span class="symbol">↗</span>
      <span class="label">{{ nameOf(d.location.cardCode) }}</span>
    </button>

    <button v-for="f in figures" :key="f.id" class="figure" :class="[f.kind, { exhausted: f.exhausted, you: f.you }]"
      :style="{ left: f.x + '%', top: f.y + '%', zIndex: Math.round(f.y) }"
      :aria-label="f.name" @click.stop="openMenu(f.name, f.id, { detail: f.note })">
      <span v-if="f.health || f.sanity" class="hud">
        <span v-if="f.health" class="meter hp" :aria-label="t('locationMap.health') + ' ' + f.health.left + '/' + f.health.max">
          <span aria-hidden="true">♥</span><span class="track"><i :style="{ width: (100 * f.health.left / Math.max(1, f.health.max)) + '%' }"></i></span><b>{{ f.health.left }}/{{ f.health.max }}</b>
        </span>
        <span v-if="f.sanity" class="meter sanity" :aria-label="t('locationMap.sanity') + ' ' + f.sanity.left + '/' + f.sanity.max">
          <span aria-hidden="true">◆</span><span class="track"><i :style="{ width: (100 * f.sanity.left / Math.max(1, f.sanity.max)) + '%' }"></i></span><b>{{ f.sanity.left }}/{{ f.sanity.max }}</b>
        </span>
        <span v-if="f.resources !== null" class="counts">
          <span class="count res" :title="t('locationMap.resources')">$ {{ f.resources }}</span>
          <span class="count clue" :title="t('locationMap.clues')">◇ {{ f.clues }}</span>
        </span>
      </span>
      <span class="stand">
        <span class="shadow"></span>
        <img v-if="f.sprite" class="sprite" :src="f.sprite" :alt="f.name" draggable="false" />
        <span v-else class="crop" :class="f.kind" role="img" :aria-label="f.name" :style="{ backgroundImage: `url(${cardImage(f.code)})` }"></span>
      </span>
      <span class="name">{{ f.name }}<small v-if="f.note"> {{ f.note }}</small></span>
    </button>

    <div v-if="menu" class="menu" role="dialog" :aria-label="menu.title" @click.stop>
      <button class="close quiet" :aria-label="t('locationMap.close')" @click="menu = null">✕</button>
      <span class="eyebrow">{{ t('locationMap.interact') }}</span>
      <h3>{{ menu.title }}</h3>
      <p v-if="menu.detail" class="detail">{{ menu.detail }}</p>
      <button v-if="menuSelectIndex !== -1" class="button" data-game-actionable="true" @click="pick(menuSelectIndex)">
        {{ t('locationMap.chooseHere') }}
      </button>
      <AbilityButton v-for="c in menuEntries" :key="c.index" :game="game" :ability="c.contents" @choose="pick(c.index)" />
      <button v-if="menu.lookAt" class="button look" @click="emit('select', menu.lookAt); menu = null">{{ t('locationMap.lookThere') }}</button>
      <p v-if="menuSelectIndex === -1 && menuEntries.length === 0" class="detail">{{ t('locationMap.noActions') }}</p>
    </div>
    <div v-if="me && hand.length" class="hand-fan" :aria-label="t('locationMap.hand', { count: hand.length })">
      <span class="hand-title">{{ t('locationMap.hand', { count: hand.length }) }}</span>
      <div class="cards">
        <div v-for="(card, i) in hand" :key="i" class="slot" :style="{ '--i': i - (hand.length - 1) / 2 }">
          <HandCard :card="card" :game="game" :ownerId="me?.id ?? playerId" :playerId="playerId" @choose="emit('choose', $event)" />
        </div>
      </div>
    </div>
  </section>
</template>

<style scoped>
.scene { position: relative; flex: 1; min-height: 420px; overflow: hidden; background: #0b100d; color: #ece6d0; isolation: isolate; }
.backdrop, .backdrop img { position: absolute; inset: 0; width: 100%; height: 100%; }
.backdrop img { object-fit: cover; }
.backdrop.plain img.from-card { object-fit: cover; object-position: 50% 30%; filter: blur(14px) brightness(0.55) saturate(1.2); transform: scale(1.15); }
.vignette { position: absolute; inset: 0; background: linear-gradient(#08100b99, transparent 22%, transparent 72%, #07100bb0); pointer-events: none; }
.heading { position: absolute; left: 16px; top: 12px; z-index: 5; text-shadow: 0 2px 6px #000; pointer-events: none; }
.heading h2 { margin: 2px 0; font: 1.9rem Georgia, serif; }
.heading p { margin: 0; font-size: 0.8rem; color: #d3d0bd; }
.eyebrow { text-transform: uppercase; letter-spacing: 0.18em; font-size: 0.65rem; color: #c6a873; }
.floor { position: absolute; left: 4%; right: 4%; top: 62%; bottom: 4%; z-index: 1; cursor: crosshair; }
.hotspot { position: absolute; z-index: 30; translate: -50% 0; display: flex; align-items: center; gap: 6px; padding: 5px 10px; border-radius: 999px; background: #101a15d9; border: 1px solid #c6a873aa; color: #f1e6c1; cursor: pointer; font-size: 0.8rem; backdrop-filter: blur(2px); animation: pulse 2.6s ease-in-out infinite; }
.hotspot:hover { background: #26352bf0; border-color: #ffe9a0; }
.hotspot.door { top: 40%; }
.symbol { color: #e6c584; }
.figure { position: absolute; translate: -50% -100%; display: grid; justify-items: center; gap: 2px; padding: 0; background: transparent; border: 0; cursor: pointer; transition: left 0.7s ease, top 0.7s ease; filter: drop-shadow(2px 4px 4px #000c); }
.figure .stand { position: relative; display: grid; justify-items: center; }
.figure .shadow { position: absolute; bottom: -4px; width: 78%; height: 14px; border-radius: 50%; background: radial-gradient(#000a, transparent 70%); }
.figure .sprite { position: relative; height: clamp(110px, 24vh, 210px); width: auto; animation: breathe 4s ease-in-out infinite; transform-origin: 50% 95%; }
.figure .crop { position: relative; display: block; width: clamp(70px, 13vh, 110px); aspect-ratio: 1; border-radius: 50%; border: 2px solid #c9dab1; background-color: #1a2620; background-repeat: no-repeat; background-size: 235% auto; background-position: 100% 14%; }
.figure .crop.enemy { background-size: 165% auto; background-position: 50% 17%; }
.figure.enemy .crop { border-color: #d99e8e; }
.figure.you .crop { border-color: #ffe9a0; box-shadow: 0 0 10px #ffe9a0aa; }
.figure.you .sprite { filter: drop-shadow(0 0 6px #ffe9a0aa); }
.figure.exhausted .stand { opacity: 0.6; transform: rotate(8deg); }
.figure .hud { display: grid; gap: 2px; min-width: 56px; padding: 3px 5px; background: #111815ef; border: 1px solid #9f8d605c; border-radius: 4px; font: 600 10px/1.15 system-ui, sans-serif; white-space: nowrap; }
.figure .meter { display: flex; align-items: center; gap: 3px; font-variant-numeric: tabular-nums; }
.figure .meter.hp { color: #ff9b87; }
.figure .meter.sanity { color: #9fceee; }
.figure .track { width: 22px; height: 4px; background: #67645d; border-radius: 3px; overflow: hidden; }
.figure .track i { display: block; height: 100%; background: currentColor; }
.figure .counts { display: flex; justify-content: space-between; gap: 8px; border-top: 1px solid #ffffff1a; padding-top: 2px; }
.figure .count.res { color: #f0d9a8; }
.figure .count.clue { color: #9fd3e6; }
.figure .meter b { margin-left: auto; color: #fff4df; }
@keyframes breathe { 50% { transform: scaleY(1.015) translateY(-1px); } }
.figure .name { margin-top: 2px; font-size: 0.7rem; background: #0b100dcc; padding: 1px 6px; border-radius: 3px; white-space: nowrap; }
.menu { position: absolute; right: 14px; bottom: 14px; z-index: 50; width: min(320px, calc(100% - 28px)); max-height: 70%; overflow: auto; display: flex; flex-direction: column; gap: 6px; padding: 12px; background: #0f1813f0; border: 1px solid #c6a873aa; border-radius: 8px; box-shadow: 0 8px 28px #000a; }
.menu h3 { margin: 0; font: 1.25rem Georgia, serif; }
.menu .close { position: absolute; right: 6px; top: 4px; background: transparent; border: 0; color: #a3aca3; cursor: pointer; }
.detail { margin: 0; font-size: 0.8rem; color: #b9c2ad; }
.look { background: rgb(180 140 70 / 25%); }
.hand-fan { position: absolute; left: 0; right: 0; bottom: -34px; z-index: 40; display: flex; flex-direction: column; align-items: center; pointer-events: none; }
.hand-title { pointer-events: auto; font-size: 0.7rem; padding: 2px 12px; background: #0f1813e6; border: 1px solid #c6a87366; border-radius: 10px 10px 0 0; color: #e6d6a8; }
.hand-fan .cards { display: flex; justify-content: center; height: 160px; overflow: visible; }
.hand-fan .slot { pointer-events: auto; width: 112px; flex: none; margin-left: -20px; transform-origin: 50% 120%; transform: translateY(calc(var(--i) * var(--i) * 2px - 4px)) rotate(calc(var(--i) * 4deg)); transition: transform 0.15s; }
.hand-fan .slot:hover { z-index: 5; transform: translateY(-44px); }
.hand-fan .slot :deep(.card) { width: 112px; min-width: 112px; }
@keyframes pulse { 50% { box-shadow: 0 0 0 5px #e6c58422; } }
@media (prefers-reduced-motion: reduce) { .hotspot { animation: none; } .figure { transition: none; } .figure .sprite { animation: none; } }
</style>
