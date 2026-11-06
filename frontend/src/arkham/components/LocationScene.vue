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
import AbilityButton from '@/arkham/components/AbilityButton.vue'

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
const choices = useGameChoices(() => props.game, () => props.playerId)

// Backdrops shipped in public/img/scenes, named by printed card code. Any other location falls back to its card art.
const SCENE_ART = new Set(['01111', '01112', '01113', '01114', '01115'])
const artCode = computed(() => cardArt(props.location.cardCode))
const sceneArt = computed(() => props.location.revealed && SCENE_ART.has(artCode.value) ? `img/scenes/${artCode.value}.webp` : null)
const cardArtUrl = computed(() => cardImage(props.location.cardCode, props.location.revealed ? '' : 'b'))

const clues = computed(() => props.location.tokens[TokenType.Clue] ?? 0)
const doom = computed(() => props.location.tokens[TokenType.Doom] ?? 0)

// --- who is standing here ---
type Figure = { id: string; code: string; name: string; kind: 'investigator' | 'enemy'; x: number; y: number; exhausted: boolean; you: boolean; note: string | null }

const ENEMY_SPOTS: [number, number][] = [[72, 80], [84, 72], [78, 90], [60, 72], [88, 88], [62, 90]]
const youAt = ref<[number, number]>([46, 82])
watch(() => props.location.id, () => { youAt.value = [46, 82]; menu.value = null })

const figures = computed<Figure[]>(() => {
  const result: Figure[] = []
  let others = 0
  for (const id of props.location.investigators) {
    const investigator = props.game.investigators[id]
    if (!investigator) continue
    const you = id === props.playerId
    const [x, y] = you ? youAt.value : [30 + 9 * others++, 86]
    result.push({ id, code: investigator.cardCode, name: nameOf(investigator.cardCode), kind: 'investigator', x, y, exhausted: false, you, note: null })
  }
  let slot = 0
  for (const id of props.location.enemies) {
    const enemy = props.game.enemies[id]
    if (!enemy || enemy.placement.tag !== 'AtLocation') continue
    const [x, y] = ENEMY_SPOTS[slot++ % ENEMY_SPOTS.length]
    const damage = enemy.tokens[TokenType.Damage] ?? 0
    result.push({ id, code: enemy.cardCode, name: nameOf(enemy.cardCode), kind: 'enemy', x, y, exhausted: enemy.exhausted, you: false, note: damage ? `♥ −${damage}` : null })
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

    <button class="hotspot search" @click.stop="openMenu(nameOf(location.cardCode), location.id, { location })">
      <span class="symbol">◈</span>
      <span class="label">{{ t('locationMap.searchArea') }}</span>
    </button>

    <button v-for="f in figures" :key="f.id" class="figure" :class="[f.kind, { exhausted: f.exhausted, you: f.you }]"
      :style="{ left: f.x + '%', top: f.y + '%', zIndex: Math.round(f.y) }"
      :aria-label="f.name" @click.stop="openMenu(f.name, f.id, { detail: f.note })">
      <img :src="cardImage(f.code)" :alt="f.name" />
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
.hotspot.search { left: 50%; top: 56%; }
.symbol { color: #e6c584; }
.figure { position: absolute; translate: -50% -100%; display: grid; justify-items: center; gap: 2px; padding: 0; background: transparent; border: 0; cursor: pointer; transition: left 0.7s ease, top 0.7s ease; filter: drop-shadow(2px 4px 4px #000c); }
.figure img { height: clamp(86px, 17vh, 150px); aspect-ratio: 5 / 7; object-fit: cover; background: #1a2620; border-radius: 6px; border: 2px solid #c9dab1; }
.figure.enemy img { border-color: #d99e8e; }
.figure.you img { border-color: #ffe9a0; box-shadow: 0 0 10px #ffe9a0aa; }
.figure.exhausted img { opacity: 0.6; transform: rotate(8deg); }
.figure .name { font-size: 0.7rem; background: #0b100dcc; padding: 1px 6px; border-radius: 3px; white-space: nowrap; }
.menu { position: absolute; right: 14px; bottom: 14px; z-index: 50; width: min(320px, calc(100% - 28px)); max-height: 70%; overflow: auto; display: flex; flex-direction: column; gap: 6px; padding: 12px; background: #0f1813f0; border: 1px solid #c6a873aa; border-radius: 8px; box-shadow: 0 8px 28px #000a; }
.menu h3 { margin: 0; font: 1.25rem Georgia, serif; }
.menu .close { position: absolute; right: 6px; top: 4px; background: transparent; border: 0; color: #a3aca3; cursor: pointer; }
.detail { margin: 0; font-size: 0.8rem; color: #b9c2ad; }
.look { background: rgb(180 140 70 / 25%); }
@keyframes pulse { 50% { box-shadow: 0 0 0 5px #e6c58422; } }
@media (prefers-reduced-motion: reduce) { .hotspot { animation: none; } .figure { transition: none; } }
</style>
