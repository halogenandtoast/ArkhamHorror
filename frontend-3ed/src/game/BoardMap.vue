<script setup lang="ts">
import { computed, onMounted, onUnmounted, onUpdated, ref } from 'vue'
import { img } from '@/assets'
import { readPref, writePref } from '@/prefs'
import { useGame } from '@/game/context'
import OutlineFilter from '@/game/OutlineFilter.vue'
import SpaceChips from '@/game/SpaceChips.vue'
import Tok from '@/game/Tok.vue'
import { HUB_R, STREET_H, STREET_W, TILE_H, TILE_W, cssName } from '@/game/util'
import type { Game, Layout } from '@/types'

const ctx = useGame()
const g = computed(() => ctx.game.value!)

const vpEl = ref<HTMLElement | null>(null)
const fitEl = ref<HTMLElement | null>(null)
const boardEl = ref<HTMLElement | null>(null)

// ---- geometry -------------------------------------------------------------
const geo = computed(() => {
  const L = g.value.board.layout
  if (!L || !L.tiles.length) return null
  const pts = [...L.tiles.map((t) => [t.x, t.y, 0.5, 0.58]), ...L.streets.map((s) => [s.x, s.y, 0.3, 0.3])]
  const minX = Math.min(...pts.map((p) => p[0] - p[2])),
    maxX = Math.max(...pts.map((p) => p[0] + p[2]))
  const minY = Math.min(...pts.map((p) => p[1] - p[3])),
    maxY = Math.max(...pts.map((p) => p[1] + p[3]))
  const px = (x: number) => (x - minX) * TILE_W,
    py = (y: number) => (y - minY) * TILE_W
  return { L, px, py, width: (maxX - minX) * TILE_W, height: (maxY - minY) * TILE_W }
})

const streetType = (sid: string) => (g.value.board.spaces[sid]?.kind?.contents ?? 'Residential').toLowerCase()
const streetAngle = (a: number) => {
  let angle = a % 360
  if (angle > 90 && angle <= 270) angle -= 180
  return angle
}

interface Shape {
  sid: string
  key: string
  clipId: string | null
  clipD: string | null
  href: string
  x: number
  y: number
  w: number
  h: number
  transform?: string
}
// the outline of one space: a location's wedge of its tile, or a street's body between the tiles
function spaceShape(game: Game, L: Layout, px: (n: number) => number, py: (n: number) => number, sid: string, idPrefix: string): Shape | null {
  const at = (cx: number, cy: number, r: number, deg: number): [number, number] => [
    px(cx + r * Math.cos((deg * Math.PI) / 180)),
    py(cy + r * Math.sin((deg * Math.PI) / 180)),
  ]
  const pt = ([x, y]: [number, number]) => `${x.toFixed(1)} ${y.toFixed(1)}`
  const id = `${idPrefix}-${cssName(sid)}`,
    key = cssName(sid)
  const a = L.anchors.find((a) => a.space === sid)
  const t = a && L.tiles.find((t) => t.neighborhood === game.board.spaces[sid]?.neighborhood)
  if (a && t) {
    const c = (Math.atan2(a.y - t.y, a.x - t.x) * 180) / Math.PI,
      hub = HUB_R * TILE_W
    // the wedge between the two dividing lines, running well past the tile so its alpha sets the outer edge
    const far = [-60, -30, 0, 30, 60].map((o) => pt(at(t.x, t.y, 0.8, c + o)))
    return {
      sid,
      key,
      clipId: id,
      clipD: `M${pt(at(t.x, t.y, HUB_R, c - 60))} L${far.join(' L')} L${pt(at(t.x, t.y, HUB_R, c + 60))} A${hub} ${hub} 0 0 0 ${pt(at(t.x, t.y, HUB_R, c - 60))} Z`,
      href: img(`tiles/${t.neighborhood}.webp`),
      x: px(t.x) - TILE_W / 2,
      y: py(t.y) - TILE_H / 2,
      w: TILE_W,
      h: TILE_H,
    }
  }
  const st = L.streets.find((st) => st.space === sid)
  if (!st) return null
  const angle = streetAngle(st.angle)
  const cx = px(st.x),
    cy = py(st.y)
  // the whole tile, letting its own alpha set the border as a neighborhood tile's does
  return {
    sid,
    key,
    clipId: null,
    clipD: null,
    href: img(`streets/${streetType(sid)}.webp`),
    x: cx - STREET_W / 2,
    y: cy - STREET_H / 2,
    w: STREET_W,
    h: STREET_H,
    transform: `rotate(${angle} ${cx} ${cy})`,
  }
}

const outlined = computed(() => {
  const G = geo.value
  if (!G) return { shapes: [] as Shape[], ids: new Set<string>() }
  const shapes: Shape[] = []
  const ids = new Set<string>()
  for (const sid of new Set(ctx.unstableShown.value)) {
    const s = spaceShape(g.value, G.L, G.px, G.py, sid, 'clip')
    if (!s) continue
    shapes.push(s)
    ids.add(sid)
  }
  return { shapes, ids }
})
// every space's outline, hidden until a choice naming that space is hovered; it sits above the
// unstable outline and below the tokens
const shaped = computed(() => {
  const G = geo.value
  if (!G) return { shapes: [] as Shape[], ids: new Set<string>() }
  const shapes: Shape[] = []
  const ids = new Set<string>()
  for (const sid of [...G.L.anchors.map((a) => a.space), ...G.L.streets.map((st) => st.space)]) {
    const s = spaceShape(g.value, G.L, G.px, G.py, sid, 'hlclip')
    if (!s) continue
    shapes.push(s)
    ids.add(sid)
  }
  return { shapes, ids }
})

const anchors = computed(() => {
  const G = geo.value
  if (!G) return []
  return [...G.L.anchors.map((a) => ({ sid: a.space, x: a.x, y: a.y })), ...G.L.streets.map((st) => ({ sid: st.space, x: st.x, y: st.y }))]
})
const tileChips = computed(() => {
  const G = geo.value
  if (!G) return []
  return G.L.tiles
    .map((t) => ({ t, n: g.value.board.neighborhoods[t.neighborhood] }))
    .filter(({ n }) => n && (n.clues || n.anomaly || n.terror))
})

function anchorInfo(sid: string) {
  const sel = ctx.spaceChoices.value[sid]
  const unstable = ctx.unstableShown.value.includes(sid) && !outlined.value.ids.has(sid)
  const name = g.value.board.spaces[sid]?.name ?? sid
  return { sel, unstable, name, label: `${name}${unstable ? ' — unstable space' : ''}` }
}
function pick(sid: string) {
  const sel = ctx.spaceChoices.value[sid]
  if (sel && sel.length === 1) void ctx.choose(sel[0][0], sel[0][1])
}

// ---- drag to move ------------------------------------------------------------
const hoverSid = ref<string | null>(null)
const dropClass = (sid: string) => ctx.moveDrag.value?.targets[sid] ?? null
function dragOver(e: DragEvent, sid: string) {
  if (!dropClass(sid)) return
  e.preventDefault()
  if (e.dataTransfer) e.dataTransfer.dropEffect = 'move'
  hoverSid.value = sid
}
function dragLeave(sid: string) {
  if (hoverSid.value === sid) hoverSid.value = null
}
function drop(e: DragEvent, sid: string) {
  if (!dropClass(sid)) return
  e.preventDefault()
  hoverSid.value = null
  document.body.classList.remove('moving')
  void ctx.walkTo(sid)
}
const clearDrag = () => {
  hoverSid.value = null
}

// ---- pan & zoom --------------------------------------------------------------
const view = { zoom: 1, x: 0, y: 0, base: 1, fresh: true }
const MAX_ZOOM = 6
/* The whole map can be turned a quarter at a time, for players sitting along a
different side of the table. Everything drawn on the map turns with it -- tiles,
streets, the space outlines -- while the pieces standing on it are turned back so
they still read upright (see --map-rot). */
const rotation = ref(((Number(readPref('ah3e-map-rot')) || 0) % 360 + 360) % 360)
const quarterTurned = computed(() => rotation.value % 180 !== 0)
function rotateMap() {
  rotation.value = (rotation.value + 90) % 360
  writePref('ah3e-map-rot', String(rotation.value))
  view.fresh = true
  applyView()
}
const zoomed = ref(false)
const panning = ref(false)
const isFull = () => document.body.classList.contains('map-full')
function applyView() {
  const vp = vpEl.value,
    fit = fitEl.value,
    board = boardEl.value
  if (!vp || !fit || !board) return
  const full = isFull()
  const W = board.offsetWidth,
    H = board.offsetHeight,
    vw = vp.clientWidth
  const [We, He] = boxSize(W, H)
  view.base = full ? Math.min(vw / We, vp.clientHeight / He) : Math.min(1, vw / We)
  fit.style.height = full ? `${vp.clientHeight}px` : `${He * view.base}px`
  const vh = full ? vp.clientHeight : He * view.base
  const s = view.base * view.zoom,
    bw = We * s,
    bh = He * s
  if (view.fresh) {
    view.x = (vw - bw) / 2
    view.y = (vh - bh) / 2
    view.fresh = false
  }
  view.x = bw <= vw ? (vw - bw) / 2 : Math.min(0, Math.max(vw - bw, view.x))
  view.y = bh <= vh ? (vh - bh) / 2 : Math.min(0, Math.max(vh - bh, view.y))
  /* rotate about the board's centre, then bring the turned box back to the origin,
  since the board is laid out from its top left */
  board.style.transform =
    `translate(${view.x}px, ${view.y}px) scale(${s})` +
    ` translate(${We / 2}px, ${He / 2}px) rotate(${rotation.value}deg) translate(${-W / 2}px, ${-H / 2}px)`
  board.style.setProperty('--map-rot', `${-rotation.value}deg`)
  zoomed.value = view.zoom > 1.001
}

// the turned box: a quarter turn swaps the map's width and height
function boxSize(w: number, h: number): [number, number] {
  return quarterTurned.value ? [h, w] : [w, h]
}

// a point on the map, in the turned box the view scrolls around
function mapToBox(bx: number, by: number): [number, number] {
  const board = boardEl.value
  if (!board) return [bx, by]
  const W = board.offsetWidth,
    H = board.offsetHeight
  const [We, He] = boxSize(W, H)
  const a = (rotation.value * Math.PI) / 180
  const dx = bx - W / 2,
    dy = by - H / 2
  return [Math.cos(a) * dx - Math.sin(a) * dy + We / 2, Math.sin(a) * dx + Math.cos(a) * dy + He / 2]
}

// and back again, for working out what was clicked
function boxToMap(ux: number, uy: number): [number, number] {
  const board = boardEl.value
  if (!board) return [ux, uy]
  const W = board.offsetWidth,
    H = board.offsetHeight
  const [We, He] = boxSize(W, H)
  const a = (-rotation.value * Math.PI) / 180
  const dx = ux - We / 2,
    dy = uy - He / 2
  return [Math.cos(a) * dx - Math.sin(a) * dy + W / 2, Math.sin(a) * dx + Math.cos(a) * dy + H / 2]
}
function zoomAt(factor: number, cx: number, cy: number) {
  const s = view.base * view.zoom
  const z = Math.min(MAX_ZOOM, Math.max(1, view.zoom * factor))
  const s2 = view.base * z
  view.x = cx - ((cx - view.x) / s) * s2
  view.y = cy - ((cy - view.y) / s) * s2
  view.zoom = z
  applyView()
}
const viewportHeight = () => (isFull() ? vpEl.value!.clientHeight : (fitEl.value?.offsetHeight ?? 0))
function zoomBy(factor: number) {
  if (vpEl.value) zoomAt(factor, vpEl.value.clientWidth / 2, viewportHeight() / 2)
}
function resetView() {
  view.zoom = 1
  view.fresh = true
  applyView()
}
function zoomToTile(bx: number, by: number) {
  const vp = vpEl.value
  if (!vp) return
  const vw = vp.clientWidth,
    vh = viewportHeight()
  const s2 = Math.min((vw * 0.85) / TILE_W, (vh * 0.85) / TILE_H)
  view.zoom = Math.min(MAX_ZOOM, Math.max(1, s2 / view.base))
  const s = view.base * view.zoom
  const [ux, uy] = mapToBox(bx, by)
  view.x = vw / 2 - ux * s
  view.y = vh / 2 - uy * s
  applyView()
}

const hideTokens = ref(!!readPref('ah3e-hide-tokens'))
function toggleTokens(hide = !hideTokens.value) {
  hideTokens.value = hide
  writePref('ah3e-hide-tokens', hide ? '1' : '')
}
const mapFull = ref(false)
function toggleMapFull(open = !mapFull.value) {
  mapFull.value = open
  document.body.classList.toggle('map-full', open)
  resetView()
}

let drag: { id: number; sx: number; sy: number; x: number; y: number; moved: boolean } | null = null
let suppressClick = false
function pointerDown(e: PointerEvent) {
  if (e.button !== 0 || (e.target as Element).closest('.map-controls')) return
  drag = { id: e.pointerId, sx: e.clientX, sy: e.clientY, x: view.x, y: view.y, moved: false }
}
function pointerMove(e: PointerEvent) {
  if (!drag || e.pointerId !== drag.id) return
  const dx = e.clientX - drag.sx,
    dy = e.clientY - drag.sy
  if (!drag.moved && Math.hypot(dx, dy) < 5) return
  if (!drag.moved) {
    drag.moved = true
    vpEl.value?.setPointerCapture(e.pointerId)
    panning.value = true
  }
  view.x = drag.x + dx
  view.y = drag.y + dy
  applyView()
}
function pointerEnd(e: PointerEvent) {
  if (!drag || e.pointerId !== drag.id) return
  if (drag.moved) suppressClick = true
  drag = null
  panning.value = false
}
// a pan must not also answer a question on the space it ended over
function clickCapture(e: MouseEvent) {
  if (suppressClick) {
    e.stopPropagation()
    e.preventDefault()
    suppressClick = false
  }
}
function wheel(e: WheelEvent) {
  if (!isFull() && !e.ctrlKey && !e.metaKey) return
  e.preventDefault()
  const r = vpEl.value!.getBoundingClientRect()
  zoomAt(Math.exp(-e.deltaY * (e.ctrlKey ? 0.01 : 0.0015)), e.clientX - r.left, e.clientY - r.top)
}
function dblclick(e: MouseEvent) {
  if ((e.target as Element).closest('.anchor.selectable, .map-controls')) return
  if (view.zoom > 1.5) {
    resetView()
    return
  }
  const r = vpEl.value!.getBoundingClientRect()
  const s = view.base * view.zoom
  const [bx, by] = boxToMap((e.clientX - r.left - view.x) / s, (e.clientY - r.top - view.y) / s)
  let best: [number, number] | null = null,
    dist = Infinity
  const G = geo.value
  for (const t of G?.L.tiles ?? []) {
    const tx = G!.px(t.x),
      ty = G!.py(t.y)
    const d = Math.hypot(tx - bx, ty - by)
    if (d < dist) {
      dist = d
      best = [tx, ty]
    }
  }
  if (best && dist < TILE_W) zoomToTile(best[0], best[1])
  else zoomAt(2, e.clientX - r.left, e.clientY - r.top)
}
function keydown(e: KeyboardEvent) {
  if (e.target instanceof HTMLElement && ['INPUT', 'TEXTAREA', 'SELECT'].includes(e.target.tagName)) return
  if (e.key === 'Escape' && isFull()) {
    toggleMapFull(false)
    return
  }
  if (e.metaKey || e.ctrlKey || e.altKey) return
  if (e.key === 'r' || e.key === 'R') rotateMap()
  else if (e.key === '+' || e.key === '=') zoomBy(1.4)
  else if (e.key === '-' || e.key === '_') zoomBy(1 / 1.4)
  else if (e.key === '0') resetView()
  else if (e.key === 't' || e.key === 'T') toggleTokens()
}
const refit = () => applyView()

onMounted(() => {
  document.addEventListener('keydown', keydown)
  document.addEventListener('dragend', clearDrag)
  window.addEventListener('resize', refit)
  window.addEventListener('ah3e-fit', refit)
  applyView()
})
onUpdated(applyView)
onUnmounted(() => {
  document.removeEventListener('keydown', keydown)
  document.removeEventListener('dragend', clearDrag)
  window.removeEventListener('resize', refit)
  window.removeEventListener('ah3e-fit', refit)
  document.body.classList.remove('map-full', 'moving')
})
</script>

<template>
  <div
    id="mapViewport"
    ref="vpEl"
    class="map-wrap"
    :class="{ 'hide-tokens': hideTokens, zoomed, panning }"
    @pointerdown="pointerDown"
    @pointermove="pointerMove"
    @pointerup="pointerEnd"
    @pointercancel="pointerEnd"
    @click.capture="clickCapture"
    @wheel="wheel"
    @dblclick="dblclick"
  >
    <div id="map">
      <div v-if="geo" ref="fitEl" class="board-fit">
        <div ref="boardEl" class="board" :style="{ width: `${geo.width}px`, height: `${geo.height}px` }">
          <img
            v-for="st in geo.L.streets"
            :key="`street-${st.space}`"
            :src="img(`streets/${streetType(st.space)}.webp`)"
            :style="{
              left: `${geo.px(st.x) - STREET_W / 2}px`,
              top: `${geo.py(st.y) - STREET_H / 2}px`,
              width: `${STREET_W}px`,
              height: `${STREET_H}px`,
              transform: `rotate(${streetAngle(st.angle)}deg)`,
            }"
          />
          <img
            v-for="t in geo.L.tiles"
            :key="`tile-${t.neighborhood}`"
            :src="img(`tiles/${t.neighborhood}.webp`)"
            :style="{
              left: `${geo.px(t.x) - TILE_W / 2}px`,
              top: `${geo.py(t.y) - TILE_H / 2}px`,
              width: `${TILE_W}px`,
              height: `${TILE_H}px`,
            }"
          />
          <svg
            v-if="outlined.shapes.length"
            class="space-outlines"
            :class="{ held: ctx.unstableHeld.value, reveal: ctx.unstableReveal.value }"
            width="1"
            height="1"
          >
            <defs>
              <OutlineFilter id="spaceOutline" stroke="var(--doom)" fill="rgb(179,38,30)" />
              <template v-for="s in outlined.shapes" :key="`c-${s.key}`">
                <clipPath v-if="s.clipId" :id="s.clipId"><path :d="s.clipD!" /></clipPath>
              </template>
            </defs>
            <!-- the unstable outline is not a hover target; only the highlight layer answers to hovering -->
            <g v-for="s in outlined.shapes" :key="`o-${s.key}`" filter="url(#spaceOutline)">
              <title>{{ ctx.spaceName(s.sid) }} &mdash; unstable space</title>
              <image
                :href="s.href"
                :x="s.x"
                :y="s.y"
                :width="s.w"
                :height="s.h"
                preserveAspectRatio="none"
                :clip-path="s.clipId ? `url(#${s.clipId})` : undefined"
                :transform="s.transform"
              />
            </g>
          </svg>
          <svg class="space-highlights" width="1" height="1">
            <defs>
              <OutlineFilter id="spaceHighlight" stroke="#ffd54f" fill="rgb(255,213,79)" />
              <template v-for="s in shaped.shapes" :key="`hc-${s.key}`">
                <clipPath v-if="s.clipId" :id="s.clipId"><path :d="s.clipD!" /></clipPath>
              </template>
            </defs>
            <g
              v-for="s in shaped.shapes"
              :key="`h-${s.key}`"
              :data-space-outline="s.key"
              :class="{ hl: ctx.highlighted.value === `space:${s.key}` }"
              filter="url(#spaceHighlight)"
            >
              <image
                :href="s.href"
                :x="s.x"
                :y="s.y"
                :width="s.w"
                :height="s.h"
                preserveAspectRatio="none"
                :clip-path="s.clipId ? `url(#${s.clipId})` : undefined"
                :transform="s.transform"
              />
            </g>
          </svg>
          <template v-for="a in anchors" :key="`a-${a.sid}`">
            <div
              v-for="info in [anchorInfo(a.sid)]"
              :key="`a-${a.sid}`"
              :data-space="cssName(a.sid)"
              :data-sid="a.sid"
              class="anchor"
              :class="[
                {
                  selectable: !!info.sel,
                  unstable: info.unstable,
                  held: info.unstable && ctx.unstableHeld.value,
                  shaped: shaped.ids.has(a.sid),
                  'drop-hover': hoverSid === a.sid,
                },
                dropClass(a.sid),
                ctx.marks(['space', cssName(a.sid)]),
              ]"
              :title="info.label"
              :style="{ left: `${geo.px(a.x)}px`, top: `${geo.py(a.y)}px` }"
              @click="pick(a.sid)"
              @dragover="dragOver($event, a.sid)"
              @dragleave="dragLeave(a.sid)"
              @drop="drop($event, a.sid)"
            >
              <SpaceChips :sid="a.sid" />
              <span v-if="info.sel" class="pick-ring" :title="info.name" @click.stop="pick(a.sid)"></span>
            </div>
          </template>
          <div
            v-for="{ t, n } in tileChips"
            :key="`chips-${t.neighborhood}`"
            class="anchor"
            :style="{ left: `${geo.px(t.x)}px`, top: `${geo.py(t.y + 0.13)}px` }"
          >
            <Tok v-if="n.clues" name="clue" :count="n.clues" :title="`${n.clues} clues`" :size="30" />
            <Tok v-if="n.anomaly" name="anomaly" title="anomaly" :size="34" />
            <Tok v-if="n.terror" name="terror" :count="n.terror" :title="`${n.terror} terror`" :size="30" />
          </div>
        </div>
      </div>
    </div>
    <div class="map-controls" role="toolbar" aria-label="Map view">
      <button title="Zoom out (−)" aria-label="Zoom out" @click="zoomBy(1 / 1.4)">−</button>
      <button title="Zoom in (+)" aria-label="Zoom in" @click="zoomBy(1.4)">+</button>
      <button title="Fit the whole map (0)" aria-label="Fit map" @click="resetView()">⤢</button>
      <button
        id="mapRotate"
        :title="`Turn the map a quarter clockwise (R) — now ${rotation}°`"
        aria-label="Rotate map"
        @click="rotateMap()"
      >
        ⟳
      </button>
      <button
        id="tokensToggle"
        :title="hideTokens ? 'Show tokens (T)' : 'Hide tokens (T)'"
        :aria-label="hideTokens ? 'Show tokens' : 'Hide tokens'"
        :aria-pressed="hideTokens"
        @click="toggleTokens()"
      >
        <svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round">
          <path d="M2 12s3.6-7 10-7 10 7 10 7-3.6 7-10 7S2 12 2 12z" />
          <circle cx="12" cy="12" r="3" />
          <path class="eye-off" d="M4 4l16 16" :style="{ display: hideTokens ? '' : 'none' }" />
        </svg>
      </button>
      <button id="mapFullToggle" title="Full screen map" aria-label="Full screen map" @click="toggleMapFull()">⛶</button>
    </div>
    <div class="map-hint">Pinch or Ctrl-scroll to zoom · drag to pan · double-click a tile · R to turn the map</div>
  </div>
</template>
