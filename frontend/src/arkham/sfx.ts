/*
 * Game sound effects, synthesised live with Web Audio.
 *
 * Same approach as horrorAmbience.ts: nothing is downloaded, so there is no
 * licence to track and no bytes to ship. Each effect is a small recipe of
 * oscillators and filtered noise with a fast envelope.
 *
 * One shared AudioContext, created on the first effect (which always follows a
 * click, so autoplay rules are satisfied) and kept for the page's life.
 */

import type { SfxName } from '@/arkham/sfxEvents'

type WindowWithWebkitAudio = Window & { webkitAudioContext?: typeof AudioContext }

const GAP = 0.16 // seconds between effects played from one update
const MASTER = 0.5

let ctx: AudioContext | null = null
let master: GainNode | null = null
let noise: AudioBuffer | null = null

function context(): AudioContext | null {
  if (ctx) return ctx
  const Ctor = window.AudioContext ?? (window as WindowWithWebkitAudio).webkitAudioContext
  if (!Ctor) return null
  ctx = new Ctor()
  master = ctx.createGain()
  master.gain.value = MASTER
  master.connect(ctx.destination)
  const length = ctx.sampleRate * 2
  noise = ctx.createBuffer(1, length, ctx.sampleRate)
  const data = noise.getChannelData(0)
  for (let i = 0; i < length; i++) data[i] = Math.random() * 2 - 1
  return ctx
}

/* An amplitude envelope: instant attack, exponential decay. */
function envelope(c: AudioContext, at: number, peak: number, decay: number, attack = 0.004): GainNode {
  const g = c.createGain()
  g.gain.setValueAtTime(0.0001, at)
  g.gain.linearRampToValueAtTime(peak, at + attack)
  g.gain.exponentialRampToValueAtTime(0.0001, at + attack + decay)
  return g
}

function tone(c: AudioContext, at: number, o: {
  type?: OscillatorType
  from: number
  to?: number
  peak: number
  decay: number
  attack?: number
  filter?: number
}) {
  const osc = c.createOscillator()
  osc.type = o.type ?? 'sine'
  osc.frequency.setValueAtTime(o.from, at)
  if (o.to) osc.frequency.exponentialRampToValueAtTime(o.to, at + o.decay)
  const env = envelope(c, at, o.peak, o.decay, o.attack)
  let node: AudioNode = osc
  if (o.filter) {
    const lp = c.createBiquadFilter()
    lp.type = 'lowpass'
    lp.frequency.value = o.filter
    osc.connect(lp)
    node = lp
  }
  node.connect(env).connect(master!)
  osc.start(at)
  osc.stop(at + (o.attack ?? 0.004) + o.decay + 0.05)
}

function hiss(c: AudioContext, at: number, o: {
  filter: BiquadFilterType
  from: number
  to?: number
  q?: number
  peak: number
  decay: number
  attack?: number
}) {
  const src = c.createBufferSource()
  src.buffer = noise
  const f = c.createBiquadFilter()
  f.type = o.filter
  f.Q.value = o.q ?? 1
  f.frequency.setValueAtTime(o.from, at)
  if (o.to) f.frequency.exponentialRampToValueAtTime(o.to, at + o.decay)
  const env = envelope(c, at, o.peak, o.decay, o.attack)
  src.connect(f).connect(env).connect(master!)
  src.start(at, Math.random())
  src.stop(at + (o.attack ?? 0.004) + o.decay + 0.05)
}

type Recipe = (c: AudioContext, t: number) => void

const RECIPES: Record<SfxName, Recipe> = {
  // Scene change: a slow dark swell with a sub boom underneath.
  transition: (c, t) => {
    hiss(c, t, { filter: 'bandpass', from: 180, to: 900, q: 1.2, peak: 0.35, decay: 1.6, attack: 0.7 })
    tone(c, t + 0.5, { from: 70, to: 38, peak: 0.55, decay: 1.6, filter: 200 })
  },
  // Two soft footsteps on old boards.
  move: (c, t) => {
    for (const [dt, base] of [[0, 150], [0.2, 130]] as const) {
      hiss(c, t + dt, { filter: 'lowpass', from: 420, to: 160, peak: 0.5, decay: 0.12 })
      tone(c, t + dt, { from: base, to: 60, peak: 0.35, decay: 0.12 })
    }
    hiss(c, t + 0.05, { filter: 'bandpass', from: 1800, to: 900, q: 6, peak: 0.05, decay: 0.3, attack: 0.1 })
  },
  // A blow landing on a monster: thud with a crack on top.
  hit: (c, t) => {
    tone(c, t, { from: 190, to: 55, peak: 0.7, decay: 0.2 })
    hiss(c, t, { filter: 'bandpass', from: 1400, to: 500, q: 1.5, peak: 0.45, decay: 0.12 })
  },
  // The investigator is hurt: heavier thud and a rasping groan.
  hurt: (c, t) => {
    tone(c, t, { from: 140, to: 40, peak: 0.85, decay: 0.3 })
    hiss(c, t, { filter: 'lowpass', from: 900, to: 200, peak: 0.55, decay: 0.25 })
    tone(c, t + 0.04, { type: 'sawtooth', from: 120, to: 70, peak: 0.18, decay: 0.45, filter: 500 })
  },
  // Sanity slipping: two close notes beating, wavering upward.
  horror: (c, t) => {
    tone(c, t, { type: 'triangle', from: 520, to: 700, peak: 0.16, decay: 1.2, attack: 0.25 })
    tone(c, t, { type: 'triangle', from: 548, to: 740, peak: 0.16, decay: 1.2, attack: 0.25 })
    tone(c, t + 0.1, { from: 46, to: 40, peak: 0.4, decay: 1.1, attack: 0.2 })
  },
  // A creature dying: a falling growl that collapses into breath.
  enemyDefeat: (c, t) => {
    tone(c, t, { type: 'sawtooth', from: 170, to: 38, peak: 0.45, decay: 0.9, filter: 600 })
    hiss(c, t, { filter: 'lowpass', from: 1200, to: 120, peak: 0.4, decay: 0.8 })
    tone(c, t, { from: 90, to: 30, peak: 0.6, decay: 0.5 })
  },
  // A card slid off the deck.
  cardDraw: (c, t) => {
    hiss(c, t, { filter: 'highpass', from: 2500, to: 5200, peak: 0.28, decay: 0.14, attack: 0.03 })
  },
  // A glassy ping for a clue.
  clue: (c, t) => {
    for (const [ratio, level] of [[1, 0.2], [2.76, 0.1], [5.4, 0.05]] as const) {
      tone(c, t, { from: 1180 * ratio, peak: level, decay: 0.7 })
    }
  },
  // A chaos token clacking onto the table.
  token: (c, t) => {
    tone(c, t, { type: 'triangle', from: 1100, to: 700, peak: 0.28, decay: 0.07 })
    hiss(c, t, { filter: 'highpass', from: 3000, peak: 0.25, decay: 0.05 })
    tone(c, t + 0.07, { type: 'triangle', from: 900, to: 600, peak: 0.12, decay: 0.05 })
  },
  // Relief, but uneasy: a rising third, soft.
  success: (c, t) => {
    tone(c, t, { type: 'triangle', from: 330, peak: 0.28, decay: 0.9 })
    tone(c, t + 0.14, { type: 'triangle', from: 415, peak: 0.28, decay: 1.0 })
    tone(c, t + 0.28, { type: 'triangle', from: 495, peak: 0.2, decay: 1.1 })
  },
  // Dull and falling: a minor sag with a low hit.
  fail: (c, t) => {
    tone(c, t, { type: 'triangle', from: 247, to: 196, peak: 0.3, decay: 0.8 })
    tone(c, t + 0.15, { type: 'triangle', from: 233, to: 175, peak: 0.26, decay: 0.9 })
    tone(c, t, { from: 80, to: 45, peak: 0.45, decay: 0.5 })
  },
}

/** Plays effects one after another so a busy update reads as a sequence, not a smear. */
export function playSfx(names: readonly SfxName[]): void {
  const c = context()
  if (!c || names.length === 0) return
  if (c.state === 'suspended') void c.resume().catch(() => {})
  const start = c.currentTime + 0.02
  names.forEach((name, i) => RECIPES[name](c, start + i * GAP))
}
