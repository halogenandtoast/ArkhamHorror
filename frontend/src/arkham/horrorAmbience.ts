/*
 * Horror ambience for a scenario, synthesised live with Web Audio.
 *
 * Nothing is downloaded: no recordings, no licences, no bytes. Three layers:
 *  - a dissonant low drone (a root, its minor second and a flattened fifth)
 *    whose filter breathes open and shut over half a minute;
 *  - wind: noise through a band-pass that wanders, swelling and dying away;
 *  - now and then, one distant event: a tolling bell, a slow heartbeat, or a
 *    thin rising whine.
 * Everything runs through a generated dark-hall reverb, and the whole bed
 * ducks under the story narration so the narrator is never fighting it.
 */

export interface HorrorAmbienceInstance {
  /** Fade in (creating the graph on first use). Safe to call repeatedly. */
  start: () => void
  /** Fade out and suspend, keeping the graph so restarting is instant. */
  stop: () => void
  setVolume: (volume: number) => void
  /** Pull the bed down while something must be heard over it. */
  duck: (ducked: boolean) => void
  destroy: () => void
}

type WindowWithWebkitAudio = Window & { webkitAudioContext?: typeof AudioContext }

const FADE = 3 // seconds in and out
const DUCK_LEVEL = 0.3 // share of the volume kept under narration
const EVENT_GAP: [number, number] = [14, 38] // seconds between distant events

const rand = (min: number, max: number) => min + Math.random() * (max - min)

/* A few seconds of decaying stereo noise: the impulse of a large, dark room. */
function hallImpulse(ctx: AudioContext, seconds = 4.5): AudioBuffer {
  const length = Math.floor(ctx.sampleRate * seconds)
  const buffer = ctx.createBuffer(2, length, ctx.sampleRate)
  for (let c = 0; c < 2; c++) {
    const data = buffer.getChannelData(c)
    for (let i = 0; i < length; i++) data[i] = (Math.random() * 2 - 1) * Math.pow(1 - i / length, 3)
  }
  return buffer
}

/* Brown-ish noise (integrated white noise): a rumble rather than a hiss. */
function noiseBuffer(ctx: AudioContext, seconds = 6): AudioBuffer {
  const length = Math.floor(ctx.sampleRate * seconds)
  const buffer = ctx.createBuffer(1, length, ctx.sampleRate)
  const data = buffer.getChannelData(0)
  let last = 0
  for (let i = 0; i < length; i++) {
    last = (last + 0.02 * (Math.random() * 2 - 1)) / 1.02
    data[i] = last * 3.5
  }
  return buffer
}

/* A slow sine wobble added onto an AudioParam. */
function lfo(ctx: AudioContext, target: AudioParam, rate: number, depth: number): OscillatorNode {
  const osc = ctx.createOscillator()
  const amount = ctx.createGain()
  osc.frequency.value = rate
  amount.gain.value = depth
  osc.connect(amount).connect(target)
  osc.start()
  return osc
}

function buildDrone(ctx: AudioContext, out: AudioNode): AudioNode[] {
  const filter = ctx.createBiquadFilter()
  filter.type = 'lowpass'
  filter.frequency.value = 240
  filter.Q.value = 4
  const gain = ctx.createGain()
  gain.gain.value = 0.16
  filter.connect(gain).connect(out)
  const root = 49 // G1
  const voices = [root, root * 1.0595, root * 1.414, root * 2.003].map((f, i) => {
    const osc = ctx.createOscillator()
    osc.type = i % 2 === 0 ? 'sawtooth' : 'triangle'
    osc.frequency.value = f
    lfo(ctx, osc.detune, rand(0.05, 0.12), 6) // slow beating between voices
    osc.connect(filter)
    osc.start()
    return osc
  })
  return [...voices, lfo(ctx, filter.frequency, 1 / 34, 140), lfo(ctx, gain.gain, 1 / 23, 0.05)]
}

function buildWind(ctx: AudioContext, out: AudioNode): AudioNode[] {
  const source = ctx.createBufferSource()
  source.buffer = noiseBuffer(ctx)
  source.loop = true
  const band = ctx.createBiquadFilter()
  band.type = 'bandpass'
  band.frequency.value = 520
  band.Q.value = 1.6
  const gain = ctx.createGain()
  gain.gain.value = 0.12
  source.connect(band).connect(gain).connect(out)
  source.start()
  return [source, lfo(ctx, band.frequency, 1 / 17, 280), lfo(ctx, gain.gain, 1 / 11, 0.09)]
}

/* A struck bell with inharmonic partials, far away in the reverb. */
function bell(ctx: AudioContext, out: AudioNode, at: number) {
  const base = rand(90, 140)
  for (const [ratio, level] of [[1, 0.5], [2.4, 0.25], [3.9, 0.14], [5.4, 0.08]] as const) {
    const osc = ctx.createOscillator()
    const env = ctx.createGain()
    osc.frequency.value = base * ratio
    env.gain.setValueAtTime(0, at)
    env.gain.linearRampToValueAtTime(level * 0.35, at + 0.01)
    env.gain.exponentialRampToValueAtTime(0.0001, at + 7)
    osc.connect(env).connect(out)
    osc.start(at)
    osc.stop(at + 7.2)
  }
}

/* Lub-dub, a handful of times, slowing. */
function heartbeat(ctx: AudioContext, out: AudioNode, at: number) {
  let t = at
  for (let beat = 0; beat < 5; beat++) {
    for (const [offset, level] of [[0, 0.7], [0.22, 0.45]] as const) {
      const osc = ctx.createOscillator()
      const env = ctx.createGain()
      osc.frequency.setValueAtTime(58, t + offset)
      osc.frequency.exponentialRampToValueAtTime(32, t + offset + 0.18)
      env.gain.setValueAtTime(level * 0.5, t + offset)
      env.gain.exponentialRampToValueAtTime(0.0001, t + offset + 0.25)
      osc.connect(env).connect(out)
      osc.start(t + offset)
      osc.stop(t + offset + 0.3)
    }
    t += 1.05 + beat * 0.12
  }
}

/* A thin tone sliding up, like wind through a gap or something breathing. */
function whine(ctx: AudioContext, out: AudioNode, at: number) {
  const osc = ctx.createOscillator()
  const env = ctx.createGain()
  osc.type = 'sine'
  const from = rand(620, 900)
  osc.frequency.setValueAtTime(from, at)
  osc.frequency.exponentialRampToValueAtTime(from * rand(1.25, 1.6), at + 6)
  const vibrato = lfo(ctx, osc.detune, 5.5, 18)
  env.gain.setValueAtTime(0, at)
  env.gain.linearRampToValueAtTime(0.05, at + 2.5)
  env.gain.linearRampToValueAtTime(0, at + 6)
  osc.connect(env).connect(out)
  osc.start(at)
  osc.stop(at + 6.1)
  vibrato.stop(at + 6.1) // or it would run, inaudible, forever
}

const EVENTS = [bell, bell, heartbeat, whine]

/** Builds nothing until started. Returns null when Web Audio is unavailable. */
export function createHorrorAmbience(initialVolume = 0.5): HorrorAmbienceInstance | null {
  const Ctx = window.AudioContext ?? (window as WindowWithWebkitAudio).webkitAudioContext
  if (!Ctx) return null

  let ctx: AudioContext | null = null
  let master: GainNode | null = null
  let eventBus: GainNode | null = null
  let nodes: AudioNode[] = []
  let timer: ReturnType<typeof setTimeout> | null = null
  let volume = initialVolume
  let ducked = false
  let running = false

  const level = () => volume * (ducked ? DUCK_LEVEL : 1)

  const ramp = (to: number, seconds: number) => {
    if (!ctx || !master) return
    const now = ctx.currentTime
    master.gain.cancelScheduledValues(now)
    master.gain.setValueAtTime(master.gain.value, now)
    master.gain.linearRampToValueAtTime(to, now + seconds)
  }

  const build = () => {
    ctx = new Ctx()
    master = ctx.createGain()
    master.gain.value = 0
    master.connect(ctx.destination)
    const reverb = ctx.createConvolver()
    reverb.buffer = hallImpulse(ctx)
    const wet = ctx.createGain()
    wet.gain.value = 0.55
    reverb.connect(wet).connect(master)
    const bed = ctx.createGain() // drone and wind: mostly dry, a little room
    bed.connect(master)
    bed.connect(reverb)
    eventBus = ctx.createGain() // distant events: almost all reverb
    eventBus.gain.value = 0.8
    eventBus.connect(reverb)
    nodes = [...buildDrone(ctx, bed), ...buildWind(ctx, bed)]
  }

  const scheduleEvent = () => {
    timer = setTimeout(() => {
      if (!running || !ctx || !eventBus) return
      const play = EVENTS[Math.floor(Math.random() * EVENTS.length)]
      play(ctx, eventBus, ctx.currentTime + 0.05)
      scheduleEvent()
    }, rand(...EVENT_GAP) * 1000)
  }

  return {
    start() {
      if (running) return
      running = true
      try {
        if (!ctx) build()
      } catch {
        running = false
        return
      }
      // Browsers keep a context made before any click suspended; the first
      // click or key anywhere on the page lets it start.
      const c = ctx!
      void c.resume()
      if (c.state !== 'running') {
        const unlock = () => {
          if (running) void c.resume()
        }
        window.addEventListener('pointerdown', unlock, { once: true })
        window.addEventListener('keydown', unlock, { once: true })
      }
      ramp(level(), FADE)
      scheduleEvent()
    },
    stop() {
      if (!running) return
      running = false
      if (timer) clearTimeout(timer)
      ramp(0, FADE)
      const c = ctx
      setTimeout(() => {
        if (!running) void c?.suspend()
      }, FADE * 1000 + 100)
    },
    setVolume(v: number) {
      volume = Math.min(1, Math.max(0, v))
      if (running) ramp(level(), 0.3)
    },
    duck(d: boolean) {
      if (ducked === d) return
      ducked = d
      if (running) ramp(level(), d ? 0.8 : 2.5)
    },
    destroy() {
      running = false
      if (timer) clearTimeout(timer)
      nodes.forEach((n) => {
        if (n instanceof AudioScheduledSourceNode) n.stop()
      })
      void ctx?.close()
      ctx = null
      master = null
      eventBus = null
      nodes = []
    },
  }
}
