/*
 * Ambient rain loop.
 *
 * Plays public/audio/rain.ogg (CC0, see LICENSE-Ylmir-rain.txt beside it)
 * through Web Audio rather than an <audio loop> element. That matters for
 * seamlessness: `<audio>` looping is not gapless, because the decoder honours
 * the encoder's padding and ticks on every wrap. Decoding once into an
 * AudioBuffer and looping an AudioBufferSourceNode is sample-accurate, so the
 * wrap is silent.
 *
 * The file is only fetched when the effect is first switched on, so nobody who
 * never sees rain pays for the download.
 */

export interface RainAudioOptions {
  /** Overall level, 0 to 1. Ambient, so this wants to stay low. */
  volume?: number
  /** Seconds to fade in and out, so it never clicks on or off. */
  fade?: number
  /** Path to the loop. */
  src?: string
}

export interface RainAudioInstance {
  /** Load if needed, then fade in. Safe to call repeatedly. */
  start: () => Promise<void>
  /** Fade out, keeping the decoded buffer so restarting is instant. */
  stop: () => void
  setVolume: (volume: number) => void
  destroy: () => void
}

const DEFAULTS: Required<RainAudioOptions> = {
  volume: 0.35,
  fade: 1.6,
  src: '/audio/rain.ogg',
}

type WindowWithWebkitAudio = Window & { webkitAudioContext?: typeof AudioContext }

/**
 * Crossfades the tail of the buffer into its head.
 *
 * The source is authored as loopable, but a butt-join between two arbitrary
 * points in a recording can still tick. Rain is noise-like, so overlapping a
 * fraction of a second is inaudible and removes any chance of a click.
 */
function smoothLoopSeam(buffer: AudioBuffer, seconds = 0.25) {
  const blend = Math.min(Math.floor(buffer.sampleRate * seconds), Math.floor(buffer.length / 4))
  if (blend <= 0) return
  for (let channel = 0; channel < buffer.numberOfChannels; channel++) {
    const data = buffer.getChannelData(channel)
    for (let i = 0; i < blend; i++) {
      const t = i / blend
      data[i] = data[i] * t + data[data.length - blend + i] * (1 - t)
    }
  }
}

/** Builds the audio graph. Returns null when Web Audio is unavailable. */
export function createRainAudio(options: RainAudioOptions = {}): RainAudioInstance | null {
  const config = { ...DEFAULTS, ...options }
  const Ctx = window.AudioContext ?? (window as WindowWithWebkitAudio).webkitAudioContext
  if (!Ctx) return null

  let ctx: AudioContext
  try {
    ctx = new Ctx()
  } catch {
    return null
  }

  const master = ctx.createGain()
  master.gain.value = 0
  master.connect(ctx.destination)

  let source: AudioBufferSourceNode | null = null
  let loading: Promise<AudioBuffer | null> | null = null
  let destroyed = false
  let running = false
  let waitingForGesture: (() => void) | null = null

  function load(): Promise<AudioBuffer | null> {
    if (!loading) {
      loading = fetch(config.src)
        .then((response) => {
          if (!response.ok) throw new Error(`${response.status} ${response.statusText}`)
          return response.arrayBuffer()
        })
        .then((data) => ctx.decodeAudioData(data))
        .then((buffer) => {
          smoothLoopSeam(buffer)
          return buffer
        })
        .catch((error) => {
          console.warn('Unable to load rain audio', error)
          return null
        })
    }
    return loading
  }

  function clearGestureWait() {
    if (!waitingForGesture) return
    window.removeEventListener('pointerdown', waitingForGesture)
    window.removeEventListener('keydown', waitingForGesture)
    waitingForGesture = null
  }

  // An AudioContext created outside a user gesture starts suspended. Rather
  // than fail silently, wait for the next interaction and pick up then.
  function resume(): Promise<void> {
    if (ctx.state === 'running') return Promise.resolve()
    return ctx.resume().catch(
      () =>
        new Promise<void>((resolve) => {
          clearGestureWait()
          waitingForGesture = () => {
            clearGestureWait()
            ctx.resume().then(resolve, resolve)
          }
          window.addEventListener('pointerdown', waitingForGesture, { once: true })
          window.addEventListener('keydown', waitingForGesture, { once: true })
        }),
    )
  }

  function fadeTo(value: number, seconds: number) {
    const now = ctx.currentTime
    master.gain.cancelScheduledValues(now)
    master.gain.setValueAtTime(master.gain.value, now)
    master.gain.linearRampToValueAtTime(value, now + seconds)
  }

  return {
    async start() {
      if (destroyed || running) return
      running = true

      const buffer = await load()
      if (destroyed || !running || !buffer) return

      await resume()
      if (destroyed || !running) return

      if (!source) {
        source = ctx.createBufferSource()
        source.buffer = buffer
        source.loop = true
        source.connect(master)
        source.start()
      }
      fadeTo(config.volume, config.fade)
    },
    stop() {
      if (destroyed || !running) return
      running = false
      clearGestureWait()
      fadeTo(0, config.fade)
    },
    setVolume(volume) {
      config.volume = volume
      if (destroyed || !running) return
      fadeTo(volume, 0.3)
    },
    destroy() {
      if (destroyed) return
      destroyed = true
      running = false
      clearGestureWait()
      if (source) {
        try {
          source.stop()
        } catch {
          /* already stopped */
        }
        source.disconnect()
        source = null
      }
      master.disconnect()
      void ctx.close()
    },
  }
}
