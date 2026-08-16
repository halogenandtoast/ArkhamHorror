/*
 * Laser Beam — a WebGL2 beam drawn along a canvas's horizontal axis.
 *
 * Adapted from the React component at https://canvasui.dev/docs/components/laser
 * (`@canvas-ui/laser-react`, a copy-in shadcn registry component with no npm
 * dependencies). The noise helpers and the beam core/glow math are upstream; the
 * rest is not, because upstream solves a different problem.
 *
 * Upstream's Laser is a page decoration: one horizontal beam pinned near the
 * bottom of a scrolling container, with a "reveal band" that heats and shimmers
 * content scrolling up past it. It has no concept of two endpoints, and it takes
 * a single solid colour.
 *
 * What we need is a connector between two locations on the map, in the Cosmic
 * Emissary's teal. So:
 *
 *  - The beam always runs along the canvas's horizontal centre line. Callers
 *    size the canvas to the connection's length and rotate it into place with a
 *    CSS transform, so the beam can join any two points at any angle.
 *  - The scroll-reveal machinery is gone (with it, the `html-in-canvas` path —
 *    see flameWrap.ts for that story).
 *  - `uColor` becomes a five-stop ramp sampled along the beam and scrolled over
 *    time, which is how the existing SVG line carries its colour: a repeating
 *    linearGradient moved by an animateTransform.
 */

export type Rgb = [number, number, number]

export interface LaserBeamOptions {
  /**
   * Five gradient stops sampled along the beam, each rgb in the 0-1 range. The
   * last should equal the first or the repeat will show a seam.
   */
  stops?: [Rgb, Rgb, Rgb, Rgb, Rgb]
  /** Length in CSS pixels of one full pass through the stops. */
  patternLength?: number
  /** Gradient scroll speed, in pattern-lengths per second. */
  flow?: number
  /** Animation speed of the beam wave and flicker. 1 is normal. */
  speed?: number
  /** Thickness of the white-hot beam core in CSS pixels. */
  thickness?: number
  /** Intensity of the white beam core (0 to 2). 0 removes it. */
  core?: number
  /** Reach of the coloured glow around the beam in CSS pixels. */
  radius?: number
  /** Brightness of the coloured glow (0 to 3). 0 removes it. */
  glow?: number
  /** Amplitude of the slow beam waviness in CSS pixels. */
  wave?: number
  /** Random intensity flicker of the beam (0 to 1). */
  flicker?: number
  /**
   * How much of the beam's length stays at full strength before it tapers off.
   * 0.5 fades to nothing exactly at both ends; higher keeps the ends brighter.
   */
  taper?: number
  /**
   * Chroma boost applied to the sampled ramp. 1 leaves the stops alone; above 1
   * pushes them away from grey. Worth having because the beam's own tone
   * mapping washes colour out as it brightens, and two of the Emissary's stops
   * (#DDF2EB especially) are pale enough to read as white on their own.
   */
  saturation?: number
}

export interface LaserBeamInstance {
  setOptions: (options: LaserBeamOptions) => void
  /** Re-read the canvas size. Call after moving or resizing the beam. */
  resize: () => void
  destroy: () => void
}

/** The Cosmic Emissary's teal, matching the SVG gradient it replaces. */
export const COSMIC_EMISSARY_STOPS: [Rgb, Rgb, Rgb, Rgb, Rgb] = [
  [0.533, 0.678, 0.643], // #88ADA4
  [0.212, 0.4, 0.447], // #366672
  [0.867, 0.949, 0.922], // #DDF2EB
  [0.518, 0.792, 0.78], // #84CAC7
  [0.533, 0.678, 0.643], // #88ADA4 again, closing the loop
]

// Dialled well down from upstream's (thickness 6, core 1, radius 20, glow 2),
// which assume a beam running the full width of a viewport. Ours is ~40px tall,
// and at upstream's settings the glow saturates to white and swallows the teal
// entirely — the one thing this beam exists to show.
const DEFAULTS: Required<LaserBeamOptions> = {
  stops: COSMIC_EMISSARY_STOPS,
  patternLength: 96,
  flow: 0.14,
  speed: 1,
  thickness: 1.6,
  // A hot white core reads as "laser", but every bit of it desaturates the
  // teal, so it is kept to a thin filament and the colour carried by the glow.
  core: 0.12,
  radius: 5,
  glow: 1.5,
  wave: 3,
  flicker: 0.18,
  taper: 0.9,
  saturation: 2.4,
}

const VERT = `#version 300 es
precision highp float;
layout(location = 0) in vec2 aPos;
out vec2 vUv;
void main () {
  vUv = aPos * 0.5 + 0.5;
  gl_Position = vec4(aPos, 0.0, 1.0);
}`

const FRAG = `#version 300 es
precision highp float;
in vec2 vUv;
out vec4 outColor;
uniform vec2 uResolution;
uniform float uTime;
uniform float uWaveAmp;
uniform float uHalfW;
uniform float uHalfCore;
uniform float uCore;
uniform float uRadius;
uniform float uGlow;
uniform vec3 uStops[5];
uniform float uRepeat;
uniform float uFlow;
uniform float uBright;
uniform float uSaturation;
uniform vec2 uFade;

float hash (vec2 v) {
  return fract(sin(dot(v, vec2(89.44, 19.36))) * 22189.22);
}

float iHash (vec2 v, vec2 r) {
  float h00 = hash(floor(v * r + vec2(0.0, 0.0)) / r);
  float h10 = hash(floor(v * r + vec2(1.0, 0.0)) / r);
  float h01 = hash(floor(v * r + vec2(0.0, 1.0)) / r);
  float h11 = hash(floor(v * r + vec2(1.0, 1.0)) / r);
  vec2 ip = smoothstep(vec2(0.0), vec2(1.0), mod(v * r, 1.0));
  return (h00 * (1.0 - ip.x) + h10 * ip.x) * (1.0 - ip.y)
    + (h01 * (1.0 - ip.x) + h11 * ip.x) * ip.y;
}

float noise (vec2 v) {
  float sum = 0.0;
  float s = 2.0;
  for (int i = 1; i < 7; i++) {
    sum += iHash(v + vec2(i), vec2(2.0 * s)) / s;
    s *= 2.0;
  }
  return sum;
}

vec3 permute (vec3 x) { return mod(((x * 34.0) + 1.0) * x, 289.0); }

float snoise (vec2 v) {
  const vec4 C = vec4(0.211324865405187, 0.366025403784439,
    -0.577350269189626, 0.024390243902439);
  vec2 i = floor(v + dot(v, C.yy));
  vec2 x0 = v - i + dot(i, C.xx);
  vec2 i1 = (x0.x > x0.y) ? vec2(1.0, 0.0) : vec2(0.0, 1.0);
  vec4 x12 = x0.xyxy + C.xxzz;
  x12.xy -= i1;
  i = mod(i, 289.0);
  vec3 p = permute(permute(i.y + vec3(0.0, i1.y, 1.0)) + i.x + vec3(0.0, i1.x, 1.0));
  vec3 m = max(0.5 - vec3(dot(x0, x0), dot(x12.xy, x12.xy), dot(x12.zw, x12.zw)), 0.0);
  m = m * m;
  m = m * m;
  vec3 x = 2.0 * fract(p * C.www) - 1.0;
  vec3 h = abs(x) - 0.5;
  vec3 ox = floor(x + 0.5);
  vec3 a0 = x - ox;
  m *= 1.79284291400159 - 0.85373472095314 * (a0 * a0 + h * h);
  vec3 g;
  g.x = a0.x * x0.x + h.x * x0.y;
  g.yz = a0.yz * x12.xz + h.yz * x12.yw;
  return 130.0 * dot(m, g);
}

float fbm (vec2 p) {
  float v = 0.0;
  float a = 0.5;
  for (int i = 0; i < 3; i++) {
    v += a * snoise(p);
    p = mat2(1.6, 1.2, -1.2, 1.6) * p + 11.7;
    a *= 0.5;
  }
  return v * 0.5 + 0.5;
}

// Five stops, four intervals, sampled with the pattern repeating along the beam.
vec3 rampColor (float p) {
  float x = fract(p) * 4.0;
  int i = int(floor(x));
  vec3 c = mix(uStops[i], uStops[i + 1], fract(x));
  float l = dot(c, vec3(0.2126, 0.7152, 0.0722));
  return max(mix(vec3(l), c, uSaturation), vec3(0.0));
}

void main () {
  vec2 uv = vUv;
  float t = uTime;

  // Distance from the middle of the beam's length, so the ends can taper.
  float nx = (uv.x - 0.5) / max(uHalfW, 1e-4);
  float env = pow(max(1.0 - nx * nx, 0.0), 1.5);
  if (env <= 0.0) {
    outColor = vec4(0.0);
    return;
  }

  float bend = 0.0;
  if (uWaveAmp > 0.0) {
    bend = (noise(vec2(uv.x * 2.5 + t * 0.6, t * 0.4)) - 0.5) * 2.0 * uWaveAmp;
  }
  // The beam runs along the canvas's horizontal centre line.
  float dy = uv.y - (0.5 + bend);
  float pxd = abs(dy) * uResolution.y;

  vec3 color = rampColor(uv.x * uRepeat - t * uFlow);

  vec3 beam = vec3(0.0);
  float pd = pxd / max(env, 0.18);
  if (uCore > 0.0) {
    beam += 10.0 * uCore * smoothstep(uHalfCore, uHalfCore * 0.3, pd) * vec3(1.0);
  }
  if (uGlow > 0.0) {
    float g = pow(uRadius / max(pd, 0.75), 0.9) * exp(-0.55 * pd / uRadius);
    beam += uGlow * g * color;
  }
  beam *= uBright;

  vec3 toned = (1.0 - exp(-beam)) * env;
  float alpha = max(max(toned.r, toned.g), toned.b);

  // The glow is still faintly lit where it meets the canvas border, and the
  // taper leaves the two ends part-lit, so without this the beam terminates on
  // a visible rectangle. Ease everything to zero at all four edges.
  vec2 frag = uv * uResolution;
  vec2 edgePx = min(frag, uResolution - frag);
  float edge = smoothstep(0.0, max(uFade.x, 1.0), edgePx.x)
    * smoothstep(0.0, max(uFade.y, 1.0), edgePx.y);

  outColor = vec4(toned * edge, clamp(alpha * edge, 0.0, 1.0));
}`

/** Cheap CPU mirror of the shader's noise, used to drive the flicker. */
function hashCpu(vx: number, vy: number) {
  const s = Math.sin(vx * 89.44 + vy * 19.36) * 22189.22
  return s - Math.floor(s)
}

function iHashCpu(vx: number, vy: number, r: number) {
  const fx = Math.floor(vx * r) / r
  const fy = Math.floor(vy * r) / r
  return hashCpu(fx, fy)
}

function noiseCpu(vx: number, vy: number) {
  let sum = 0
  let s = 2
  for (let i = 1; i < 7; i++) {
    sum += iHashCpu(vx + i, vy + i, 2 * s) / s
    s *= 2
  }
  return sum
}

/**
 * Attaches a beam to `output`, running along its horizontal centre line.
 *
 * Returns null when WebGL2 is unavailable or the context cannot be created, so
 * callers can fall back to whatever they drew before.
 */
export function createLaserBeam(
  output: HTMLCanvasElement,
  options: LaserBeamOptions = {},
): LaserBeamInstance | null {
  const config = { ...DEFAULTS, ...options }

  const gl = output.getContext('webgl2', {
    alpha: true,
    depth: false,
    stencil: false,
    antialias: false,
    premultipliedAlpha: true,
  })
  if (!gl || gl.isContextLost()) return null

  let failed = false
  function compile(type: number, text: string): WebGLShader {
    const shader = gl!.createShader(type)!
    gl!.shaderSource(shader, text)
    gl!.compileShader(shader)
    if (!gl!.getShaderParameter(shader, gl!.COMPILE_STATUS)) {
      console.error('LaserBeam shader error:', gl!.getShaderInfoLog(shader))
      failed = true
    }
    return shader
  }

  const vertexShader = compile(gl.VERTEX_SHADER, VERT)
  const fragmentShader = compile(gl.FRAGMENT_SHADER, FRAG)
  const program = gl.createProgram()!
  gl.attachShader(program, vertexShader)
  gl.attachShader(program, fragmentShader)
  gl.linkProgram(program)
  if (!gl.getProgramParameter(program, gl.LINK_STATUS)) {
    console.error('LaserBeam link error:', gl.getProgramInfoLog(program))
    failed = true
  }

  // A broken shader would render nothing at all, which is worse than the SVG
  // line we replaced. Tell the caller so it can put that line back.
  if (failed) {
    gl.deleteProgram(program)
    gl.deleteShader(vertexShader)
    gl.deleteShader(fragmentShader)
    return null
  }

  const uniforms: Record<string, WebGLUniformLocation> = {}
  const uniformCount = gl.getProgramParameter(program, gl.ACTIVE_UNIFORMS)
  for (let i = 0; i < uniformCount; i++) {
    const info = gl.getActiveUniform(program, i)!
    // Array uniforms report as "uStops[0]"; store the base name too.
    const name = info.name.replace(/\[0\]$/, '')
    uniforms[name] = gl.getUniformLocation(program, info.name)!
  }

  const quad = gl.createBuffer()
  gl.bindBuffer(gl.ARRAY_BUFFER, quad)
  gl.bufferData(gl.ARRAY_BUFFER, new Float32Array([-1, -1, 1, -1, -1, 1, 1, 1]), gl.STATIC_DRAW)
  gl.enableVertexAttribArray(0)
  gl.vertexAttribPointer(0, 2, gl.FLOAT, false, 0, 0)

  let dpr = 1

  function syncCanvasSize() {
    dpr = Math.min(window.devicePixelRatio || 1, 2)
    const width = Math.max(1, Math.round(output.clientWidth * dpr))
    const height = Math.max(1, Math.round(output.clientHeight * dpr))
    if (output.width !== width || output.height !== height) {
      output.width = width
      output.height = height
    }
  }

  syncCanvasSize()

  const stopValues = new Float32Array(15)
  let time = 0

  function render() {
    const clientWidth = Math.max(output.clientWidth, 1)
    const clientHeight = Math.max(output.clientHeight, 1)

    for (let i = 0; i < 5; i++) {
      const stop = config.stops[i] ?? config.stops[config.stops.length - 1]
      stopValues[i * 3] = stop[0]
      stopValues[i * 3 + 1] = stop[1]
      stopValues[i * 3 + 2] = stop[2]
    }

    gl!.useProgram(program)
    gl!.uniform2f(uniforms.uResolution, output.width, output.height)
    gl!.uniform1f(uniforms.uTime, time)
    gl!.uniform1f(uniforms.uWaveAmp, Math.max(config.wave, 0) / clientHeight)
    gl!.uniform1f(uniforms.uHalfW, Math.max(config.taper, 0.05))
    gl!.uniform1f(uniforms.uHalfCore, Math.max(config.thickness, 0.5) * dpr * 0.5)
    gl!.uniform1f(uniforms.uCore, Math.max(config.core, 0))
    gl!.uniform1f(uniforms.uRadius, Math.max(config.radius, 0.5) * dpr)
    gl!.uniform1f(uniforms.uGlow, Math.max(config.glow, 0))
    gl!.uniform3fv(uniforms.uStops, stopValues)
    // Repeats across the beam, so the pattern keeps a fixed on-screen length
    // however long the connection is.
    gl!.uniform1f(uniforms.uRepeat, clientWidth / Math.max(config.patternLength, 1))
    gl!.uniform1f(uniforms.uFlow, Math.max(config.flow, 0))
    gl!.uniform1f(uniforms.uSaturation, Math.max(config.saturation, 0))
    // Across the beam, fade over most of the half-height so the glow has room
    // to die out. Along it, only the very ends, so the beam still reads as
    // reaching both locations.
    gl!.uniform2f(
      uniforms.uFade,
      Math.min(clientWidth * 0.12, 48) * dpr,
      clientHeight * 0.42 * dpr,
    )
    gl!.uniform1f(
      uniforms.uBright,
      1 - Math.min(Math.max(config.flicker, 0), 1) * noiseCpu(time * 1.8, 3.7),
    )
    gl!.bindFramebuffer(gl!.FRAMEBUFFER, null)
    gl!.viewport(0, 0, output.width, output.height)
    gl!.drawArrays(gl!.TRIANGLE_STRIP, 0, 4)
  }

  let raf = 0
  let lastTime = performance.now()
  let destroyed = false
  let running = false
  let visible = true

  function frame(now: number) {
    if (destroyed) return
    if (!visible) {
      running = false
      return
    }
    const delta = Math.min((now - lastTime) / 1000, 1 / 30)
    lastTime = now
    time += delta * config.speed
    render()
    raf = requestAnimationFrame(frame)
  }

  function start() {
    if (destroyed || running || !visible) return
    running = true
    lastTime = performance.now()
    raf = requestAnimationFrame(frame)
  }

  start()

  const observer = new ResizeObserver(() => {
    syncCanvasSize()
    start()
  })
  observer.observe(output)

  const intersection = new IntersectionObserver((entries) => {
    visible = entries[entries.length - 1]?.isIntersecting ?? true
    if (visible) start()
  })
  intersection.observe(output)

  return {
    setOptions(next) {
      Object.assign(config, next)
      syncCanvasSize()
      start()
    },
    resize() {
      syncCanvasSize()
      start()
    },
    destroy() {
      destroyed = true
      cancelAnimationFrame(raf)
      observer.disconnect()
      intersection.disconnect()
      gl!.deleteProgram(program)
      gl!.deleteShader(vertexShader)
      gl!.deleteShader(fragmentShader)
      gl!.deleteBuffer(quad)
      // No WEBGL_lose_context here — see the note in flameWrap.ts's destroy.
    },
  }
}
