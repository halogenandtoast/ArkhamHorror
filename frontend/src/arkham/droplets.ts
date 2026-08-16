/*
 * Droplets — rain running down the screen, refracting the board behind it.
 *
 * Ported from the React component at https://canvasui.dev/docs/components/droplets
 * (`@canvas-ui/droplets-react`, a copy-in shadcn registry component with no npm
 * dependencies). The GLSL is verbatim upstream so it stays diffable.
 *
 * This effect only runs where `html-in-canvas` is available. That API — the
 * `layoutsubtree` attribute plus `ctx.drawElementImage()` and
 * `canvas.requestPaint()` — is what lets the shader sample the real page and
 * bend it through each drop. Without it the drops have nothing to refract and
 * read as flat stickers, so callers gate on supportsHtmlInCanvas() and simply
 * render no rain rather than a worse version of it.
 *
 * The GLSL is upstream's, unmodified. The upstream fallback branch (see
 * `uHasContent` in the shader) is therefore still present as a safety net if the
 * capability probe and the real context ever disagree.
 *
 * One thing upstream does that we do not: the pointer wipe. It renders a trail
 * into a second framebuffer and samples it to rub drops off the glass under the
 * cursor. We want no cursor interaction, so that whole pass, its framebuffer and
 * the pointer listeners are gone, and `uTrail` is bound to a 1x1 black texture
 * so the wipe terms read zero. The uniforms stay so the GLSL still diffs.
 */

export interface DropletsOptions {
  /** How much rain there is (0 to 1.25). */
  intensity?: number
  /** Animation speed multiplier. */
  speed?: number
  /** Droplet pattern size; higher means smaller drops. */
  scale?: number
  /** Width of droplets and their trails. */
  dropWidth?: number
  /** Elongation of falling droplets. */
  dropLength?: number
  /** Descent velocity of the running drops. */
  fallSpeed?: number
  /** Horizontal wander of the falling droplets. */
  wiggle?: number
  /** Multiplier for the stationary droplets. */
  staticDrops?: number
  /** How strongly each drop bends the content behind it, in uv units. */
  refraction?: number
  /** Fogging of the content behind the drops. */
  blur?: number
  /** Edge darkening of the refracted content (0 to 1). */
  vignette?: number
  /** Colour wash over the drops, rgb in the 0-1 range. */
  tint?: [number, number, number]
  /** How strongly the tint applies (0 to 1). */
  tintStrength?: number
}

export interface DropletsElements {
  /** Canvas with `layoutsubtree` that hosts the page content. */
  source: HTMLCanvasElement
  /** The element inside the source canvas that gets captured and refracted. */
  content: HTMLElement
  /** Canvas the WebGL effect renders to. */
  output: HTMLCanvasElement
}

export interface DropletsInstance {
  setOptions: (options: DropletsOptions) => void
  resize: () => void
  destroy: () => void
}

const DEFAULTS: Required<DropletsOptions> = {
  intensity: 0.5,
  speed: 1,
  scale: 0.4,
  dropWidth: 1,
  dropLength: 1,
  fallSpeed: 1,
  wiggle: 1,
  staticDrops: 0.2,
  refraction: 0.2,
  blur: 0,
  vignette: 0,
  tint: [1, 1, 1],
  tintStrength: 0,
}

type PaintableCanvas = HTMLCanvasElement & {
  onpaint?: (() => void) | null
  requestPaint?: () => void
}

type ElementImageContext = CanvasRenderingContext2D & {
  drawElementImage?: (element: Element, x: number, y: number) => void
}

/**
 * True when the browser exposes html-in-canvas, i.e. when the rain can actually
 * refract the page. Chrome ships this behind a flag; everywhere else it is
 * false and callers should render no rain at all.
 */
export function supportsHtmlInCanvas(): boolean {
  if (typeof document === 'undefined') return false
  const probe = document.createElement('canvas') as PaintableCanvas
  const ctx = probe.getContext('2d') as ElementImageContext | null
  return Boolean(
    ctx && typeof ctx.drawElementImage === 'function' && typeof probe.requestPaint === 'function',
  )
}

const VERT = `#version 300 es
precision highp float;
layout(location = 0) in vec2 aPos;
out vec2 vUv;
void main () {
  vUv = aPos * 0.5 + 0.5;
  gl_Position = vec4(aPos, 0.0, 1.0);
}`;

const FRAG = `#version 300 es
precision highp float;
in vec2 vUv;
out vec4 outColor;
uniform sampler2D uContent;
uniform vec2 uResolution;
uniform vec2 uOffset;
uniform float uTime;
uniform float uIntensity;
uniform float uScale;
uniform float uDropWidth;
uniform float uDropLength;
uniform float uRefraction;
uniform float uBlur;
uniform float uVignette;
uniform float uFallSpeed;
uniform float uWiggle;
uniform float uStaticDrops;
uniform float uMaxX;
uniform sampler2D uTrail;
uniform float uWipe;
uniform float uWipeDistort;
uniform vec3 uTint;
uniform float uTintStrength;
uniform float uHasContent;

#define S(a, b, t) smoothstep(a, b, t)

vec3 N13 (float p) {
  vec3 p3 = fract(vec3(p) * vec3(0.1031, 0.11369, 0.13787));
  p3 += dot(p3, p3.yzx + 19.19);
  return fract(vec3(
    (p3.x + p3.y) * p3.z,
    (p3.x + p3.z) * p3.y,
    (p3.y + p3.z) * p3.x
  ));
}

float N (float t) {
  return fract(sin(t * 12345.564) * 7658.76);
}

float Saw (float b, float t) {
  return S(0.0, b, t) * S(1.0, b, t);
}

float sdEgg (vec2 p, float ra, float rb) {
  const float k = 1.7320508;
  p.x = abs(p.x);
  float r = ra - rb;
  return ((p.y < 0.0) ? length(vec2(p.x, p.y)) - r :
          (k * (p.x + r) < p.y) ? length(vec2(p.x, p.y - k * r)) :
          length(vec2(p.x + r, p.y)) - 2.0 * r) - rb;
}

vec2 DropLayer (vec2 uv, float t) {
  vec2 UV = uv;
  vec2 a = vec2(6.0, 1.0);
  vec2 grid = a * 2.0;

  vec2 id = floor(uv * grid);
  float gridFall = N(id.x) / 3.0 + 0.5;
  uv.y += t * gridFall / a.y;
  id = floor(uv * grid);
  uv.y += N(id.x);

  id = floor(uv * grid);
  vec2 st = fract(uv * grid) - vec2(0.5, 0.0);
  vec3 n = N13(id.x * 35.2 + id.y * 2376.1);

  float x = n.x - 0.5;
  float lambda = UV.y * 20.0;
  float wiggle = sin(lambda + sin(lambda));
  x += wiggle * (0.5 - abs(x)) * (n.z - 0.5) * uWiggle;
  x *= 0.6;

  float slowStart = 0.85;
  float ti = fract(t * (gridFall + 0.1) + n.z);
  float y = (Saw(slowStart, ti) - 0.5) * 0.9 + 0.5;
  vec2 p = vec2(x, y);

  float dropShape = (ti > slowStart)
    ? -sin(6.2831853 * ti / (1.0 - slowStart)) * 0.5 - 0.5
    : 0.0;
  float d = sdEgg((st - p) * a.yx / vec2(uDropWidth, uDropLength), 0.0, dropShape);
  float diameter = N(id.x + id.y) / 7.0 + 0.2;
  float mainDrop = S(diameter / 1.5, 0.0, d);

  float r2 = S(1.0, y, st.y);
  float r = sqrt(r2);
  float cd = abs(st.x - x);
  float thickness = diameter * 0.95 * uDropWidth;
  float trail = S(thickness * r, 0.0, cd);
  float trailFront = S(-0.02, 0.02, st.y - y);
  trail *= r2 * trailFront * 0.5;

  y = UV.y;
  float trail2 = S((thickness - 0.15) * r, 0.0, cd);
  trail2 *= trailFront * n.z;
  float rndX = N(id.x) / 1.5 + 0.5;
  float rndY = N(st.y) / 40.0 + 0.05;
  y = fract(y * 11.0 * rndX) + (st.y - 0.5);
  float dd = length(st - vec2(x, y));
  float droplets = S(trail2 + rndY, 0.0, dd);

  float m = mainDrop + droplets * r * trailFront;
  return vec2(m, trail);
}

float StaticDrops (vec2 uv, float t) {
  uv *= 40.0;

  vec2 id = floor(uv);
  vec3 n = N13(id.x * 107.45 + id.y * 3543.654);
  vec2 p = (n.xy - 0.5) * 0.6;
  uv = fract(uv) - 0.5;

  float d = length(uv - p);
  float drop = S(0.3 * clamp(uDropWidth, 0.4, 1.4), 0.0, d);

  float fade = Saw(0.1, fract(t + n.y));
  float intensity = fract(n.x * 27.0);
  return drop * fade * intensity;
}

vec2 Drops (vec2 uv, float t, float tFall, float l0, float l1, float l2, float wipe) {
  float s = StaticDrops(uv, t) * l0 * (1.0 - wipe);
  vec2 m1 = DropLayer(uv, tFall) * (l1 * (1.0 - wipe * 0.8));
  vec2 m2 = DropLayer(uv * 1.85, tFall) * (l2 * (1.0 - wipe * 0.8));

  float c = s + m1.x + m2.x;
  c = S(0.3, 1.0, c);

  return vec2(c, m1.y + m2.y);
}

void main () {
  vec2 uv = vUv;

  if (uv.x > uMaxX) {
    outColor = vec4(0.0);
    return;
  }

  vec2 aspectUv = (uv + uOffset - 0.5) * vec2(uResolution.x / uResolution.y, 1.0);
  float t = uTime * 0.2;
  float dropScale = clamp(min(uResolution.x, uResolution.y) / 900.0, 0.75, 1.35) * uScale;
  vec2 scaledUv = aspectUv * dropScale;

  float rainAmount = clamp(uIntensity, 0.0, 1.25);

  float staticDrops = S(-0.5, 1.0, rainAmount) * 2.0 * uStaticDrops;
  float layer1 = S(0.25, 0.75, rainAmount);
  float layer2 = S(0.0, 0.5, rainAmount);
  float tFall = t * uFallSpeed;

  float wipeMask = texture(uTrail, uv).r;
  float wipe = wipeMask * clamp(uWipe, 0.0, 1.0);

  vec2 c = Drops(scaledUv, t, tFall, staticDrops, layer1, layer2, wipe);

  vec2 e = vec2(0.001, 0.0);
  float cx = Drops(scaledUv + e, t, tFall, staticDrops, layer1, layer2, wipe).x;
  float cy = Drops(scaledUv + e.yx, t, tFall, staticDrops, layer1, layer2, wipe).x;
  vec2 normal = vec2(cx - c.x, cy - c.x);

  vec2 e2 = vec2(0.012, 0.0);
  float wx = texture(uTrail, uv + e2).r;
  float wy = texture(uTrail, uv + e2.yx).r;
  normal += vec2(wipeMask - wx, wipeMask - wy) * 0.05 * uWipeDistort * clamp(uWipe, 0.0, 1.0);

  vec2 refractedUv = clamp(uv + normal * uRefraction, vec2(0.001), vec2(uMaxX - 0.004, 0.999));
  float fog = clamp(uBlur, 0.0, 8.0) * mix(0.7, 1.0, rainAmount);
  float back = fog * (1.0 - clamp(c.y * 2.0, 0.0, 1.0)) * (1.0 - wipe);
  float focus = mix(back, 0.0, S(0.1, 0.2, c.x));

  if (uHasContent < 0.5) {
    float mask = S(0.02, 0.14, c.x);
    vec3 n3 = normalize(vec3(normal * 42.0, 1.0));
    vec3 L = normalize(vec3(-0.35, 0.75, 0.55));
    float spec = pow(max(dot(reflect(vec3(0.0, 0.0, -1.0), n3), L), 0.0), 34.0);
    float rim = clamp(length(normal) * 26.0, 0.0, 1.0);
    vec3 dropCol = mix(vec3(0.72), uTint, clamp(uTintStrength, 0.0, 1.0));
    vec3 colF = dropCol * (0.12 + 0.5 * rim) + vec3(spec);
    float alphaF = mask * clamp(0.1 + rim * 0.5 + spec * 0.9, 0.0, 1.0);
    outColor = vec4(clamp(colF, 0.0, 1.0) * alphaF, alphaF);
    return;
  }

  vec4 content = textureLod(uContent, vec2(refractedUv.x, 1.0 - refractedUv.y), focus);
  vec3 col = content.rgb;

  col = mix(col, uTint, clamp(uTintStrength, 0.0, 1.0) * 0.35);

  vec2 vignetteUv = uv - 0.5;
  col *= 1.0 - dot(vignetteUv, vignetteUv) * clamp(uVignette, 0.0, 1.0) * 2.0;

  outColor = vec4(col * content.a, content.a);
}`;

/**
 * Attaches rain to `output`, refracting whatever is inside `content`.
 *
 * Returns null when WebGL2 is unavailable or the shaders fail to build.
 */
export function createDroplets(
  elements: DropletsElements,
  options: DropletsOptions = {},
): DropletsInstance | null {
  const config = { ...DEFAULTS, ...options }
  const { source, content, output } = elements

  const gl = output.getContext('webgl2', {
    alpha: true,
    depth: false,
    stencil: false,
    antialias: false,
    premultipliedAlpha: true,
  })
  if (!gl || gl.isContextLost()) return null

  const sourceCtx = source.getContext('2d') as ElementImageContext | null
  const paintable = source as PaintableCanvas
  const htmlInCanvas = Boolean(
    sourceCtx &&
      typeof sourceCtx.drawElementImage === 'function' &&
      typeof paintable.requestPaint === 'function',
  )

  let contentDirty = false
  let wake = () => {}

  // The browser calls this whenever the captured subtree repaints, which is how
  // the refracted image keeps up with the board.
  if (htmlInCanvas) {
    paintable.onpaint = () => {
      try {
        sourceCtx!.reset()
        sourceCtx!.drawElementImage!(content, 0, 0)
        contentDirty = true
        wake()
      } catch {
        /* a failed capture just leaves the previous frame in place */
      }
    }
  }

  let failed = false
  function compile(type: number, text: string): WebGLShader {
    const shader = gl!.createShader(type)!
    gl!.shaderSource(shader, text)
    gl!.compileShader(shader)
    if (!gl!.getShaderParameter(shader, gl!.COMPILE_STATUS)) {
      console.error('Droplets shader error:', gl!.getShaderInfoLog(shader))
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
    console.error('Droplets link error:', gl.getProgramInfoLog(program))
    failed = true
  }

  if (failed) {
    gl.deleteProgram(program)
    gl.deleteShader(vertexShader)
    gl.deleteShader(fragmentShader)
    if (htmlInCanvas) paintable.onpaint = null
    return null
  }

  const uniforms: Record<string, WebGLUniformLocation> = {}
  const uniformCount = gl.getProgramParameter(program, gl.ACTIVE_UNIFORMS)
  for (let i = 0; i < uniformCount; i++) {
    const info = gl.getActiveUniform(program, i)!
    uniforms[info.name.replace(/\[0\]$/, '')] = gl.getUniformLocation(program, info.name)!
  }

  const quad = gl.createBuffer()
  gl.bindBuffer(gl.ARRAY_BUFFER, quad)
  gl.bufferData(gl.ARRAY_BUFFER, new Float32Array([-1, -1, 1, -1, -1, 1, 1, 1]), gl.STATIC_DRAW)
  gl.enableVertexAttribArray(0)
  gl.vertexAttribPointer(0, 2, gl.FLOAT, false, 0, 0)

  // Mipmapped: the shader samples it with textureLod to fog the content behind
  // the drops.
  const contentTexture = gl.createTexture()!
  gl.bindTexture(gl.TEXTURE_2D, contentTexture)
  gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.LINEAR_MIPMAP_LINEAR)
  gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.LINEAR)
  gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE)
  gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE)
  gl.texImage2D(
    gl.TEXTURE_2D,
    0,
    gl.RGBA,
    1,
    1,
    0,
    gl.RGBA,
    gl.UNSIGNED_BYTE,
    new Uint8Array([0, 0, 0, 0]),
  )

  // Black, so every wipe term in the shader reads zero. Stands in for
  // upstream's pointer-trail framebuffer, which we do not have.
  const trailTexture = gl.createTexture()!
  gl.bindTexture(gl.TEXTURE_2D, trailTexture)
  gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.LINEAR)
  gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.LINEAR)
  gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE)
  gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE)
  gl.texImage2D(
    gl.TEXTURE_2D,
    0,
    gl.RGBA,
    1,
    1,
    0,
    gl.RGBA,
    gl.UNSIGNED_BYTE,
    new Uint8Array([0, 0, 0, 255]),
  )

  let contentMaxX = 1

  function syncCanvasSize() {
    const dpr = Math.min(window.devicePixelRatio || 1, 2)
    const width = Math.max(1, Math.round(output.clientWidth * dpr))
    const height = Math.max(1, Math.round(output.clientHeight * dpr))
    if (output.width !== width || output.height !== height) {
      output.width = width
      output.height = height
    }
    contentMaxX = Math.min(1, Math.max(0.05, content.clientWidth / Math.max(output.clientWidth, 1)))
    if (htmlInCanvas) {
      const cssWidth = Math.max(1, Math.round(source.clientWidth))
      const cssHeight = Math.max(1, Math.round(source.clientHeight))
      if (source.width !== cssWidth * dpr || source.height !== cssHeight * dpr) {
        source.width = cssWidth * dpr
        source.height = cssHeight * dpr
      }
      paintable.requestPaint!()
    }
  }

  syncCanvasSize()

  function uploadContent() {
    if (!htmlInCanvas || !contentDirty) return
    contentDirty = false
    gl!.bindTexture(gl!.TEXTURE_2D, contentTexture)
    gl!.texImage2D(gl!.TEXTURE_2D, 0, gl!.RGBA, gl!.RGBA, gl!.UNSIGNED_BYTE, source)
    gl!.generateMipmap(gl!.TEXTURE_2D)
  }

  let time = 0

  function render() {
    uploadContent()
    gl!.useProgram(program)
    gl!.activeTexture(gl!.TEXTURE0)
    gl!.bindTexture(gl!.TEXTURE_2D, contentTexture)
    gl!.uniform1i(uniforms.uContent, 0)
    gl!.activeTexture(gl!.TEXTURE1)
    gl!.bindTexture(gl!.TEXTURE_2D, trailTexture)
    gl!.uniform1i(uniforms.uTrail, 1)

    gl!.uniform2f(uniforms.uResolution, output.width, output.height)
    gl!.uniform2f(uniforms.uOffset, 0, -content.scrollTop / Math.max(content.clientHeight, 1))
    gl!.uniform1f(uniforms.uTime, time)
    gl!.uniform1f(uniforms.uIntensity, Math.max(config.intensity, 0))
    gl!.uniform1f(uniforms.uScale, Math.max(config.scale, 0.05))
    gl!.uniform1f(uniforms.uDropWidth, Math.max(config.dropWidth, 0.05))
    gl!.uniform1f(uniforms.uDropLength, Math.max(config.dropLength, 0.05))
    gl!.uniform1f(uniforms.uFallSpeed, Math.max(config.fallSpeed, 0))
    gl!.uniform1f(uniforms.uWiggle, Math.max(config.wiggle, 0))
    gl!.uniform1f(uniforms.uStaticDrops, Math.max(config.staticDrops, 0))
    gl!.uniform1f(uniforms.uRefraction, Math.max(config.refraction, 0))
    gl!.uniform1f(uniforms.uBlur, Math.max(config.blur, 0))
    gl!.uniform1f(uniforms.uVignette, Math.max(config.vignette, 0))
    gl!.uniform3f(uniforms.uTint, config.tint[0], config.tint[1], config.tint[2])
    gl!.uniform1f(uniforms.uTintStrength, Math.min(Math.max(config.tintStrength, 0), 1))
    gl!.uniform1f(uniforms.uMaxX, contentMaxX)
    gl!.uniform1f(uniforms.uHasContent, htmlInCanvas ? 1 : 0)
    // No pointer interaction, so there is never anything to wipe.
    gl!.uniform1f(uniforms.uWipe, 0)
    gl!.uniform1f(uniforms.uWipeDistort, 0)

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

  wake = start
  start()

  const observer = new ResizeObserver(() => {
    syncCanvasSize()
    start()
  })
  observer.observe(output)
  observer.observe(content)

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
      gl!.deleteTexture(contentTexture)
      gl!.deleteTexture(trailTexture)
      gl!.deleteProgram(program)
      gl!.deleteShader(vertexShader)
      gl!.deleteShader(fragmentShader)
      gl!.deleteBuffer(quad)
      if (htmlInCanvas) paintable.onpaint = null
      // No WEBGL_lose_context here — see the note in flameWrap.ts's destroy.
    },
  }
}
