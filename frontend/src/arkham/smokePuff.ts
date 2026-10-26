/*
 * Smoke Puff — a WebGL2 one-shot that implodes a portrait into a puff of
 * coloured smoke, and plays the same thing backwards to bring it back. A
 * Nightcrawler bamf.
 *
 * Written here rather than adapted from upstream like flameWrap.ts and
 * laserBeam.ts: those are standing decorations with a steady loop, and this is
 * an event. The differences follow from that.
 *
 *  - __One layer, many puffs.__ A WebGL context costs far too much to create
 *    per vanishing portrait, and browsers cap how many a page may hold, so the
 *    layer owns a single context over a viewport-sized canvas and draws every
 *    live puff into it as its own quad. Nothing is drawn, and no frame is
 *    scheduled, while the list is empty.
 *  - __Two timelines, not one reversed one.__ `uProgress` is the portrait: 0
 *    intact, 1 entirely gone, and the return simply runs it 1 -> 0. `uBloom`
 *    is the cloud, and always runs 0 -> 1. A puff bursts outward whichever way
 *    the figure is going -- running the whole shader backwards made the return
 *    a cloud that sucked inward, which reads as a video played in reverse
 *    rather than as someone arriving. `uTime` runs forward too, so the smoke
 *    keeps churning either way. The two clocks are also offset on a return;
 *    see `returnDelay`.
 *  - __The portrait is a texture.__ The art is already on screen, so the puff
 *    dissolves the real face rather than a stand-in. A cross-origin asset host
 *    that does not allow CORS cannot be sampled; that falls back to a tinted
 *    silhouette, which still reads as a figure going up in smoke.
 */

export type Rgb = [number, number, number]

/** Viewport rectangle, CSS pixels, as getBoundingClientRect reports one. */
export interface PuffRect {
  left: number
  top: number
  width: number
  height: number
}

export interface SmokePuffOptions {
  /** Smoke colour as [r, g, b] in the 0-1 range. */
  color?: Rgb
  /** Seconds the puff takes end to end. */
  duration?: number
  /**
   * How far the smoke reaches past the portrait, as a multiple of its larger
   * side. This is the knob for how big the puff reads: the quad is sized from
   * it and the cloud fills the quad, so it scales the whole effect (and the
   * fill rate with it). Kept tight on purpose -- a cloud several portraits
   * wide stops reading as one investigator going and starts covering the
   * neighbours.
   */
  reach?: number
  /** Thickness of the cloud (0 to 2). */
  density?: number
  /** Turns of swirl as the portrait is sucked in. */
  swirl?: number
  /**
   * On a return only: how far into the portrait's expansion the cloud holds
   * off before it bursts, as a fraction of `duration`. The figure starts to
   * condense and the puff goes off around it, which is the order a bamf-in
   * reads in -- a cloud already hanging there before anything has arrived
   * does not. The puff still gets its full `duration`, so a return runs for
   * `duration * (1 + returnDelay)`. Ignored on the way out, where the two
   * clocks are the same thing.
   */
  returnDelay?: number
  /** Corner radius of the portrait in CSS pixels. Match the element's. */
  radius?: number
}

export interface PuffRequest extends SmokePuffOptions {
  /** Where the portrait is (or was) on screen. */
  rect: PuffRect
  /** 'out' implodes the portrait into smoke; 'in' condenses it back. */
  direction: 'out' | 'in'
  /** URL of the art to dissolve. Omitted or unreadable gives a silhouette. */
  image?: string | null
  /**
   * The live element the art is already displayed in, when there is one. Tried
   * before `image` because an element the browser has decoded needs no second
   * request -- and because a same-origin one is readable where a re-fetch of
   * the same URL might not be.
   */
  imageElement?: HTMLImageElement | null
}

export interface SmokePuffLayer {
  /**
   * Play one puff. Resolves when it has finished, or at once if the effect
   * cannot run -- so a caller that hides the real portrait for the duration
   * always gets it back.
   */
  play: (request: PuffRequest) => Promise<void>
  /** Stop everything and release the GPU resources. */
  destroy: () => void
}

export const SMOKE_PUFF_DEFAULTS: Required<SmokePuffOptions> = {
  color: [0.24, 0.85, 0.42],
  duration: 0.85,
  reach: 0.45,
  density: 1,
  swirl: 2.4,
  returnDelay: 0.5,
  radius: 3,
}

const VERT = `#version 300 es
precision highp float;
layout(location = 0) in vec2 aPos;
uniform vec4 uQuad;
uniform vec2 uViewport;
out vec2 vUv;
void main () {
  vUv = aPos * 0.5 + 0.5;
  vec2 px = uQuad.xy + vec2(vUv.x, 1.0 - vUv.y) * uQuad.zw;
  gl_Position = vec4(px.x / uViewport.x * 2.0 - 1.0, 1.0 - px.y / uViewport.y * 2.0, 0.0, 1.0);
}`

const FRAG = `#version 300 es
precision highp float;
in vec2 vUv;
out vec4 outColor;

uniform sampler2D uPortrait;
uniform float uHasPortrait;
uniform vec2 uQuadSize;
uniform vec2 uCenter;
uniform vec2 uHalf;
uniform float uCorner;
uniform float uProgress;
uniform float uBloom;
uniform float uTime;
uniform vec3 uColor;
uniform float uDensity;
uniform float uSwirl;
uniform float uSeed;

vec3 permute (vec3 x) {
  return mod(((x * 34.0) + 1.0) * x, 289.0);
}

float snoise (vec2 v) {
  const vec4 C = vec4(
    0.211324865405187, 0.366025403784439,
    -0.577350269189626, 0.024390243902439
  );
  vec2 i = floor(v + dot(v, C.yy));
  vec2 x0 = v - i + dot(i, C.xx);
  vec2 i1 = (x0.x > x0.y) ? vec2(1.0, 0.0) : vec2(0.0, 1.0);
  vec4 x12 = x0.xyxy + C.xxzz;
  x12.xy -= i1;
  i = mod(i, 289.0);
  vec3 p = permute(
    permute(i.y + vec3(0.0, i1.y, 1.0)) + i.x + vec3(0.0, i1.x, 1.0)
  );
  vec3 m = max(
    0.5 - vec3(dot(x0, x0), dot(x12.xy, x12.xy), dot(x12.zw, x12.zw)),
    0.0
  );
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
  mat2 m = mat2(0.8, -0.6, 0.6, 0.8);
  float v = 0.5 * snoise(p);
  p = m * p * 2.03 + vec2(11.3, 7.1);
  v += 0.27 * snoise(p);
  p = m * p * 1.97 + vec2(3.7, 19.1);
  v += 0.15 * snoise(p);
  return v * 0.53 + 0.5;
}

mat2 rot (float a) {
  float c = cos(a);
  float s = sin(a);
  return mat2(c, -s, s, c);
}

/* Signed distance to the portrait's rounded rectangle, so the silhouette the
   smoke eats is the shape the player sees rather than a circle. */
float roundedBox (vec2 p, vec2 b, float r) {
  vec2 d = abs(p) - b + r;
  return length(max(d, 0.0)) + min(max(d.x, d.y), 0.0) - r;
}

void main () {
  vec2 p = (vUv - 0.5) * uQuadSize - uCenter;
  vec2 q = p / uHalf;
  float t = clamp(uProgress, 0.0, 1.0);
  float b = clamp(uBloom, 0.0, 1.0);

  /* The portrait being pulled in. It holds its size while it starts to fray
     and only then snaps inward, because a shrink that begins on frame one
     reads as the card being scaled rather than as something taking it.
     Clamped well above zero: dividing by the scale is what magnifies the art,
     and at zero the whole quad becomes one texel. */
  float pull = pow(t, 1.25);
  float scale = max(1.0 - 0.92 * pull, 0.05);
  vec2 sp = rot(uSwirl * pull * (1.2 - clamp(length(q), 0.0, 1.6) * 0.5)) * (q / scale);

  vec2 uv = sp * 0.5 + 0.5;
  vec4 art = texture(uPortrait, vec2(uv.x, 1.0 - uv.y));
  vec3 artRgb = mix(uColor * 0.3, art.rgb, uHasPortrait);
  float shape = 1.0 - smoothstep(-1.5, 1.5, roundedBox(sp * uHalf, uHalf, uCorner));

  /* Edges go first. The noise alone dissolves in blotches all over the face;
     biasing it by how far out the texel sits makes the silhouette eat inward,
     which is what sells the figure being drawn into a point. Coarse noise on
     purpose -- fine noise reads as television static, not as coming apart. */
  float grain = fbm(sp * 1.5 + uSeed);
  float inward = smoothstep(1.25, 0.0, length(sp));
  /* Linear, not eased. An eased front spends the first third of the puff
     doing nothing visible and then takes the whole face in two frames, which
     is what the first pass looked like. */
  float front = t * 1.5;
  float keep = smoothstep(front - 0.45, front, inward * 0.9 + grain * 0.45);
  float artA = shape * keep;

  /* The cloud, in a space normalised to the whole quad rather than to the
     portrait -- the quad is sized for the smoke to have somewhere to go, and
     scaling the billows to the face instead was what kept the first version's
     puff trapped inside the card. Uniform scale, so the cloud is round on a
     portrait that is not. */
  float unit = max(uQuadSize.x, uQuadSize.y) * 0.5;
  float grow = mix(0.25, 0.92, pow(b, 0.5));
  vec2 cp = (p / unit) / grow;
  cp.y -= b * 0.42;
  vec2 warp = vec2(
    fbm(cp * 1.5 + vec2(uTime * 0.22, -uTime * 0.15) + uSeed),
    fbm(cp * 1.5 + vec2(4.7, 1.9) - vec2(uTime * 0.18, uTime * 0.24) + uSeed)
  );
  float cloud = fbm(cp * 1.9 + (warp - 0.5) * 1.9 - vec2(0.0, uTime * 0.28) + uSeed * 0.5);
  float falloff = 1.0 - smoothstep(0.1, 1.0, length(cp));
  float body = smoothstep(0.4, 0.72, cloud) * falloff;
  /* Opens faster than it clears, and is gone by the end: anything still on
     screen at t = 1 would be a smudge left over the empty board. */
  float env = smoothstep(0.0, 0.16, b) * (1.0 - smoothstep(0.66, 1.0, b));
  float smokeA = clamp(body * env * 1.35 * uDensity, 0.0, 1.0);
  vec3 smokeCol = mix(uColor * 0.16, uColor * 1.35 + 0.06, smoothstep(0.45, 0.95, cloud));

  /* The snap, lit from inside: the silhouette giving on the way out, and the
     arrival on the way back. On the cloud's clock, so it lands with the puff
     rather than at opposite ends of the two directions. */
  float flash = exp(-pow((b - 0.5) / 0.14, 2.0));
  float core = 1.0 - smoothstep(0.0, 1.1, length(q));
  float glowA = flash * core * 0.55;

  /* The dissolving fringe catches that light, which is what keeps the edge of
     the silhouette reading as burning away rather than as a soft mask. */
  float fringe = shape * clamp(keep * (1.0 - keep) * 3.0, 0.0, 1.0);

  float alpha = artA + smokeA * (1.0 - artA);
  vec3 pm = artRgb * artA + smokeCol * smokeA * (1.0 - artA);
  pm += uColor * fringe * 0.9;
  alpha = min(alpha + fringe * 0.3, 1.0);
  pm += uColor * flash * core * 1.1;
  alpha = min(alpha + glowA, 1.0);

  outColor = vec4(pm, alpha);
}`

interface ActivePuff {
  quad: { x: number; y: number; w: number; h: number }
  center: [number, number]
  half: [number, number]
  corner: number
  color: Rgb
  density: number
  swirl: number
  seed: number
  direction: 'out' | 'in'
  /** The portrait's timeline. The cloud's is the same length, offset by `delay`. */
  duration: number
  /** Fraction of `duration` the cloud waits before it starts. */
  delay: number
  /** When the whole puff is over: `duration * (1 + delay)`. */
  life: number
  elapsed: number
  texture: WebGLTexture | null
  done: () => void
}

/* Textures are keyed by URL and kept for the life of the layer: an
 * investigator who bamfs out and back has the same portrait both times, and a
 * scenario holds at most a handful of them. `null` is cached too, so an asset
 * host that refuses CORS is asked once rather than on every puff. */
type TextureCache = Map<string, Promise<WebGLTexture | null>>

export function createSmokePuffLayer(
  canvas: HTMLCanvasElement,
  options: SmokePuffOptions = {},
): SmokePuffLayer | null {
  const defaults = { ...SMOKE_PUFF_DEFAULTS, ...options }

  const gl = canvas.getContext('webgl2', {
    alpha: true,
    depth: false,
    stencil: false,
    antialias: false,
    premultipliedAlpha: true,
  })
  if (!gl || gl.isContextLost()) return null

  function compile(type: number, text: string): WebGLShader {
    const shader = gl!.createShader(type)!
    gl!.shaderSource(shader, text)
    gl!.compileShader(shader)
    if (!gl!.getShaderParameter(shader, gl!.COMPILE_STATUS)) {
      console.error('SmokePuff shader error:', gl!.getShaderInfoLog(shader))
    }
    return shader
  }

  const vertexShader = compile(gl.VERTEX_SHADER, VERT)
  const fragmentShader = compile(gl.FRAGMENT_SHADER, FRAG)
  const program = gl.createProgram()!
  gl.attachShader(program, vertexShader)
  gl.attachShader(program, fragmentShader)
  gl.linkProgram(program)

  const uniforms: Record<string, WebGLUniformLocation> = {}
  const uniformCount = gl.getProgramParameter(program, gl.ACTIVE_UNIFORMS)
  for (let i = 0; i < uniformCount; i++) {
    const info = gl.getActiveUniform(program, i)!
    uniforms[info.name] = gl.getUniformLocation(program, info.name)!
  }

  const quadBuffer = gl.createBuffer()
  gl.bindBuffer(gl.ARRAY_BUFFER, quadBuffer)
  gl.bufferData(gl.ARRAY_BUFFER, new Float32Array([-1, -1, 1, -1, -1, 1, 1, 1]), gl.STATIC_DRAW)
  gl.enableVertexAttribArray(0)
  gl.vertexAttribPointer(0, 2, gl.FLOAT, false, 0, 0)

  // The sampler must be bound to something valid even for a silhouette puff.
  const blank = gl.createTexture()!
  gl.bindTexture(gl.TEXTURE_2D, blank)
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

  gl.enable(gl.BLEND)
  // Premultiplied, which the fragment shader emits: puffs may overlap, and a
  // straight-alpha blend darkens where they do.
  gl.blendFunc(gl.ONE, gl.ONE_MINUS_SRC_ALPHA)

  const textures: TextureCache = new Map()
  const owned: WebGLTexture[] = []
  let destroyed = false

  function upload(source: TexImageSource): WebGLTexture | null {
    const texture = gl!.createTexture()
    if (!texture) return null
    gl!.bindTexture(gl!.TEXTURE_2D, texture)
    gl!.texParameteri(gl!.TEXTURE_2D, gl!.TEXTURE_MIN_FILTER, gl!.LINEAR)
    gl!.texParameteri(gl!.TEXTURE_2D, gl!.TEXTURE_MAG_FILTER, gl!.LINEAR)
    gl!.texParameteri(gl!.TEXTURE_2D, gl!.TEXTURE_WRAP_S, gl!.CLAMP_TO_EDGE)
    gl!.texParameteri(gl!.TEXTURE_2D, gl!.TEXTURE_WRAP_T, gl!.CLAMP_TO_EDGE)
    try {
      gl!.texImage2D(gl!.TEXTURE_2D, 0, gl!.RGBA, gl!.RGBA, gl!.UNSIGNED_BYTE, source)
    } catch {
      // A cross-origin image the asset host served without CORS headers taints
      // the upload. Silhouette instead; see the module header.
      gl!.deleteTexture(texture)
      return null
    }
    owned.push(texture)
    return texture
  }

  function loadTexture(request: PuffRequest): Promise<WebGLTexture | null> {
    const live = request.imageElement
    const src = request.image ?? live?.currentSrc ?? live?.src ?? null
    if (!src) return Promise.resolve(null)
    const cached = textures.get(src)
    if (cached) return cached

    const pending = (async () => {
      // The element already on screen first: it is decoded, so this costs no
      // request at all, and the common case resolves within the frame.
      if (live?.complete && live.naturalWidth > 0) {
        const texture = upload(live)
        if (texture) return texture
      }
      const image = new Image()
      image.crossOrigin = 'anonymous'
      image.src = src
      try {
        await image.decode()
      } catch {
        return null
      }
      return destroyed ? null : upload(image)
    })()

    textures.set(src, pending)
    return pending
  }

  const dpr = () => Math.min(window.devicePixelRatio || 1, 2)
  const puffs: ActivePuff[] = []
  let raf = 0
  let last = 0

  function syncCanvasSize() {
    const ratio = dpr()
    const width = Math.max(1, Math.round(window.innerWidth * ratio))
    const height = Math.max(1, Math.round(window.innerHeight * ratio))
    if (canvas.width !== width || canvas.height !== height) {
      canvas.width = width
      canvas.height = height
    }
  }

  function drawPuff(puff: ActivePuff) {
    // The cloud's clock always runs forward, and may start late; only the
    // portrait's reverses.
    const shape = Math.min(puff.elapsed / puff.duration, 1)
    const progress = puff.direction === 'out' ? shape : 1 - shape
    const bloom = Math.min(Math.max(puff.elapsed / puff.duration - puff.delay, 0), 1)
    gl!.activeTexture(gl!.TEXTURE0)
    gl!.bindTexture(gl!.TEXTURE_2D, puff.texture ?? blank)
    gl!.uniform1i(uniforms.uPortrait, 0)
    gl!.uniform1f(uniforms.uHasPortrait, puff.texture ? 1 : 0)
    gl!.uniform4f(uniforms.uQuad, puff.quad.x, puff.quad.y, puff.quad.w, puff.quad.h)
    gl!.uniform2f(uniforms.uViewport, canvas.width, canvas.height)
    gl!.uniform2f(uniforms.uQuadSize, puff.quad.w, puff.quad.h)
    gl!.uniform2f(uniforms.uCenter, puff.center[0], puff.center[1])
    gl!.uniform2f(uniforms.uHalf, puff.half[0], puff.half[1])
    gl!.uniform1f(uniforms.uCorner, puff.corner)
    gl!.uniform1f(uniforms.uProgress, progress)
    gl!.uniform1f(uniforms.uBloom, bloom)
    gl!.uniform1f(uniforms.uTime, puff.elapsed + puff.seed)
    gl!.uniform3f(uniforms.uColor, puff.color[0], puff.color[1], puff.color[2])
    gl!.uniform1f(uniforms.uDensity, puff.density)
    gl!.uniform1f(uniforms.uSwirl, puff.swirl)
    gl!.uniform1f(uniforms.uSeed, puff.seed)
    gl!.drawArrays(gl!.TRIANGLE_STRIP, 0, 4)
  }

  function frame(now: number) {
    if (destroyed) return
    const delta = Math.min((now - last) / 1000, 1 / 20)
    last = now
    syncCanvasSize()
    gl!.viewport(0, 0, canvas.width, canvas.height)
    gl!.clearColor(0, 0, 0, 0)
    gl!.clear(gl!.COLOR_BUFFER_BIT)
    gl!.useProgram(program)
    gl!.bindBuffer(gl!.ARRAY_BUFFER, quadBuffer)
    gl!.vertexAttribPointer(0, 2, gl!.FLOAT, false, 0, 0)

    for (let i = puffs.length - 1; i >= 0; i--) {
      const puff = puffs[i]!
      puff.elapsed += delta
      if (puff.elapsed >= puff.life) {
        puffs.splice(i, 1)
        puff.done()
        continue
      }
      drawPuff(puff)
    }

    if (puffs.length === 0) {
      // Nothing left to draw, and the last frame cleared the canvas, so stop.
      raf = 0
      return
    }
    raf = requestAnimationFrame(frame)
  }

  function start() {
    if (raf || destroyed) return
    last = performance.now()
    raf = requestAnimationFrame(frame)
  }

  const motionQuery = window.matchMedia('(prefers-reduced-motion: reduce)')

  return {
    async play(request) {
      if (destroyed) return
      // No puff at all under reduced motion: this is a half-second of swirling
      // particles, which is exactly what the setting is asking us not to do.
      if (motionQuery.matches) return
      const { rect } = request
      if (rect.width <= 0 || rect.height <= 0) return

      const texture = await loadTexture(request)
      if (destroyed) return

      const ratio = dpr()
      const color = request.color ?? defaults.color
      const reach = Math.max(request.reach ?? defaults.reach, 0.2)
      // The smoke reaches further up than out, the way smoke does.
      const margin = Math.max(rect.width, rect.height) * reach
      const top = margin * 1.3
      const quad = {
        x: (rect.left - margin) * ratio,
        y: (rect.top - top) * ratio,
        w: (rect.width + margin * 2) * ratio,
        h: (rect.height + top + margin) * ratio,
      }
      const duration = Math.max(request.duration ?? defaults.duration, 0.1)
      const delay =
        request.direction === 'in' ? Math.max(request.returnDelay ?? defaults.returnDelay, 0) : 0

      await new Promise<void>((resolve) => {
        puffs.push({
          quad,
          // The portrait sits lower than the quad's centre by exactly the extra
          // headroom given to the rising smoke. Positive y is up in the shader.
          center: [0, -((top - margin) / 2) * ratio],
          half: [(rect.width / 2) * ratio, (rect.height / 2) * ratio],
          corner: (request.radius ?? defaults.radius) * ratio,
          color,
          density: request.density ?? defaults.density,
          swirl: request.swirl ?? defaults.swirl,
          seed: Math.random() * 40,
          direction: request.direction,
          duration,
          delay,
          life: duration * (1 + delay),
          elapsed: 0,
          texture,
          done: resolve,
        })
        start()
      })
    },
    destroy() {
      destroyed = true
      cancelAnimationFrame(raf)
      raf = 0
      // Anything still in flight has a caller waiting to put a portrait back.
      for (const puff of puffs.splice(0)) puff.done()
      for (const texture of owned) gl!.deleteTexture(texture)
      owned.length = 0
      textures.clear()
      gl!.deleteTexture(blank)
      gl!.deleteProgram(program)
      gl!.deleteShader(vertexShader)
      gl!.deleteShader(fragmentShader)
      gl!.deleteBuffer(quadBuffer)
      // Deliberately not WEBGL_lose_context; see flameWrap.ts for why that
      // poisons the canvas element for good.
    },
  }
}

/** `#rrggbb` (or `#rgb`) to the 0-1 triple the shader wants. */
export function parseRgb(color: string, fallback: Rgb): Rgb {
  const hex = color.trim().replace(/^#/, '')
  const full =
    hex.length === 3
      ? hex
          .split('')
          .map((c) => c + c)
          .join('')
      : hex
  if (!/^[0-9a-fA-F]{6}$/.test(full)) return fallback
  return [
    parseInt(full.slice(0, 2), 16) / 255,
    parseInt(full.slice(2, 4), 16) / 255,
    parseInt(full.slice(4, 6), 16) / 255,
  ]
}
