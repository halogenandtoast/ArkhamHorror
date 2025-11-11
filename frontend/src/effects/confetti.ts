/* eslint-disable @typescript-eslint/no-explicit-any */

// confetti.ts
// TypeScript port of the provided IIFE module (browser-only).
// Works in Vue: import confetti, { create, shapeFromPath, shapeFromText } from './confetti'

type RGB = { r: number; g: number; b: number };

type Origin = { x?: number; y?: number };

type PathShape = {
  type: 'path';
  path: string;
  matrix: number[];
};

type BitmapShape = {
  type: 'bitmap';
  bitmap: ImageBitmap;
  matrix: number[];
};

type Shape = 'square' | 'circle' | 'star' | PathShape | BitmapShape;

export interface ConfettiOptions {
  particleCount?: number;
  angle?: number;
  spread?: number;
  startVelocity?: number;
  decay?: number;
  gravity?: number;
  drift?: number;
  ticks?: number;
  x?: number; // deprecated, use origin.x
  y?: number; // deprecated, use origin.y
  origin?: Origin;
  shapes?: Shape[];
  zIndex?: number;
  colors?: string[];
  disableForReducedMotion?: boolean;
  scalar?: number;
  flat?: boolean;
  resize?: boolean;
  useWorker?: boolean;
}

type Fire = (options?: ConfettiOptions) => Promise<void>;
interface FireWithReset extends Fire { reset(): void }

const globalObj: Window & typeof globalThis =
  (typeof window !== 'undefined'
    ? window
    : typeof self !== 'undefined'
    ? (self as any)
    : (globalThis as any)) as any;

const canUseWorker =
  !!globalObj.Worker &&
  !!globalObj.Blob &&
  !!globalObj.Promise &&
  !!globalObj.OffscreenCanvas &&
  !!globalObj.OffscreenCanvasRenderingContext2D &&
  !!globalObj.HTMLCanvasElement &&
  !!globalObj.HTMLCanvasElement.prototype.transferControlToOffscreen &&
  !!globalObj.URL &&
  !!globalObj.URL.createObjectURL;

const canUsePaths = typeof (globalObj as any).Path2D === 'function' && typeof (globalObj as any).DOMMatrix === 'function';

const canDrawBitmap = (() => {
  if (!globalObj.OffscreenCanvas) return false;
  try {
    const canvas = new OffscreenCanvas(1, 1);
    const ctx = canvas.getContext('2d')!;
    ctx.fillRect(0, 0, 1, 1);
    const bitmap = canvas.transferToImageBitmap();
    ctx.createPattern(bitmap, 'no-repeat');
  } catch {
    return false;
  }
  return true;
})();

const defaults: Required<Pick<
  ConfettiOptions,
  | 'particleCount'
  | 'angle'
  | 'spread'
  | 'startVelocity'
  | 'decay'
  | 'gravity'
  | 'drift'
  | 'ticks'
  | 'zIndex'
  | 'colors'
  | 'disableForReducedMotion'
  | 'scalar'
>> & { shapes: Shape[] } = {
  particleCount: 50,
  angle: 90,
  spread: 45,
  startVelocity: 45,
  decay: 0.9,
  gravity: 1,
  drift: 0,
  ticks: 200,
  zIndex: 100,
  colors: ['#26ccff', '#a25afd', '#ff5e7e', '#88ff5a', '#fcff42', '#ffa62d', '#ff36ff'],
  disableForReducedMotion: false,
  scalar: 1,
  shapes: ['square', 'circle'],
};

function toRgb(hex: string): RGB {
  let val = String(hex).replace(/[^0-9a-f]/gi, '');
  if (val.length < 6) val = val[0] + val[0] + val[1] + val[1] + val[2] + val[2];
  const toDec = (s: string) => parseInt(s, 16);
  return { r: toDec(val.slice(0, 2)), g: toDec(val.slice(2, 4)), b: toDec(val.slice(4, 6)) };
}
function colorsToRgb(colors: string[]) { return colors.map(toRgb); }
function onlyPositiveInt(n: number) { return n < 0 ? 0 : Math.floor(n); }
function randomInt(min: number, max: number) { return Math.floor(Math.random() * (max - min)) + min; }
function isOk<T>(v: T | null | undefined): v is T { return v !== null && v !== undefined; }
function prop<T, K extends keyof T, R = T[K]>(
  options: T | undefined,
  name: K,
  transform?: (v: T[K]) => R
): R | T[K] {
  const src = options && isOk(options[name]) ? options[name] : (defaults as any)[name];
  return transform ? transform(src as any) : (src as any);
}
function getOrigin(options?: ConfettiOptions): Required<Origin> {
  const origin = (prop(options, 'origin', Object) as Origin) || {};
  origin.x = prop(origin, 'x', Number) as number ?? (isOk(options?.x) ? Number(options!.x) : 0.5);
  origin.y = prop(origin, 'y', Number) as number ?? (isOk(options?.y) ? Number(options!.y) : 0.5);
  return { x: origin.x!, y: origin.y! };
}

function setCanvasWindowSize(canvas: HTMLCanvasElement) {
  canvas.width = document.documentElement.clientWidth;
  canvas.height = document.documentElement.clientHeight;
}
function setCanvasRectSize(canvas: HTMLCanvasElement) {
  const rect = canvas.getBoundingClientRect();
  canvas.width = rect.width;
  canvas.height = rect.height;
}
function getCanvas(zIndex: number) {
  const canvas = document.createElement('canvas');
  canvas.style.position = 'fixed';
  canvas.style.top = '0px';
  canvas.style.left = '0px';
  canvas.style.pointerEvents = 'none';
  canvas.style.zIndex = String(zIndex);
  return canvas;
}

const raf = (() => {
  const TIME = Math.floor(1000 / 60);
  let lastFrameTime = 0;
  const frames: Record<string, number> = {};
  let frame: (cb: () => void) => any;
  let cancel: (id: any) => void;

  if (typeof globalObj.requestAnimationFrame === 'function' && typeof globalObj.cancelAnimationFrame === 'function') {
    frame = (cb) => {
      const id = Math.random().toString(36);
      frames[id] = globalObj.requestAnimationFrame(function onFrame(time: number) {
        if (lastFrameTime === time || lastFrameTime + TIME - 1 < time) {
          lastFrameTime = time;
          delete frames[id];
          cb();
        } else {
          frames[id] = globalObj.requestAnimationFrame(onFrame);
        }
      });
      return id;
    };
    cancel = (id) => {
      if (frames[id]) globalObj.cancelAnimationFrame(frames[id]);
    };
  } else {
    frame = (cb) => setTimeout(cb, TIME);
    cancel = (timer) => clearTimeout(timer);
  }
  return { frame, cancel };
})();

// Map ImageBitmap -> OffscreenCanvas for pattern transforms (perf + avoid leaks)
const bitmapMapper = (() => {
  const skipTransform = !canDrawBitmap;
  const map = new Map<ImageBitmap, OffscreenCanvas>();
  return {
    transform(bitmap: ImageBitmap): ImageBitmap | OffscreenCanvas {
      if (skipTransform) return bitmap;
      if (map.has(bitmap)) return map.get(bitmap)!;
      const canvas = new OffscreenCanvas(bitmap.width, bitmap.height);
      const ctx = canvas.getContext('2d')!;
      ctx.drawImage(bitmap, 0, 0);
      map.set(bitmap, canvas);
      return canvas;
    },
    clear() { map.clear(); },
  };
})();

// --- physics & drawing ---

type Fetti = {
  x: number; y: number; wobble: number; wobbleSpeed: number; velocity: number;
  angle2D: number; tiltAngle: number; color: RGB; shape: Shape | 'circle' | 'square' | 'star';
  tick: number; totalTicks: number; decay: number; drift: number; random: number;
  tiltSin: number; tiltCos: number; wobbleX: number; wobbleY: number; gravity: number;
  ovalScalar: number; scalar: number; flat?: boolean;
};

function randomPhysics(opts: {
  x: number; y: number; angle: number; spread: number; startVelocity: number;
  color: RGB; shape: Shape | 'circle' | 'square' | 'star'; ticks: number;
  decay: number; gravity: number; drift: number; scalar: number; flat?: boolean;
}): Fetti {
  const radAngle = opts.angle * (Math.PI / 180);
  const radSpread = opts.spread * (Math.PI / 180);
  return {
    x: opts.x, y: opts.y,
    wobble: Math.random() * 10,
    wobbleSpeed: Math.min(0.11, Math.random() * 0.1 + 0.05),
    velocity: (opts.startVelocity * 0.5) + (Math.random() * opts.startVelocity),
    angle2D: -radAngle + ((0.5 * radSpread) - (Math.random() * radSpread)),
    tiltAngle: (Math.random() * (0.75 - 0.25) + 0.25) * Math.PI,
    color: opts.color, shape: opts.shape, tick: 0, totalTicks: opts.ticks,
    decay: opts.decay, drift: opts.drift, random: Math.random() + 2,
    tiltSin: 0, tiltCos: 0, wobbleX: 0, wobbleY: 0,
    gravity: opts.gravity * 3, ovalScalar: 0.6, scalar: opts.scalar, flat: opts.flat,
  };
}

function ellipse(context: CanvasRenderingContext2D, x: number, y: number, radiusX: number, radiusY: number, rotation: number, startAngle: number, endAngle: number, antiClockwise?: boolean) {
  context.save();
  context.translate(x, y);
  context.rotate(rotation);
  context.scale(radiusX, radiusY);
  context.arc(0, 0, 1, startAngle, endAngle, antiClockwise ?? false);
  context.restore();
}

function transformPath2D(
  pathString: string,
  pathMatrix: number[],
  x: number,
  y: number,
  scaleX: number,
  scaleY: number,
  rotation: number
): Path2D {
  // @ts-ignore - TS lib may not have Path2D DOMMatrix in all targets
  const path2d = new Path2D(pathString);
  // @ts-ignore
  const t1 = new Path2D();
  // @ts-ignore
  t1.addPath(path2d, new DOMMatrix(pathMatrix));
  // @ts-ignore
  const t2 = new Path2D();
  // @ts-ignore
  t2.addPath(
    t1,
    // @ts-ignore
    new DOMMatrix([
      Math.cos(rotation) * scaleX,
      Math.sin(rotation) * scaleX,
      -Math.sin(rotation) * scaleY,
      Math.cos(rotation) * scaleY,
      x,
      y,
    ])
  );
  return t2;
}

function updateFetti(ctx: CanvasRenderingContext2D, f: Fetti): boolean {
  f.x += Math.cos(f.angle2D) * f.velocity + f.drift;
  f.y += Math.sin(f.angle2D) * f.velocity + f.gravity;
  f.velocity *= f.decay;

  if (f.flat) {
    f.wobble = 0;
    f.wobbleX = f.x + (10 * f.scalar);
    f.wobbleY = f.y + (10 * f.scalar);
    f.tiltSin = 0; f.tiltCos = 0; f.random = 1;
  } else {
    f.wobble += f.wobbleSpeed;
    f.wobbleX = f.x + ((10 * f.scalar) * Math.cos(f.wobble));
    f.wobbleY = f.y + ((10 * f.scalar) * Math.sin(f.wobble));
    f.tiltAngle += 0.1;
    f.tiltSin = Math.sin(f.tiltAngle);
    f.tiltCos = Math.cos(f.tiltAngle);
    f.random = Math.random() + 2;
  }

  const progress = (f.tick++) / f.totalTicks;
  const x1 = f.x + (f.random * f.tiltCos);
  const y1 = f.y + (f.random * f.tiltSin);
  const x2 = f.wobbleX + (f.random * f.tiltCos);
  const y2 = f.wobbleY + (f.random * f.tiltSin);

  ctx.fillStyle = `rgba(${f.color.r}, ${f.color.g}, ${f.color.b}, ${1 - progress})`;
  ctx.beginPath();

  if (canUsePaths && (f as any).shape?.['type'] === 'path') {
    const shp = f.shape as PathShape;
    // @ts-ignore
    ctx.fill(transformPath2D(
      shp.path,
      shp.matrix,
      f.x, f.y,
      Math.abs(x2 - x1) * 0.1,
      Math.abs(y2 - y1) * 0.1,
      Math.PI / 10 * f.wobble
    ));
  } else if ((f as any).shape?.['type'] === 'bitmap') {
    // @ts-ignore
    const rotation = Math.PI / 10 * f.wobble;
    const scaleX = Math.abs(x2 - x1) * 0.1;
    const scaleY = Math.abs(y2 - y1) * 0.1;
    const bmp = (f.shape as BitmapShape).bitmap;
    const width = bmp.width * f.scalar;
    const height = bmp.height * f.scalar;

    // @ts-ignore
    const matrix = new DOMMatrix([
      Math.cos(rotation) * scaleX,
      Math.sin(rotation) * scaleX,
      -Math.sin(rotation) * scaleY,
      Math.cos(rotation) * scaleY,
      f.x,
      f.y
    ]);
    // @ts-ignore
    matrix.multiplySelf(new DOMMatrix((f.shape as BitmapShape).matrix));
    const pattern = ctx.createPattern(bitmapMapper.transform(bmp) as any, 'no-repeat')!;
    // @ts-ignore
    pattern.setTransform(matrix);
    (ctx as any).globalAlpha = (1 - progress);
    ctx.fillStyle = pattern as any;
    ctx.fillRect(f.x - (width / 2), f.y - (height / 2), width, height);
    (ctx as any).globalAlpha = 1;
  } else if (f.shape === 'circle') {
    if (typeof (ctx as any).ellipse === 'function') {
      // @ts-ignore
      ctx.ellipse(f.x, f.y, Math.abs(x2 - x1) * f.ovalScalar, Math.abs(y2 - y1) * f.ovalScalar, Math.PI / 10 * f.wobble, 0, 2 * Math.PI);
    } else {
      ellipse(ctx, f.x, f.y, Math.abs(x2 - x1) * f.ovalScalar, Math.abs(y2 - y1) * f.ovalScalar, Math.PI / 10 * f.wobble, 0, 2 * Math.PI);
    }
  } else if (f.shape === 'star') {
    let rot = Math.PI / 2 * 3;
    const innerRadius = 4 * f.scalar;
    const outerRadius = 8 * f.scalar;
    let x = f.x, y = f.y;
    let spikes = 5;
    const step = Math.PI / spikes;
    while (spikes--) {
      x = f.x + Math.cos(rot) * outerRadius; y = f.y + Math.sin(rot) * outerRadius; ctx.lineTo(x, y); rot += step;
      x = f.x + Math.cos(rot) * innerRadius; y = f.y + Math.sin(rot) * innerRadius; ctx.lineTo(x, y); rot += step;
    }
  } else { // 'square' fallback as skewed quad
    ctx.moveTo(Math.floor(f.x), Math.floor(f.y));
    ctx.lineTo(Math.floor(f.wobbleX), Math.floor(y1));
    ctx.lineTo(Math.floor(x2), Math.floor(y2));
    ctx.lineTo(Math.floor(x1), Math.floor(f.wobbleY));
  }

  ctx.closePath();
  ctx.fill();

  return f.tick < f.totalTicks;
}

function animate(
  canvas: HTMLCanvasElement,
  fettis: Fetti[],
  resizer: (c: HTMLCanvasElement) => void,
  size: { width: number | null; height: number | null },
  done: () => void
) {
  let animating = fettis.slice();
  const ctx = canvas.getContext('2d')!;
  let animationFrame: any;
  let destroy: (() => void) | null;

  const prom = new Promise<void>((resolve) => {
    const onDone = () => {
      animationFrame = destroy = null;
      ctx.clearRect(0, 0, size.width || 0, size.height || 0);
      bitmapMapper.clear();
      done();
      resolve();
    };

    const update = () => {
      if (!size.width && !size.height) {
        resizer(canvas);
        size.width = canvas.width;
        size.height = canvas.height;
      }
      ctx.clearRect(0, 0, size.width || 0, size.height || 0);
      animating = animating.filter((f) => updateFetti(ctx, f));
      if (animating.length) {
        animationFrame = raf.frame(update);
      } else {
        onDone();
      }
    };

    animationFrame = raf.frame(update);
    destroy = onDone;
  });

  return {
    addFettis: (more: Fetti[]) => {
      animating = animating.concat(more);
      return prom;
    },
    canvas,
    promise: prom,
    reset: () => {
      if (animationFrame) raf.cancel(animationFrame);
      if (destroy) destroy();
    },
  };
}

// --- worker management ---

function getWorkerFactory() {
  let worker: Worker | null = null;
  let prom: Promise<void> | null = null;
  const resolves: Record<string, () => void> = {};

  function decorate(w: Worker) {
    (w as any).init = (canvas: HTMLCanvasElement) => {
      const offscreen = (canvas as any).transferControlToOffscreen();
      w.postMessage({ canvas: offscreen }, [offscreen]);
    };
    (w as any).fire = (options: ConfettiOptions, size: { width: number | null; height: number | null }, done: () => void) => {
      if (prom) {
        w.postMessage({ options: options || {}, callback: null });
        return prom;
      }
      const id = Math.random().toString(36).slice(2);
      prom = new Promise<void>((resolve) => {
        const workerDone = (msg: MessageEvent) => {
          if ((msg.data as any).callback !== id) return;
          delete resolves[id];
          w.removeEventListener('message', workerDone);
          prom = null;
          bitmapMapper.clear();
          done();
          resolve();
        };
        w.addEventListener('message', workerDone);
        w.postMessage({ options: options || {}, callback: id });
        resolves[id] = () => workerDone({ data: { callback: id } } as any);
      });
      return prom;
    };
    (w as any).reset = () => {
      w.postMessage({ reset: true });
      for (const id in resolves) {
        resolves[id]();
        delete resolves[id];
      }
    };
  }

  return function getWorker(): (Worker & {
    init(canvas: HTMLCanvasElement): void;
    fire(options: ConfettiOptions, size: { width: number | null; height: number | null }, done: () => void): Promise<void>;
    reset(): void;
  }) | null {
    if (worker) return worker as any;
    if (!canUseWorker) return null;

    const code = [
      'var CONFETTI, SIZE = {}, module = {};',
      '(' + workerMain.toString() + ')(this, module, true, SIZE);',
      'onmessage = function(msg) {',
      '  if (msg.data.options) {',
      '    CONFETTI(msg.data.options).then(function () {',
      '      if (msg.data.callback) postMessage({ callback: msg.data.callback });',
      '    });',
      '  } else if (msg.data.reset) {',
      '    CONFETTI && CONFETTI.reset();',
      '  } else if (msg.data.resize) {',
      '    SIZE.width = msg.data.resize.width;',
      '    SIZE.height = msg.data.resize.height;',
      '  } else if (msg.data.canvas) {',
      '    SIZE.width = msg.data.canvas.width;',
      '    SIZE.height = msg.data.canvas.height;',
      '    CONFETTI = module.exports.create(msg.data.canvas);',
      '  }',
      '}',
    ].join('\n');

    try {
      worker = new Worker(URL.createObjectURL(new Blob([code])));
    } catch (e) {
      if (typeof console !== 'undefined' && typeof console.warn === 'function') {
        console.warn('🎊 Could not load worker', e);
      }
      return null;
    }
    decorate(worker);
    return worker as any;
  };
}

// the worker runs a reduced version of this module; we pass it in as stringified function
function workerMain(global: any, module: any, isWorker: boolean, workerSize: { width?: number; height?: number }) {
  // Reuse the same codebody but mark isWorker=true. We inline a tiny shim here:
  (function body() {
    // Minimal pieces needed inside worker:
    // We'll re-execute the same TS-ported logic but with different globals.
    // For brevity and size, we embed a small subset:
    module.exports = {} as any;
    // We will build a tiny facade; the worker uses module.exports.create(canvas)
    // to return a fire function that animates on OffscreenCanvas.
    // Instead of duplicating everything, we forward to a simplified cannon.
    // NOTE: Worker relies on OffscreenCanvas APIs which exist in worker.
    const defaultsW = {
      particleCount: 50, angle: 90, spread: 45, startVelocity: 45, decay: 0.9,
      gravity: 1, drift: 0, ticks: 200, scalar: 1, shapes: ['square', 'circle'],
      colors: ['#26ccff', '#a25afd', '#ff5e7e', '#88ff5a', '#fcff42', '#ffa62d', '#ff36ff'],
      disableForReducedMotion: false,
    };
    function toRgb(hex: string) {
      let v = String(hex).replace(/[^0-9a-f]/gi, '');
      if (v.length < 6) v = v[0]+v[0]+v[1]+v[1]+v[2]+v[2];
      const d = (s: string) => parseInt(s, 16);
      return { r: d(v.slice(0,2)), g: d(v.slice(2,4)), b: d(v.slice(4,6)) };
    }
    function colorsToRgbW(colors: string[]) { return colors.map(toRgb); }
    function onlyPositiveIntW(n: number) { return n < 0 ? 0 : Math.floor(n); }
    function randomIntW(min: number, max: number) { return Math.floor(Math.random() * (max - min)) + min; }

    const rafW = (() => {
      const TIME = Math.floor(1000 / 60);
      let lastFrameTime = 0;
      let id: any;
      return {
        frame(cb: () => void) {
          id = setTimeout(() => {
            const now = Date.now();
            if (lastFrameTime === now || lastFrameTime + TIME - 1 < now) {
              lastFrameTime = now;
              cb();
            }
          }, TIME);
          return id;
        },
        cancel(t: any) { clearTimeout(t); }
      };
    })();

    function getOriginW(options: any) {
      const o = options?.origin || {};
      const x = 'x' in o ? Number(o.x) : ('x' in options ? Number(options.x) : 0.5);
      const y = 'y' in o ? Number(o.y) : ('y' in options ? Number(options.y) : 0.5);
      return { x, y };
    }

    function randomPhysicsW(opts: any) {
      const radAngle = opts.angle * (Math.PI / 180);
      const radSpread = opts.spread * (Math.PI / 180);
      return {
        x: opts.x, y: opts.y, wobble: Math.random() * 10,
        wobbleSpeed: Math.min(0.11, Math.random() * 0.1 + 0.05),
        velocity: (opts.startVelocity * 0.5) + (Math.random() * opts.startVelocity),
        angle2D: -radAngle + ((0.5 * radSpread) - (Math.random() * radSpread)),
        tiltAngle: (Math.random() * (0.75 - 0.25) + 0.25) * Math.PI,
        color: opts.color, shape: opts.shape, tick: 0, totalTicks: opts.ticks,
        decay: opts.decay, drift: opts.drift, random: Math.random() + 2,
        tiltSin: 0, tiltCos: 0, wobbleX: 0, wobbleY: 0,
        gravity: opts.gravity * 3, ovalScalar: 0.6, scalar: opts.scalar, flat: opts.flat,
      };
    }

    function updateFettiW(ctx: OffscreenCanvasRenderingContext2D, f: any) {
      f.x += Math.cos(f.angle2D) * f.velocity + f.drift;
      f.y += Math.sin(f.angle2D) * f.velocity + f.gravity;
      f.velocity *= f.decay;
      if (f.flat) {
        f.wobble = 0; f.wobbleX = f.x + (10 * f.scalar); f.wobbleY = f.y + (10 * f.scalar);
        f.tiltSin = 0; f.tiltCos = 0; f.random = 1;
      } else {
        f.wobble += f.wobbleSpeed;
        f.wobbleX = f.x + ((10 * f.scalar) * Math.cos(f.wobble));
        f.wobbleY = f.y + ((10 * f.scalar) * Math.sin(f.wobble));
        f.tiltAngle += 0.1; f.tiltSin = Math.sin(f.tiltAngle); f.tiltCos = Math.cos(f.tiltAngle);
        f.random = Math.random() + 2;
      }
      const progress = (f.tick++) / f.totalTicks;
      const x1 = f.x + (f.random * f.tiltCos);
      const y1 = f.y + (f.random * f.tiltSin);
      const x2 = f.wobbleX + (f.random * f.tiltCos);
      const y2 = f.wobbleY + (f.random * f.tiltSin);
      (ctx as any).fillStyle = `rgba(${f.color.r}, ${f.color.g}, ${f.color.b}, ${1 - progress})`;
      ctx.beginPath();
      if (f.shape === 'circle') {
        // @ts-ignore
        if (ctx.ellipse) (ctx as any).ellipse(f.x, f.y, Math.abs(x2 - x1) * f.ovalScalar, Math.abs(y2 - y1) * f.ovalScalar, Math.PI / 10 * f.wobble, 0, 2 * Math.PI);
      } else if (f.shape === 'star') {
        let rot = Math.PI / 2 * 3;
        const ir = 4 * f.scalar, or = 8 * f.scalar;
        let x = f.x, y = f.y; let spikes = 5; const step = Math.PI / spikes;
        while (spikes--) {
          x = f.x + Math.cos(rot) * or; y = f.y + Math.sin(rot) * or; ctx.lineTo(x, y); rot += step;
          x = f.x + Math.cos(rot) * ir; y = f.y + Math.sin(rot) * ir; ctx.lineTo(x, y); rot += step;
        }
      } else {
        ctx.moveTo(Math.floor(f.x), Math.floor(f.y));
        ctx.lineTo(Math.floor(f.wobbleX), Math.floor(y1));
        ctx.lineTo(Math.floor(x2), Math.floor(y2));
        ctx.lineTo(Math.floor(x1), Math.floor(f.wobbleY));
      }
      ctx.closePath();
      ctx.fill();
      return f.tick < f.totalTicks;
    }

    function animateW(canvas: OffscreenCanvas, fettis: any[], size: { width: number; height: number }, done: () => void) {
      let animating = fettis.slice();
      const ctx = canvas.getContext('2d')!;
      let frame: any;
      return new Promise<void>((resolve) => {
        const onDone = () => { frame = null; ctx.clearRect(0,0,size.width,size.height); done(); resolve(); };
        const update = () => {
          if (canvas.width !== SIZE.width || canvas.height !== SIZE.height) {
            canvas.width = SIZE.width || canvas.width;
            canvas.height = SIZE.height || canvas.height;
            size.width = canvas.width; size.height = canvas.height;
          }
          ctx.clearRect(0,0,size.width,size.height);
          animating = animating.filter((f) => updateFettiW(ctx as any, f));
          if (animating.length) frame = rafW.frame(update); else onDone();
        };
        frame = rafW.frame(update);
      });
    }

    function createW(canvas: OffscreenCanvas): (opts?: any) => Promise<void> & { reset(): void } {
      let animation: { promise: Promise<void>; reset(): void } | null = null;
      const fire = (options?: any) => {
        const particleCount = onlyPositiveIntW(options?.particleCount ?? defaultsW.particleCount);
        const angle = Number(options?.angle ?? defaultsW.angle);
        const spread = Number(options?.spread ?? defaultsW.spread);
        const startVelocity = Number(options?.startVelocity ?? defaultsW.startVelocity);
        const decay = Number(options?.decay ?? defaultsW.decay);
        const gravity = Number(options?.gravity ?? defaultsW.gravity);
        const drift = Number(options?.drift ?? defaultsW.drift);
        const colors = colorsToRgbW(options?.colors ?? defaultsW.colors);
        const ticks = Number(options?.ticks ?? defaultsW.ticks);
        const shapes = options?.shapes ?? defaultsW.shapes;
        const scalar = Number(options?.scalar ?? defaultsW.scalar);
        const flat = !!options?.flat;
        const origin = getOriginW(options);
        const startX = canvas.width * origin.x;
        const startY = canvas.height * origin.y;

        let temp = particleCount;
        const fettis: any[] = [];
        while (temp--) {
          fettis.push(
            randomPhysicsW({
              x: startX, y: startY, angle, spread, startVelocity,
              color: colors[temp % colors.length], shape: shapes[randomIntW(0, shapes.length)],
              ticks, decay, gravity, drift, scalar, flat
            })
          );
        }
        const size = { width: canvas.width, height: canvas.height };
        const done = () => { animation = null; };
        const promise = animateW(canvas, fettis, size, done);
        animation = { promise, reset: () => {} };
        return promise;
      };
      (fire as any).reset = () => { /* no-op in worker */ };
      return fire as any;
    }

    module.exports.create = (canvas: OffscreenCanvas) => createW(canvas);
    module.exports.reset = () => {};
    module.exports.Promise = Promise;
  })();
}

// --- public API ---

let defaultFire: FireWithReset | null = null;

function getDefaultFire(): FireWithReset {
  if (!defaultFire) {
    defaultFire = confettiCannon(null, { useWorker: true, resize: true });
  }
  return defaultFire;
}

export function create(canvas: HTMLCanvasElement, globalOptions?: { useWorker?: boolean; resize?: boolean }): FireWithReset {
  return confettiCannon(canvas, globalOptions || {});
}

function confettiCannon(canvas: HTMLCanvasElement | null, globalOpts?: { useWorker?: boolean; resize?: boolean }): FireWithReset {
  const isLibCanvas = !canvas;
  const allowResize = !!(globalOpts && (globalOpts as any).resize);
  const globalDisableForReducedMotion = prop(globalOpts, 'disableForReducedMotion', Boolean) as boolean || false;
  const shouldUseWorker = canUseWorker && !!(globalOpts && (globalOpts as any).useWorker);
  const getWorker = getWorkerFactory();
  const worker = shouldUseWorker ? getWorker() : null;
  const resizer = isLibCanvas ? setCanvasWindowSize : setCanvasRectSize;
  let initialized = (canvas && worker) ? !!(canvas as any).__confetti_initialized : false;
  const preferLessMotion = typeof globalObj.matchMedia === 'function' && globalObj.matchMedia('(prefers-reduced-motion)').matches;
  let animationObj: null | ReturnType<typeof animate> = null;
  let hasResizeEventRegistered = false;

  function fireLocal(options: ConfettiOptions, size: { width: number | null; height: number | null }, done: () => void) {
    const particleCount = prop(options, 'particleCount', onlyPositiveInt) as number;
    const angle = prop(options, 'angle', Number) as number;
    const spread = prop(options, 'spread', Number) as number;
    const startVelocity = prop(options, 'startVelocity', Number) as number;
    const decay = prop(options, 'decay', Number) as number;
    const gravity = prop(options, 'gravity', Number) as number;
    const drift = prop(options, 'drift', Number) as number;
    const colors = prop(options, 'colors', colorsToRgb) as RGB[];
    const ticks = prop(options, 'ticks', Number) as number;
    const shapes = (options && options.shapes) || defaults.shapes;
    const scalar = prop(options, 'scalar', Number) as number;
    const flat = !!prop(options, 'flat') as boolean;
    const origin = getOrigin(options);

    const fettis: Fetti[] = [];
    const startX = (canvas as HTMLCanvasElement).width * origin.x;
    const startY = (canvas as HTMLCanvasElement).height * origin.y;

    for (let i = 0; i < particleCount; i++) {
      fettis.push(
        randomPhysics({
          x: startX, y: startY, angle, spread, startVelocity,
          color: colors[i % colors.length],
          shape: shapes[randomInt(0, shapes.length)] as any,
          ticks, decay, gravity, drift, scalar, flat
        })
      );
    }

    if (animationObj) return animationObj.addFettis(fettis);
    animationObj = animate(canvas as HTMLCanvasElement, fettis, resizer, size, done);
    return animationObj.promise;
  }

  const fire: FireWithReset = ((options?: ConfettiOptions) => {
    const disableForReducedMotion = globalDisableForReducedMotion || (prop(options, 'disableForReducedMotion', Boolean) as boolean);
    const zIndex = prop(options, 'zIndex', Number) as number;

    if (disableForReducedMotion && preferLessMotion) return Promise.resolve();

    if (isLibCanvas && animationObj) {
      canvas = animationObj.canvas;
    } else if (isLibCanvas && !canvas) {
      canvas = getCanvas(zIndex);
      document.body.appendChild(canvas);
    }

    if (allowResize && !initialized) resizer(canvas as HTMLCanvasElement);

    const size = { width: (canvas as HTMLCanvasElement).width, height: (canvas as HTMLCanvasElement).height };

    if (worker && !initialized) (worker as any).init(canvas);
    initialized = true;
    if (worker) (canvas as any).__confetti_initialized = true;

    const onResize = () => {
      if (worker) {
        const obj = {
          getBoundingClientRect: () => (!isLibCanvas ? (canvas as HTMLCanvasElement).getBoundingClientRect() : undefined),
          width: 0,
          height: 0,
        } as any;
        resizer(obj as HTMLCanvasElement);
        (worker as any).postMessage({ resize: { width: obj.width, height: obj.height } });
        return;
      }
      size.width = size.height = null;
    };

    const done = () => {
      animationObj = null;
      if (allowResize) {
        hasResizeEventRegistered = false;
        globalObj.removeEventListener('resize', onResize);
      }
      if (isLibCanvas && canvas) {
        if (document.body.contains(canvas)) document.body.removeChild(canvas);
        canvas = null;
        initialized = false;
      }
    };

    if (allowResize && !hasResizeEventRegistered) {
      hasResizeEventRegistered = true;
      globalObj.addEventListener('resize', onResize, false);
    }

    if (worker) {
      return (worker as any).fire(options || {}, size, done);
    }
    return fireLocal(options || {}, size, done);
  }) as FireWithReset;

  fire.reset = () => {
    if (worker) (worker as any).reset();
    if (animationObj) animationObj.reset();
  };

  return fire;
}

export function shapeFromPath(pathData: string | { path: string; matrix?: number[] }): PathShape {
  if (!canUsePaths) throw new Error('path confetti are not supported in this browser');

  let path: string;
  let matrix: number[] | undefined;

  if (typeof pathData === 'string') {
    path = pathData;
  } else {
    path = pathData.path;
    matrix = pathData.matrix;
  }

  // @ts-ignore
  const path2d = new Path2D(path);
  const tempCanvas = document.createElement('canvas');
  const tempCtx = tempCanvas.getContext('2d')!;

  if (!matrix) {
    const maxSize = 1000;
    let minX = maxSize, minY = maxSize, maxX = 0, maxY = 0;
    for (let x = 0; x < maxSize; x += 2) {
      for (let y = 0; y < maxSize; y += 2) {
        // @ts-ignore
        if (tempCtx.isPointInPath(path2d, x, y, 'nonzero')) {
          minX = Math.min(minX, x);
          minY = Math.min(minY, y);
          maxX = Math.max(maxX, x);
          maxY = Math.max(maxY, y);
        }
      }
    }
    const width = maxX - minX;
    const height = maxY - minY;
    const maxDesiredSize = 10;
    const scale = Math.min(maxDesiredSize / width, maxDesiredSize / height);

    matrix = [
      scale, 0, 0, scale,
      -Math.round((width / 2) + minX) * scale,
      -Math.round((height / 2) + minY) * scale
    ];
  }

  return { type: 'path', path, matrix };
}

export function shapeFromText(textData: string | { text: string; scalar?: number; fontFamily?: string; color?: string }): BitmapShape {
  let text: string;
  let scalar = 1;
  let color = '#000000';
  let fontFamily =
    '"Apple Color Emoji", "Segoe UI Emoji", "Segoe UI Symbol", "Noto Color Emoji", "EmojiOne Color", "Android Emoji", "Twemoji Mozilla", "system emoji", sans-serif';

  if (typeof textData === 'string') {
    text = textData;
  } else {
    text = textData.text;
    if ('scalar' in textData && isOk(textData.scalar)) scalar = textData.scalar!;
    if ('fontFamily' in textData && isOk(textData.fontFamily)) fontFamily = textData.fontFamily!;
    if ('color' in textData && isOk(textData.color)) color = textData.color!;
  }

  const fontSize = 10 * scalar;
  const font = `${fontSize}px ${fontFamily}`;

  let canvas = new OffscreenCanvas(fontSize, fontSize);
  let ctx = canvas.getContext('2d')!;
  (ctx as any).font = font;
  const size = (ctx as any).measureText(text);
  let width = Math.ceil(size.actualBoundingBoxRight + size.actualBoundingBoxLeft);
  let height = Math.ceil(size.actualBoundingBoxAscent + size.actualBoundingBoxDescent);

  const padding = 2;
  const x = size.actualBoundingBoxLeft + padding;
  const y = size.actualBoundingBoxAscent + padding;
  width += padding + padding;
  height += padding + padding;

  canvas = new OffscreenCanvas(width, height);
  ctx = canvas.getContext('2d')!;
  (ctx as any).font = font;
  (ctx as any).fillStyle = color;
  (ctx as any).fillText(text, x, y);

  const scale = 1 / scalar;

  return {
    type: 'bitmap',
    bitmap: (canvas as any).transferToImageBitmap(),
    matrix: [scale, 0, 0, scale, -width * scale / 2, -height * scale / 2]
  };
}

// default export (lazy, uses worker + resize)
const defaultExport: FireWithReset = ((options?: ConfettiOptions) => getDefaultFire()(options)) as FireWithReset;
defaultExport.reset = () => getDefaultFire().reset();

export default defaultExport;
