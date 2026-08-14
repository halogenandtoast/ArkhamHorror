// Writes .gz and .br siblings for the text assets in dist/ so nginx can serve
// them precompressed instead of gzipping on every request.
//
// Why precompress at all: prod.nginxconf runs gzip at level 6 on the fly, and
// Ubuntu's nginx has no brotli module (nginx.org doesn't package one either),
// so brotli is only reachable as static files. Doing it at build time also
// lets us pay for the slowest, smallest settings once instead of per request.
//
// nginx picks these up via gzip_static for .gz, and via the explicit
// Accept-Encoding + `-f` check in prod.nginxconf for .br. That check tests the
// file on disk, so anything this script skips simply falls back to gzip —
// coverage here is an optimization, never a correctness requirement.
//
// Runs automatically as npm's `postbuild`.

const fs = require('fs')
const path = require('path')
const zlib = require('zlib')

// Below roughly one MTU there is nothing to win, and tiny files can even grow.
const MIN_BYTES = 1024

// Quality 11 is worth it on the critical path but scales badly: on the 6 MB
// cards_en.json it takes 7.2s for 602 KB, where quality 10 takes 2.5s for
// 611 KB. Pay for 11 on the small stuff, drop to 10 once a file is big enough
// for the extra minutes to show up in every image build.
const MAX_QUALITY_BYTES = 2 * 1024 * 1024

// Vite copies public/ verbatim, which includes the full ArkhamDB exports at
// the dist root. The app never fetches those — dbCards.ts requests the slimmed
// copies under /cards/ — so compressing ~130 MB of them costs minutes per
// build and saves no bytes on the wire. If something starts serving them,
// delete this and they get precompressed like everything else (until then
// nginx still gzips them on the fly).
const SKIP = /^cards_[a-z-]+\.json$/i

const EXTENSIONS = new Set([
  '.js',
  '.mjs',
  '.css',
  '.json',
  '.html',
  '.svg',
  '.txt',
  '.xml',
  '.map',
])

function brotliSync(buf) {
  const input = Buffer.isBuffer(buf) ? buf : Buffer.from(buf)
  const quality =
    input.length > MAX_QUALITY_BYTES ? 10 : zlib.constants.BROTLI_MAX_QUALITY
  return zlib.brotliCompressSync(input, {
    params: {
      [zlib.constants.BROTLI_PARAM_QUALITY]: quality,
      [zlib.constants.BROTLI_PARAM_SIZE_HINT]: input.length,
    },
  })
}

function gzipSync(buf) {
  return zlib.gzipSync(buf, { level: 9 })
}

function* walk(dir) {
  for (const entry of fs.readdirSync(dir, { withFileTypes: true })) {
    const full = path.join(dir, entry.name)
    if (entry.isDirectory()) yield* walk(full)
    else if (entry.isFile()) yield full
  }
}

function precompressDir(dir) {
  const stats = { files: 0, raw: 0, gz: 0, br: 0, skipped: 0 }

  for (const file of walk(dir)) {
    const ext = path.extname(file)
    if (!EXTENSIONS.has(ext)) continue
    if (file.endsWith('.gz') || file.endsWith('.br')) continue
    if (path.dirname(file) === dir && SKIP.test(path.basename(file))) {
      stats.skipped++
      continue
    }

    const raw = fs.readFileSync(file)
    if (raw.length < MIN_BYTES) {
      stats.skipped++
      continue
    }

    // Skip work already done upstream (slim-cards.cjs precompresses the card
    // JSON as it writes it) when the sibling is newer than its source.
    const mtime = fs.statSync(file).mtimeMs
    const fresh = (p) => fs.existsSync(p) && fs.statSync(p).mtimeMs >= mtime

    const gzPath = file + '.gz'
    const brPath = file + '.br'
    if (!fresh(gzPath)) fs.writeFileSync(gzPath, gzipSync(raw))
    if (!fresh(brPath)) fs.writeFileSync(brPath, brotliSync(raw))

    stats.files++
    stats.raw += raw.length
    stats.gz += fs.statSync(gzPath).size
    stats.br += fs.statSync(brPath).size
  }

  return stats
}

module.exports = { brotliSync, gzipSync, precompressDir }

if (require.main === module) {
  const dist = path.resolve(__dirname, '..', 'dist')
  if (!fs.existsSync(dist)) {
    console.error(`precompress: ${dist} does not exist — run the build first`)
    process.exit(1)
  }

  const started = Date.now()
  const { files, raw, gz, br, skipped } = precompressDir(dist)
  const mb = (n) => (n / 1024 / 1024).toFixed(2)
  const pct = gz > 0 ? (100 - (br / gz) * 100).toFixed(1) : '0.0'

  console.log(
    `precompress: ${files} files (${skipped} too small), ` +
      `${mb(raw)} MB raw -> ${mb(gz)} MB gzip -> ${mb(br)} MB brotli ` +
      `(brotli is ${pct}% smaller than gzip) in ${((Date.now() - started) / 1000).toFixed(1)}s`,
  )
}
