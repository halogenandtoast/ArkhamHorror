import { fileURLToPath, URL } from 'node:url'

import { defineConfig } from 'vite'
import vue from '@vitejs/plugin-vue'
import fs from 'node:fs'
import path from 'node:path'
import process from 'node:process'
import { createRequire } from 'node:module'

const { slimCards } = createRequire(import.meta.url)('./scripts/slim-cards.cjs')

// Serves homebrew campaign images (frontend/homebrew/<campaign>/img/*) at
// their CDN paths (/img/arkham/homebrew/<campaign>/*) so a local
// VITE_ASSET_HOST keeps working in dev; production loads them from S3, synced
// there by `make sync-images`.
const homebrewImages = () => ({
  name: 'homebrew-images',
  configureServer(server) {
    server.middlewares.use((req, res, next) => {
      const match = req.url?.match(/^\/img\/arkham\/homebrew\/([a-z0-9-]+)\/(.+)$/)
      if (!match) return next()
      const homebrewDir = fileURLToPath(new URL('./homebrew', import.meta.url))
      const campaign = match[1]
      const file = path.join(homebrewDir, campaign, 'img', decodeURIComponent(match[2]).split('?')[0])
      if (!file.startsWith(path.join(homebrewDir, campaign, 'img')) || !fs.existsSync(file)) return next()
      res.setHeader('Content-Type', file.endsWith('.avif') ? 'image/avif' : file.endsWith('.jpg') ? 'image/jpeg' : 'image/png')
      fs.createReadStream(file).pipe(res)
    })
  },
})

// Serves the slimmed card data (/cards/cards_<lang>.json) that dbCards.ts
// fetches. `npm run build` writes those files via slim-cards.cjs, but nothing
// generates them for `vite dev`, so without this the SPA fallback answers with
// index.html and every card lookup fails on `Unexpected token '<'`. Slim the
// source export on demand instead, cached per language for the server's life.
const cardData = () => {
  const cache = new Map()

  return {
    name: 'card-data',
    configureServer(server) {
      const publicDir = fileURLToPath(new URL('./public', import.meta.url))

      server.middlewares.use((req, res, next) => {
        const match = req.url?.split('?')[0].match(/^\/cards\/(cards_[a-z]+(?:-[a-z]+)?\.json)$/)
        if (!match) return next()

        const file = match[1]
        // A generated copy (from a previous build) wins, so dev matches prod.
        if (fs.existsSync(path.join(publicDir, 'cards', file))) return next()

        const source = path.join(publicDir, file)
        if (!fs.existsSync(source)) return next()

        const mtime = fs.statSync(source).mtimeMs
        let cached = cache.get(file)
        if (!cached || cached.mtime !== mtime) {
          const raw = fs.readFileSync(source, 'utf8').trim()
          if (!raw) return next()
          cached = { mtime, json: JSON.stringify(slimCards(JSON.parse(raw))) }
          cache.set(file, cached)
        }

        res.setHeader('Content-Type', 'application/json')
        res.end(cached.json)
      })
    },
  }
}

// Injects a <link rel="preload"> for the UI body font.
//
// Nothing discovers that font until the app stylesheet has been downloaded and
// parsed, because that is where the @font-face lives -- so on a cold load it
// used to start only once the app had mounted, which was 10s in. The preload
// scanner reads index.html before any of that, so naming the file there starts
// it alongside the entry chunk instead.
//
// It has to be a plugin rather than a line in index.html because the filename
// is content-hashed at build time. Only the 400 is preloaded: the bold is
// fetched when a bold glyph is first laid out, which is not the first paint.
//
// `crossorigin` is required even same-origin -- fonts are always fetched in
// CORS mode, and a preload without it is fetched a second time rather than
// reused, which is strictly worse than not preloading at all.
const preloadBodyFont = () => ({
  name: 'preload-body-font',
  enforce: 'post',
  transformIndexHtml(html, ctx) {
    // vite joins the hash with a dash: assets/NotoSans-400-DNZua9B2.woff2
    const name = Object.keys(ctx.bundle ?? {}).find((f) =>
      /\/NotoSans-400-[^/]+\.woff2$/.test(f),
    )
    if (!name) return html
    const base = process.env.VITE_ASSET_BASE || '/'
    return {
      html,
      tags: [
        {
          tag: 'link',
          attrs: {
            rel: 'preload',
            as: 'font',
            type: 'font/woff2',
            crossorigin: '',
            href: `${base}${name}`,
          },
          injectTo: 'head',
        },
      ],
    }
  },
})

// https://vitejs.dev/config/
export default defineConfig({
  /* Where the hashed bundles are fetched from.
   *
   * Unset (the default, and every dev run) they are same-origin, which is how
   * this has always worked. Set to the CDN at build time, the app's own origin
   * stops serving them: that origin is a DigitalOcean LB in front of nginx and
   * is HTTP/1.1, where six connections and no multiplexing made a cold load
   * crawl, while assets.arkhamhorror.app is CloudFront and speaks HTTP/2.
   * Measured on the same file at the same moment: 4.94s from the origin, 1.86s
   * from the CDN.
   *
   * This is safe in the order the deploy already runs: `make v2-deploy` calls
   * publish-frontend-assets.sh BEFORE the rollout, so every bundle a new
   * index.html can name is in the bucket before any pod serves that
   * index.html. prod.nginxconf's @cdn_assets fallback stays as it is -- it
   * still covers a tab that was loaded before a deploy. */
  base: process.env.VITE_ASSET_BASE || '/',
  plugins: [
    preloadBodyFont(),
    vue({
      template: {
        compilerOptions: {
          /* <selectedcontent> is a real element -- the part of a customizable
             <select> that mirrors the chosen option. Vue does not know it, and
             without this it looks for a component by that name and warns. */
          isCustomElement: (tag) => tag === 'selectedcontent',
        },
      },
    }),
    homebrewImages(),
    cardData(),
  ],
  resolve: {
    alias: {
      '@': fileURLToPath(new URL('./src', import.meta.url)),
      '@homebrew': fileURLToPath(new URL('./homebrew', import.meta.url))
    }
  },
  server: {
    port: 8080,
    proxy: {
      "^/api": {
        target: "http://127.0.0.1:3002",
        changeOrigin: true,
        secure: false,
        ws: true
      },
      "^/health": {
        target: "http://127.0.0.1:3002",
        changeOrigin: true,
        secure: false,
        ws: false
      }
    }
  }
})
