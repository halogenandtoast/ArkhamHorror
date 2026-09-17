import { fileURLToPath, URL } from 'node:url'

import { defineConfig } from 'vite'
import vue from '@vitejs/plugin-vue'
import fs from 'node:fs'
import path from 'node:path'
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

// https://vitejs.dev/config/
export default defineConfig({
  plugins: [
    vue(),
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
