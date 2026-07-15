import { fileURLToPath, URL } from 'node:url'

import { defineConfig } from 'vite'
import vue from '@vitejs/plugin-vue'
import fs from 'node:fs'
import path from 'node:path'

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

// https://vitejs.dev/config/
export default defineConfig({
  plugins: [
    vue(),
    homebrewImages(),
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
