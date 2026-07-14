import { fileURLToPath, URL } from 'node:url'

import { defineConfig } from 'vite'
import vue from '@vitejs/plugin-vue'

// https://vitejs.dev/config/
export default defineConfig({
  plugins: [
    vue(),
  ],
  resolve: {
    alias: {
      '@': fileURLToPath(new URL('./src', import.meta.url)),
      '@homebrew': fileURLToPath(new URL('../backend/arkham-api/library/Arkham/Homebrew', import.meta.url))
    }
  },
  server: {
    port: 8080,
    fs: {
      // homebrew campaign locales live beside their backend code
      allow: ['.', '../backend/arkham-api/library/Arkham/Homebrew']
    },
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
