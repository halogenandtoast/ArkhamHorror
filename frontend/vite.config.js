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
      '@': fileURLToPath(new URL('./src', import.meta.url))
    }
  },
  css: {
    preprocessorOptions: {
      scss: {
        additionalData: `@import "@/styles/_variables.scss";`
      }
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
      }
    }
  }
})
