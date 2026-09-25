import { fileURLToPath, URL } from 'node:url'
import { defineConfig } from 'vite'
import vue from '@vitejs/plugin-vue'

export default defineConfig({
  plugins: [vue()],
  resolve: {
    alias: {
      '@': fileURLToPath(new URL('./src', import.meta.url)),
    },
  },
  server: {
    port: 8081,
    strictPort: true,
    proxy: {
      '^/api': { target: 'http://127.0.0.1:3002', changeOrigin: true, ws: true },
      '^/health': { target: 'http://127.0.0.1:3002', changeOrigin: true },
    },
  },
  build: {
    outDir: 'dist',
  },
})
