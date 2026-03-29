import { fileURLToPath, URL } from 'node:url'

import { defineConfig } from 'vite'
import vue from '@vitejs/plugin-vue'

// https://vitejs.dev/config/
export default defineConfig(({ mode }) => {
  // Docker 环境使用 web 服务名访问后端，本地开发使用 localhost
  const isDocker = process.env.API_TARGET?.includes('docker') || false;
  const apiTarget = isDocker ? 'http://web:3000' : 'http://127.0.0.1:3000';
  
  return {
    plugins: [vue()],
    resolve: {
      alias: {
        '@': fileURLToPath(new URL('./src', import.meta.url))
      }
    },
    server: {
      port: 8080,
      host: '0.0.0.0',
      hmr: {
        overlay: false,
        timeout: 30000,
        host: '0.0.0.0',
        port: 8080,
        clientPort: 8080,
      },
      proxy: {
        "^/api": {
          target: apiTarget,
          changeOrigin: true,
          secure: false,
          ws: true
        },
        "^/health": {
          target: apiTarget,
          changeOrigin: true,
          secure: false,
          ws: false
        }
      }
    }
  }
})
