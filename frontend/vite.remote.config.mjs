// Local-only: serve this checkout's frontend against the public API. Not committed.
import { mergeConfig } from 'vite'
import base from './vite.config.js'

const target = 'https://arkhamhorror.app'
const proxied = { target, changeOrigin: true, secure: true }
export default mergeConfig(base, {
  server: { proxy: { '^/api': { ...proxied, ws: true }, '^/health': { ...proxied, ws: false } } },
  // Let `vite preview` answer to tunnel hostnames (e.g. *.trycloudflare.com), not just IPs/localhost.
  preview: { allowedHosts: true },
}, true)
