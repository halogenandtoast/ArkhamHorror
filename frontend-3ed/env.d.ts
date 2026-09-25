/// <reference types="vite/client" />

interface ImportMetaEnv {
  readonly VITE_ASSET_HOST?: string
  readonly VITE_AUTH_COOKIE_DOMAIN?: string
  readonly VITE_MAIN_SITE_URL?: string
}

interface ImportMeta {
  readonly env: ImportMetaEnv
}
