import { defineStore } from 'pinia'
import api from '@/api';

export interface SiteSettings {
  assetHost: string
}

const cdnUrl = "https://assets.arkhamhorror.app"
export const baseUrl = import.meta.env.VITE_ASSET_HOST !== undefined ? import.meta.env.VITE_ASSET_HOST : cdnUrl

export const useSiteSettingsStore = defineStore("site_settings", {
  state: () => ({
    assetHost: baseUrl,
  } as SiteSettings),

  getters: {
    getAssetHost(state) {
      return state.assetHost
    }
  },

  actions: {
    async init() {
      try {
        const result = await api.get('/site-settings')
        if (result.status === 200) {
          this.assetHost = result.data.assetHost ?? baseUrl
        }
      } catch {
        this.assetHost = baseUrl
      }
    }
  }
})
