import { defineStore } from 'pinia'
import api from '@/api';

export interface SiteSettings {
  assetHost: string
}

export const baseUrl = import.meta.env.PROD ? "https://assets.arkhamhorror.app" : ''

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
      //const assetHost = localStorage.getItem('asset-host');
      // if (assetHost !== null && assetHost !== undefined) {
      //   this.assetHost = assetHost
      // } else {
        const result = await api.get('/site-settings')
        if (result.status === 200) {
          this.assetHost = result.data.assetHost ?? baseUrl
          //localStorage.setItem('asset-host', this.assetHost)
        } else {
          this.assetHost = baseUrl
          //localStorage.setItem('asset-host', baseUrl)
        }
      //}
    }
  }
})
