import { defineStore } from 'pinia'
import api from '@/api';

export interface SiteSettings {
  assetHost: string
  audioHost: string
}

const cdnUrl = "https://assets.arkhamhorror.app"
export const baseUrl = import.meta.env.VITE_ASSET_HOST !== undefined ? import.meta.env.VITE_ASSET_HOST : cdnUrl
export const audioBaseUrl = import.meta.env.VITE_AUDIO_HOST !== undefined ? import.meta.env.VITE_AUDIO_HOST : cdnUrl

export const useSiteSettingsStore = defineStore("site_settings", {
  state: () => ({
    assetHost: baseUrl,
    audioHost: audioBaseUrl,
  } as SiteSettings),

  getters: {
    getAssetHost(state) {
      return state.assetHost
    },
    getAudioHost(state) {
      return state.audioHost
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
          this.audioHost = result.data.audioHost ?? ''
          //localStorage.setItem('asset-host', this.assetHost)
        } else {
          this.assetHost = baseUrl
          this.audioHost = audioBaseUrl
          //localStorage.setItem('asset-host', baseUrl)
        }
      //}
    }
  }
})
