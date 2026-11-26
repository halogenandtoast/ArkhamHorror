import { defineStore } from 'pinia'
import api from '@/api';
import { setMaintenance } from '@/maintenance';

export interface SiteSettings {
  assetHost: string
  /* The server is closed to new accounts (INVITE_ONLY). Starts false because
   * /site-settings is fetched after mount: the sign-up link is shown until the
   * server says otherwise, and POST /register refuses regardless, so the worst
   * case is a link that disappears a moment later rather than a form that
   * silently fails. */
  inviteOnly: boolean
}

const cdnUrl = "https://assets.arkhamhorror.app"
export const baseUrl = import.meta.env.VITE_ASSET_HOST !== undefined ? import.meta.env.VITE_ASSET_HOST : cdnUrl

export const useSiteSettingsStore = defineStore("site_settings", {
  state: () => ({
    assetHost: baseUrl,
    inviteOnly: false,
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
          this.inviteOnly = result.data.inviteOnly === true
          // This endpoint stays open during a maintenance window (it reads no
          // database), so it is how a cold load learns the site is down --
          // the 503 interceptor only fires on calls that were refused.
          setMaintenance(result.data.maintenance === true)
          //localStorage.setItem('asset-host', this.assetHost)
        } else {
          this.assetHost = baseUrl
          this.inviteOnly = false
          //localStorage.setItem('asset-host', baseUrl)
        }
      //}
    }
  }
})
