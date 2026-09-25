import { createApp } from 'vue'
import App from '@/App.vue'
import { loadAssetHost } from '@/assets'
import { router } from '@/router'
import { loadSession } from '@/session'
import '@/style.css'

void loadAssetHost()
void loadSession()
createApp(App).use(router).mount('#app')
