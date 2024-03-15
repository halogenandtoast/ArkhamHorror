import './assets/main.scss'
import { createApp } from 'vue'
import { createPinia } from 'pinia'
import FloatingVue from 'floating-vue'
import Toast from "vue-toastification";
import { createVfm } from 'vue-final-modal'
import "vue-toastification/dist/index.css";
import 'vue-final-modal/style.css'
import App from './App.vue'
import router from './router'
import { FontAwesomeIcon } from "@fortawesome/vue-fontawesome";
import { library } from "@fortawesome/fontawesome-svg-core";
import { faExpeditedssl } from "@fortawesome/free-brands-svg-icons";
import { faSearch, faList, faImage, faAngleDown, faUndo, faTrash, faEye, faCopy, faExternalLink, faRefresh } from '@fortawesome/free-solid-svg-icons'
import * as VueI18n from 'vue-i18n'
import messages from '@/locales/messages'

const currentLanguage = localStorage.getItem('language') ?? 'en'

const i18n = VueI18n.createI18n({
  locale: currentLanguage, // set locale
  fallbackLocale: 'en', // set fallback locale
  legacy: false,
  warnHtmlMessage: false,
  messages
})

library.add(faSearch, faList, faImage, faAngleDown, faExpeditedssl, faUndo, faTrash, faEye, faCopy, faExternalLink, faRefresh)

const pinia = createPinia()
const vfm = createVfm()

const app = createApp(App).
  use(router).
  use(pinia).
  use(FloatingVue).
  use(Toast, {}).
  use(vfm).
  use(i18n).
  component("font-awesome-icon", FontAwesomeIcon)

app.mount('#app')
