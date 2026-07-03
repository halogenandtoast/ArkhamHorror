import './styles/index.css'
import { createApp } from 'vue'
import { createPinia } from 'pinia'
import FloatingVue from 'floating-vue'
import Toast from "vue-toastification";
import { createVfm } from 'vue-final-modal'
import App from './App.vue'
import router from './router'
import { FontAwesomeIcon } from "@fortawesome/vue-fontawesome";
import { library } from "@fortawesome/fontawesome-svg-core";
import { faExpeditedssl } from "@fortawesome/free-brands-svg-icons";
import { faBan, faCircleExclamation, faGhost, faLocationDot, faSearch, faList, faImage, faAngleDown, faUndo, faTrash, faEye, faCopy, faExternalLink, faRefresh, faBook, faChevronRight, faBars, faTimes, faShieldHeart, faWrench, faPaperclip, faArrowLeft, faArrowUp, faStore, faTriangleExclamation } from '@fortawesome/free-solid-svg-icons'
import * as VueI18n from 'vue-i18n'
import { loadLocaleMessages, normalizeLocale } from '@/locales/messages'
import mitt from 'mitt';

library.add(faBan, faLocationDot, faCircleExclamation, faGhost, faSearch, faList, faImage, faAngleDown, faExpeditedssl, faUndo, faTrash, faEye, faCopy, faExternalLink, faRefresh, faBook, faChevronRight, faBars, faTimes, faShieldHeart, faWrench, faPaperclip, faArrowLeft, faArrowUp, faStore, faTriangleExclamation)

async function bootstrap() {
  const language = localStorage.getItem('language')
  const naviLanguage = (navigator.language || 'en').split('-')[0]
  const currentLanguage = language ?? naviLanguage
  const currentLocale = normalizeLocale(currentLanguage)
  if (!language) { localStorage.setItem('language', currentLanguage) }

  const loadedMessages: Record<string, any> = {}
  const fallback = await loadLocaleMessages('en')
  loadedMessages[fallback.locale] = fallback.messages

  if (currentLocale !== fallback.locale) {
    const current = await loadLocaleMessages(currentLocale)
    loadedMessages[current.locale] = current.messages
  }

  const i18n = VueI18n.createI18n({
    locale: currentLanguage, // set locale
    fallbackLocale: 'en', // set fallback locale
    legacy: false,
    warnHtmlMessage: false,
    messages: loadedMessages
  })

  const pinia = createPinia()
  const vfm = createVfm()
  const emitter = mitt()

  const app = createApp(App).
    use(router).
    use(pinia).
    use(FloatingVue, {
      themes: {
        'stack-indicator-popover': {
          $extend: 'dropdown',
        },
      },
    }).
    use(Toast, {}).
    use(vfm).
    use(i18n).
    component("font-awesome-icon", FontAwesomeIcon)

  app.config.globalProperties.emitter = emitter

  app.mount('#app')
}

void bootstrap()
