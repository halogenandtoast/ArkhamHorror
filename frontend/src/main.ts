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
import { faGear, faLayerGroup, faBan, faCircleExclamation, faGhost, faLocationDot, faSearch, faList, faImage, faAngleDown, faUndo, faTrash, faEye, faCopy, faExternalLink, faRefresh, faBook, faChevronRight, faBars, faTimes, faShieldHeart, faWrench, faPaperclip, faArrowLeft, faArrowUp, faStore, faTriangleExclamation, faShuffle, faTrophy, faDownload, faCheckDouble, faFlask, faBug, faPen, faThumbsUp } from '@fortawesome/free-solid-svg-icons'
import * as VueI18n from 'vue-i18n'
import { loadLocaleMessages, normalizeLocale } from '@/locales/messages'
import { preferredLanguage } from '@/locales/language'
import mitt from 'mitt';

library.add(faBan, faLocationDot, faCircleExclamation, faGhost, faSearch, faList, faImage, faAngleDown, faExpeditedssl, faUndo, faTrash, faEye, faCopy, faExternalLink, faRefresh, faBook, faChevronRight, faBars, faTimes, faShieldHeart, faWrench, faPaperclip, faArrowLeft, faArrowUp, faStore, faTriangleExclamation, faShuffle, faTrophy, faGear, faLayerGroup, faDownload, faCheckDouble, faFlask, faBug, faPen, faThumbsUp)

async function bootstrap() {
  const language = localStorage.getItem('language')
  const naviLanguage = preferredLanguage(navigator.language || 'en')
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
    locale: currentLocale, // set locale
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
        'chaos-bag-stats-popover': {
          $extend: 'dropdown',
        },
        /* Used in four places and never registered. The others survive only by
         * passing every option explicitly; anything that leaned on the theme for
         * a default -- its triggers, say -- read undefined and threw. */
        'cards-under-popover': {
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
