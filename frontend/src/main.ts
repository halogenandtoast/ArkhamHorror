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

library.add(faSearch, faList, faImage, faAngleDown, faExpeditedssl, faUndo, faTrash, faEye, faCopy, faExternalLink, faRefresh)

const pinia = createPinia()
const vfm = createVfm()

const app = createApp(App).
  use(router).
  use(pinia).
  use(FloatingVue).
  use(Toast, {}).
  use(vfm).
  component("font-awesome-icon", FontAwesomeIcon)

app.mount('#app')
