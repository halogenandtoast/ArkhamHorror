import { createApp } from 'vue'
import { createPinia } from 'pinia'
import FloatingVue from 'floating-vue'
import Toast from "vue-toastification";
import "vue-toastification/dist/index.css";
import App from './App.vue'
import router from './router'
import { FontAwesomeIcon } from "@fortawesome/vue-fontawesome";
import { library } from "@fortawesome/fontawesome-svg-core";
import { faExpeditedssl } from "@fortawesome/free-brands-svg-icons";
import { faAngleDown, faTrash, faEye, faCopy, faExternalLink, faRefresh } from '@fortawesome/free-solid-svg-icons'

library.add(faAngleDown, faExpeditedssl, faTrash, faEye, faCopy, faExternalLink, faRefresh)

const pinia = createPinia()

createApp(App).
  use(router).
  use(pinia).
  use(FloatingVue).
  use(Toast, {}).
  component("font-awesome-icon", FontAwesomeIcon).
  mount('#app')
