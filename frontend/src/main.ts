import { createApp } from 'vue'
import App from './App.vue'
import router from './router'
import store from './store'
import { FontAwesomeIcon } from "@fortawesome/vue-fontawesome";
import { library } from "@fortawesome/fontawesome-svg-core";
import { faExpeditedssl } from "@fortawesome/free-brands-svg-icons";

library.add(faExpeditedssl)

createApp(App).use(router).use(store).component("font-awesome-icon", FontAwesomeIcon).mount('#app')
