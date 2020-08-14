import Vue from 'vue';
import { library } from '@fortawesome/fontawesome-svg-core';
import { faExpeditedssl } from '@fortawesome/free-brands-svg-icons';
import { FontAwesomeIcon } from '@fortawesome/vue-fontawesome';
import App from './App.vue';
import './registerServiceWorker';
import router from './router';
import store from './store';

library.add(faExpeditedssl);
Vue.component('font-awesome-icon', FontAwesomeIcon);

Vue.config.productionTip = false;

new Vue({
  router,
  store,
  render: (h) => h(App),
}).$mount('#app');
