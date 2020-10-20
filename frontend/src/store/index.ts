import { createStore } from 'vuex';
import login from './login';

export default createStore({
  state: {
    version: '1.0',
  },
  mutations: {
  },
  actions: {
  },
  modules: {
    login,
  },
});
