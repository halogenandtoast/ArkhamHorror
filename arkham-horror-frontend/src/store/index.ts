import Vue from 'vue';
import Vuex from 'vuex';
import login from './login';
import game from './game';

Vue.use(Vuex);

export default new Vuex.Store({
  state: {
    version: '1.0',
  },
  mutations: {
  },
  actions: {
  },
  modules: {
    login,
    game,
  },
});
