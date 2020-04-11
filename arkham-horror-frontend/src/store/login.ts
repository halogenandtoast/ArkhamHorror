import { GetterTree, ActionTree, MutationTree } from 'vuex';
import {
  RootState,
  LoginState,
  Credentials,
  Registration,
  Authentication,
  User,
} from '@/types';
import api from '@/api';
import router from '@/router';

const mutations: MutationTree<LoginState> = {
  signIn(state, user) {
    state.currentUser = user;
  },
  signOut(state) {
    state.currentUser = undefined;
  },
};

const actions: ActionTree<LoginState, RootState> = {
  authenticate({ dispatch }, credentials: Credentials): void {
    api.post<Authentication>('authenticate', credentials).then((authentication) => {
      dispatch('setCurrentUser', authentication.data);
      router.push({ path: '/' });
    });
  },
  register({ dispatch }, registration: Registration): void {
    api.post<Authentication>('register', registration).then((authentication) => {
      dispatch('setCurrentUser', authentication.data);
      router.push({ path: '/' });
    });
  },
  logout({ commit }): void {
    localStorage.removeItem('token');
    delete api.defaults.headers.common.Authorization;
    commit('signOut');
    router.push({ path: '/' });
  },
  setCurrentUser({ commit }, authentication: Authentication): void {
    localStorage.setItem('token', authentication.token);
    api.defaults.headers.common.Authorization = `Token ${authentication.token}`;
    api.get<User>('whoami').then((whoami) => {
      commit('signIn', whoami.data);
    });
  },
  loadUserFromStorage({ dispatch }): void {
    const token = localStorage.getItem('token');
    if (token !== null && token !== undefined) {
      dispatch('setCurrentUser', { token });
    }
  },
};

const getters: GetterTree<LoginState, RootState> = {
  currentUser: (state) => state.currentUser,
};

const state: LoginState = {
  currentUser: undefined,
};

const store = {
  state,
  getters,
  actions,
  mutations,
};

export default store;
