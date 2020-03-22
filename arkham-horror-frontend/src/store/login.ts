import { ActionTree, MutationTree } from 'vuex';
import {
  RootState,
  LoginState,
  Credentials,
  Registration,
  Authentication,
  User,
} from '../types';
import api from '../api';

const mutations: MutationTree<LoginState> = {
  signIn(state, user) {
    state.currentUser = user;
  },
};

const actions: ActionTree<LoginState, RootState> = {
  authenticate({ dispatch }, credentials: Credentials): void {
    api.post<Authentication>('authenticate', credentials).then((authentication) => {
      dispatch('setCurrentUser', authentication.data);
    });
  },
  register({ dispatch }, registration: Registration): void {
    api.post<Authentication>('register', registration).then((authentication) => {
      dispatch('setCurrentUser', authentication.data);
    });
  },
  setCurrentUser({ commit }, authentication: Authentication): void {
    localStorage.setItem('token', authentication.token);
    api.defaults.headers.common.Authorization = `Bearer ${authentication.token}`;
    api.get<User>('whoami').then((whoami) => {
      commit('signIn', whoami.data);
    });
  },
};

const state: LoginState = {
  currentUser: undefined,
};

const store = {
  state,
  actions,
  mutations,
};

export default store;
