import { GetterTree, ActionTree, MutationTree } from 'vuex';
import { AxiosError } from 'axios'
import {
  RootState,
  LoginState,
  Credentials,
  Registration,
  Authentication,
  User,
} from '@/types';
import api from '@/api';

const mutations: MutationTree<LoginState> = {
  signIn(state, user) {
    state.currentUser = user;
  },
  signOut(state) {
    state.currentUser = undefined;
  },
};

const actions: ActionTree<LoginState, RootState> = {
  authenticate({ dispatch }, credentials: Credentials): Promise<void> {
    return api.post<Authentication>('authenticate', credentials).then((authentication) => {
      dispatch('setCurrentUser', authentication.data);
    });
  },
  register({ dispatch }, registration: Registration): Promise<void> {
    return api.post<Authentication>('register', registration).then((authentication) => {
      dispatch('setCurrentUser', authentication.data);
    });
  },
  logout({ commit }): Promise<void> {
    localStorage.removeItem('token')
    delete api.defaults.headers.common.Authorization
    commit('signOut')
    return Promise.resolve()
  },
  setCurrentUser({ commit, dispatch }, authentication: Authentication): Promise<void> {
    localStorage.setItem('token', authentication.token);
    api.defaults.headers.common.Authorization = `Token ${authentication.token}`;
    return api.get<User>('whoami').then(
      (whoami) => {
        commit('signIn', whoami.data);
      }).catch((reason: AxiosError) => {
        if (reason.response && reason.response.status == 401) {
          dispatch('logout');
        }
      });
  },
  loadUserFromStorage({ dispatch }): Promise<void> {
    const token = localStorage.getItem('token');
    if (token !== null && token !== undefined) {
      return dispatch('setCurrentUser', { token })
    }
    return Promise.resolve()
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
