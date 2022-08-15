import { defineStore } from 'pinia'
import api from '@/api';
import {
  Credentials,
  Registration,
  Authentication,
  User,
} from '@/types';

export interface UserState {
  currentUser: User | null
  token: string | null
}

export const useUserStore = defineStore("user", {
  state: () => ({
    currentUser: null,
    token: null,
  } as UserState),

  getters: {
    getCurrentUser(state) {
      return state.currentUser
    }
  },

  actions: {
    async authenticate(credentials: Credentials) {
      const authentication = await api.post<Authentication>('authenticate', credentials)
      this.token = authentication.data.token
      this.setCurrentUser()
    },

    async register(registration: Registration) {
      const authentication = await api.post<Authentication>('register', registration)
      this.token = authentication.data.token
      this.setCurrentUser()
    },

    async logout() {
      localStorage.removeItem('token')
      delete api.defaults.headers.common.Authorization
      this.signOut()
    },

    async setCurrentUser() {
      if (this.token) {
        localStorage.setItem('token', this.token);
        api.defaults.headers.common.Authorization = `Token ${this.token}`;
        try {
          const whoami = await api.get<User>('whoami')
          this.currentUser = whoami.data
        } catch (err) {
          this.logout()
        }
      }
    },

    async loadUserFromStorage() {
      const token = localStorage.getItem('token');
      if (token !== null && token !== undefined) {
        this.token = token
        this.setCurrentUser()
      }
    },

    async signOut() {
      this.currentUser = null
      this.token = null
    }
  }
})
