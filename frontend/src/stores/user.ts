import { defineStore } from 'pinia'
import { ref } from 'vue'
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

export const useUserStore = defineStore("user", () => {
  const currentUser = ref<User | null>(null)
  const token = ref<string | null>(null)
  const isAdmin = ref(false)

  async function authenticate(credentials: Credentials) {
    const authentication = await api.post<Authentication>('authenticate', credentials)
    token.value = authentication.data.token
    setCurrentUser()
  }

  async function register(registration: Registration) {
    const authentication = await api.post<Authentication>('register', registration)
    token.value = authentication.data.token
    setCurrentUser()
  }

  function logout() {
    localStorage.removeItem('arkham-token')
    delete api.defaults.headers.common.Authorization
    signOut()
  }

  async function setCurrentUser() {
    if (token.value) {
      localStorage.setItem('arkham-token', token.value);
      api.defaults.headers.common.Authorization = `Token ${token.value}`;
      try {
        const whoami = await api.get<User>('whoami')
        currentUser.value = whoami.data
        isAdmin.value = whoami.data.admin
      } catch (_err) {
        logout()
      }
    }
  }

  function loadUserFromStorage() {
    const tokenFromStorage = localStorage.getItem('arkham-token');
    if (tokenFromStorage !== null && tokenFromStorage !== undefined) {
      token.value = tokenFromStorage
      setCurrentUser()
    }
  }

  function signOut() {
    currentUser.value = null
    token.value = null
  }

  loadUserFromStorage()

  return { token, currentUser, isAdmin, loadUserFromStorage, authenticate, register }
})
