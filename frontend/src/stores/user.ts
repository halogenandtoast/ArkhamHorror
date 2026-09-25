import { defineStore } from 'pinia'
import { ref } from 'vue'
import api from '@/api';
import { getToken, setToken, clearToken } from '@/authToken';
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
    clearToken()
    delete api.defaults.headers.common.Authorization
    signOut()
  }

  async function setCurrentUser() {
    if (token.value) {
      setToken(token.value);
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

  async function deleteAccount() {
    await api.delete('account')
    logout()
  }

  async function loadUserFromStorage() {
    if (currentUser.value) return
    const tokenFromStorage = getToken();
    if (tokenFromStorage !== null) {
      token.value = tokenFromStorage
      await setCurrentUser()
    }
  }

  function signOut() {
    currentUser.value = null
    token.value = null
  }

  return { token, currentUser, isAdmin, loadUserFromStorage, authenticate, register, logout, deleteAccount, setCurrentUser }
})
