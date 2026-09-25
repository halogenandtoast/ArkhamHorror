import { createRouter, createWebHashHistory } from 'vue-router'
import LobbyScreen from '@/views/LobbyScreen.vue'
import TableScreen from '@/views/TableScreen.vue'

export const router = createRouter({
  history: createWebHashHistory(),
  routes: [
    { path: '/', component: LobbyScreen },
    { path: '/tables/:id', component: TableScreen, props: true },
    { path: '/:pathMatch(.*)*', redirect: '/' },
  ],
})
