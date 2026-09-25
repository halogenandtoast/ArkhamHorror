import { createRouter, createWebHashHistory, RouteRecordRaw } from 'vue-router'
import { useUserStore } from '@/stores/user'
import { getToken } from '@/authToken'
import baseRoutes from '@/routes';
import arkhamRoutes from '@/arkham/routes';

const routes: Array<RouteRecordRaw> = [
  ...baseRoutes,
  ...arkhamRoutes
]

const router = createRouter({
  history: createWebHashHistory(),
  routes
})


router.beforeEach(async (to, _from, next) => {
  const store = useUserStore()
  await store.loadUserFromStorage()

  if (to.matched.some((record) => record.meta && record.meta.requiresAuth)) {
    if (getToken() === null) {
      next({ path: '/sign-in', query: { nextUrl: to.fullPath } });
    } else {
      if (to.matched.some((record) => record.meta && record.meta.requiresAdmin)) {
        if (store.isAdmin) {
          document.title = `${to.meta.title}`
          next();
        } else {
          next({ path: '/' })
        }
      } else {
        document.title = `${to.meta.title}`
        next();
      }
    }
  } else if (to.matched.some((record) => record.meta && record.meta.guest)) {
    if (getToken() === null) {
      document.title = `${to.meta.title}`
      next();
    } else {
      next({ path: '/' });
    }
  } else {
    next();
  }
});

export default router
