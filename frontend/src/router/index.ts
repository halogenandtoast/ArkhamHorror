import { createRouter, createWebHashHistory, RouteRecordRaw } from 'vue-router'
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

router.beforeEach((to, from, next) => {
  if (to.matched.some((record) => record.meta && record.meta.requiresAuth)) {
    if (localStorage.getItem('token') === null) {
      next({ path: '/sign-in', query: { nextUrl: to.fullPath } });
    } else {
      next();
    }
  } else if (to.matched.some((record) => record.meta && record.meta.guest)) {
    if (localStorage.getItem('token') === null) {
      next();
    } else {
      next({ path: '/' });
    }
  } else {
    next();
  }
});

export default router
