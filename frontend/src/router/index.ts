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
    document.title = `${to.meta.title}`
    if (localStorage.getItem('arkham-token') === null) {
      next({ path: '/sign-in', query: { nextUrl: to.fullPath } });
    } else {
      next();
    }
  } else if (to.matched.some((record) => record.meta && record.meta.guest)) {
    if (localStorage.getItem('arkham-token') === null) {
      next();
    } else {
      next({ path: '/' });
    }
  } else {
    next();
  }
});

export default router
