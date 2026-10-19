import { createRouter, createWebHashHistory, RouteRecordRaw } from 'vue-router'
import { useUserStore } from '@/stores/user'
import { getToken } from '@/authToken'
import { reloadIfStaleChunk } from '@/chunkRecovery'
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


/* Returns the redirect rather than calling next(): vue-router 5 deprecates the
 * next() callback (one warning per guard run) and 6 removes it. Returning
 * nothing proceeds, which is also how a route with neither flag passes through
 * -- and, as before, leaves its title alone. */
router.beforeEach(async (to) => {
  const store = useUserStore()
  await store.loadUserFromStorage()

  const flagged = (flag: 'requiresAuth' | 'requiresAdmin' | 'guest') =>
    to.matched.some((record) => record.meta && record.meta[flag])

  if (flagged('requiresAuth')) {
    if (getToken() === null) return { path: '/sign-in', query: { nextUrl: to.fullPath } }
    if (flagged('requiresAdmin') && !store.isAdmin) return { path: '/' }
    document.title = `${to.meta.title}`
    return
  }

  if (flagged('guest')) {
    if (getToken() !== null) return { path: '/' }
    document.title = `${to.meta.title}`
  }
});

/* Navigating to a route whose chunk was removed by a deploy fails here. It is
 * not an application error -- the route is fine, the bundle naming it is gone --
 * so reload and let the fresh index.html point at one that exists. */
router.onError((error) => { reloadIfStaleChunk(error) })

export default router
