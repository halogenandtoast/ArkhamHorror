import { computed, type ComputedRef } from 'vue'
import { isDevBuild } from '@/arkham/displayRules'
import { useUserStore } from '@/stores/user'

/* Who sees the marketplace: everybody in a dev build, and admins everywhere.
 *
 * It is still unfinished for players, but an admin has to be able to work it in
 * production -- put the first sets up, and look at what they are approving
 * submissions against.
 *
 * Hiding things is all this does. Every endpoint behind it is authorized on its
 * own, so the worst a wrong answer here manages is to show somebody a page that
 * tells them no.
 *
 * Computed rather than read once, because `isAdmin` is false until `whoami`
 * comes back: the nav bar is built before the first route guard has awaited it,
 * and a plain boolean would leave it without the link for the rest of the
 * session. */
export function useMarketplaceVisible(): ComputedRef<boolean> {
  const store = useUserStore()
  return computed(() => isDevBuild() || store.isAdmin)
}
