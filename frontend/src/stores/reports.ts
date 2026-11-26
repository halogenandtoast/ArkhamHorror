import { defineStore } from 'pinia'
import * as Api from '@/arkham/api'

/* The reports this account follows: the ones it filed, and the ones it replied
 * to. One fetch feeds both places they are shown -- the nav badge and the
 * home-page panel -- so the two can never disagree about whether there is news.
 *
 * Polled rather than pushed. The game socket is per-game and the tracker is
 * not, and a reply arrives minutes after it is written at worst, which is well
 * inside how long someone takes to notice anything on a home page.
 */
const POLL_MS = 60_000

export const useReportsStore = defineStore('reports', {
  state: () => ({
    reports: [] as Api.Report[],
    loaded: false,
    timer: null as ReturnType<typeof setInterval> | null,
  }),

  getters: {
    /* Reports with something on them the follower has not seen. */
    unread(state): Api.Report[] {
      return state.reports.filter((report) => report.unread)
    },

    open(state): Api.Report[] {
      return state.reports.filter((report) =>
        (Api.OPEN_REPORT_STATUSES as string[]).includes(report.status),
      )
    },

    /* Archived: finished, and the follower has seen it finished.
     *
     * Derived rather than stored. "Archived" is the conjunction of two facts
     * this list already carries -- the report is in an ending column, and
     * there is nothing on it the viewer has not seen -- so a column, a sweep
     * and a second copy of the truth would all be a way of being wrong about
     * them. It also unarchives itself for free: a reply after the close moves
     * `updatedAt` past the view, and the report is back on the live list.
     *
     * Per viewer, because `unread` is: the same closed report is archived for
     * whoever has read the outcome and live for whoever has not.
     */
    archived(state): Api.Report[] {
      return state.reports.filter(
        (report) =>
          !(Api.OPEN_REPORT_STATUSES as string[]).includes(report.status) && !report.unread,
      )
    },

    /* What is still worth the follower's attention: open work, plus anything
     * finished they have not looked at since. */
    live(state): Api.Report[] {
      return state.reports.filter(
        (report) =>
          (Api.OPEN_REPORT_STATUSES as string[]).includes(report.status) || report.unread,
      )
    },
  },

  actions: {
    async refresh() {
      try {
        this.reports = await Api.fetchMyReports()
        this.loaded = true
      } catch {
        /* Keep what was shown. A tracker badge is not worth an error state, and
         * the next poll tries again. */
      }
    },

    /* Called when a signed-in session starts, and again on sign-out with no
     * user, which clears the badge rather than leaving the last account's. */
    watch(signedIn: boolean) {
      if (this.timer) clearInterval(this.timer)
      this.timer = null
      if (!signedIn) {
        this.reports = []
        this.loaded = false
        return
      }
      void this.refresh()
      this.timer = setInterval(() => void this.refresh(), POLL_MS)
    },
  },
})
