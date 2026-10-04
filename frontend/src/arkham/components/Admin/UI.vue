<script setup lang="ts">
/* The admin shell: a left rail of sections, the section's own page beside it.
 *
 * A rail rather than a row of tabs because the sections are a list that grows --
 * dashboard, rooms, submissions, whatever comes next -- and a vertical list has
 * room for the next one without squeezing the others. On a narrow screen it
 * collapses behind a toggle, since a sidebar that eats half a phone is not a
 * sidebar.
 */
import { computed, nextTick, ref, watch } from 'vue'
import { useRoute, useRouter } from 'vue-router'

const route = useRoute()
const router = useRouter()

/* The sections, in the order the rail shows them. Data rather than markup so a
 * new one is a list entry rather than another copy of the link markup. */
const sections = [
  { key: 'dashboard', path: '/admin', label: 'admin.dashboard', icon: 'gear' },
  { key: 'rooms', path: '/admin/rooms', label: 'admin.rooms', icon: 'layer-group' },
  { key: 'stats', path: '/admin/stats', label: 'admin.gameStats', icon: 'chart-simple' },
  { key: 'submissions', path: '/admin/submissions', label: 'admin.submissions', icon: 'clipboard-check' },
] as const

const byRouteName: Record<string, string> = {
  Rooms: 'rooms',
  AdminGameStats: 'stats',
  AdminSubmissions: 'submissions',
}

const selected = computed(() => byRouteName[String(route.name)] ?? 'dashboard')

const selectedLabel = computed(
  () => sections.find((s) => s.key === selected.value)?.label ?? 'admin.dashboard',
)

/* Only ever used at narrow widths, where the rail is off-canvas. Closed again on
 * every navigation: the thing you opened it to reach is now on screen behind it. */
const railOpen = ref(false)
watch(() => route.fullPath, () => { railOpen.value = false })

async function navigateTo(path: string) {
  if (router.currentRoute.value.path === path) return

  const documentWithTransition = document as Document & {
    startViewTransition?: (callback: () => Promise<void>) => void
  }

  if (documentWithTransition.startViewTransition) {
    documentWithTransition.startViewTransition(async () => {
      await router.push(path)
      await nextTick()
    })
  } else {
    await router.push(path)
  }
}
</script>

<template>
  <div class="admin-page page-container">
    <div class="admin-layout page-content" :class="{ 'rail-open': railOpen }">
      <aside class="admin-rail" :class="{ open: railOpen }">
        <div class="rail-head">
          <p class="eyebrow">{{ $t('admin.title') }}</p>
        </div>

        <nav class="admin-nav" :aria-label="$t('admin.title')">
          <a
            v-for="section in sections"
            :key="section.key"
            class="admin-nav-link"
            :class="{ active: selected === section.key }"
            :href="router.resolve(section.path).href"
            :aria-current="selected === section.key ? 'page' : undefined"
            @click.prevent="navigateTo(section.path)"
          >
            <font-awesome-icon :icon="section.icon" fixed-width />
            <span>{{ $t(section.label) }}</span>
          </a>
        </nav>
      </aside>

      <!-- Tapping away closes the rail at narrow widths, where it overlays the
           page rather than sitting beside it. -->
      <div v-if="railOpen" class="rail-scrim" @click="railOpen = false"></div>

      <div class="admin-main">
        <header class="admin-header">
          <button
            type="button"
            class="rail-toggle"
            :aria-expanded="railOpen"
            :aria-label="$t('admin.toggleSidebar')"
            @click="railOpen = !railOpen"
          >
            <font-awesome-icon icon="bars" />
          </button>
          <Transition name="admin-title-fade" mode="out-in">
            <h1 :key="selected">{{ $t(selectedLabel) }}</h1>
          </Transition>
        </header>

        <div class="admin-scroll">
          <RouterView v-slot="{ Component }">
            <Transition name="admin-route" mode="out-in">
              <main class="admin-content" :key="route.fullPath">
                <Suspense>
                  <component :is="Component" />
                  <template #fallback>
                    <div class="admin-loading" role="status" aria-live="polite">
                      <div class="loading-header">
                        <span class="loading-title"></span>
                        <span class="loading-count"></span>
                      </div>
                      <div class="loading-line wide"></div>
                      <div class="loading-line"></div>
                      <div class="loading-line short"></div>
                      <span class="sr-only">Loading admin content…</span>
                    </div>
                  </template>
                </Suspense>
              </main>
            </Transition>
          </RouterView>
        </div>
      </div>
    </div>
  </div>
</template>

<style scoped>
/* The page itself does not scroll -- the content column does. That is what keeps
   the rail still: while a section is being swapped there is a moment with no
   content at all, and if the page were the scroller its height would collapse,
   the scroll position would clamp, and everything anchored to it would jump. */
.admin-page {
  box-sizing: border-box;
  margin-block-start: 0;
  overflow: hidden;
  padding-block: 20px 28px;
}

/* Rail then page, both full height. The rail column is a fixed width so the
   content does not change width between sections either. */
.admin-layout {
  display: grid;
  gap: 20px;
  grid-template-columns: 208px minmax(0, 1fr);
  height: 100%;
  min-height: 0;
  padding-top: 0;
  padding-bottom: 0;
  position: relative;
  width: min(1240px, calc(100vw - 32px));
}

/* The rail is the one thing on this page that does not change between sections,
   so it must not look like it does: it is named for the view transition, which
   keeps the browser from folding it into the root snapshot and cross-fading it
   with itself, and it scrolls on its own so a tall page cannot move it. */
.admin-rail {
  align-self: start;
  background: color-mix(in srgb, var(--background-dark) 55%, transparent);
  border: 1px solid var(--box-border);
  border-radius: 6px;
  display: flex;
  flex-direction: column;
  gap: 10px;
  /* Its own scroller, and never taller than the column, so a long list of
     sections cannot push on the layout around it. */
  max-height: 100%;
  overflow-y: auto;
  padding: 12px;
  view-transition-name: admin-rail;
}

.rail-head {
  border-bottom: 1px solid var(--box-border);
  padding-bottom: 8px;
}

.eyebrow {
  color: var(--spooky-green);
  font-size: 0.75rem;
  font-weight: 700;
  letter-spacing: 0.14em;
  margin: 0;
  text-transform: uppercase;
}

.admin-nav {
  display: flex;
  flex-direction: column;
  gap: 2px;
}

.admin-nav-link {
  align-items: center;
  border-radius: 4px;
  color: color-mix(in srgb, var(--spooky-green) 50%, #aaa);
  display: flex;
  font-size: 0.85rem;
  font-weight: 700;
  gap: 10px;
  padding: 9px 10px;
  text-decoration: none;
  text-transform: uppercase;
  transition: color 0.15s ease, background 0.15s ease;
}

.admin-nav-link:hover {
  background: rgba(255, 255, 255, 0.06);
  color: white;
}

/* The selected section, marked on the edge nearest the content it is showing. */
.admin-nav-link.active {
  background: var(--spooky-green-dark);
  box-shadow: inset 3px 0 0 var(--spooky-green);
  color: white;
}

.admin-main {
  display: flex;
  flex-direction: column;
  gap: 16px;
  min-height: 0;
  min-width: 0;
}

/* The only thing on the page that scrolls. `scrollbar-gutter` keeps the column
   the same width whether or not the section is long enough to need a bar. */
.admin-scroll {
  flex: 1;
  min-height: 0;
  overflow-x: hidden;
  overflow-y: auto;
  scrollbar-gutter: stable;
}

.admin-header {
  align-items: center;
  border-bottom: 1px solid var(--box-border);
  display: flex;
  flex: none;
  gap: 12px;
  min-height: 44px;
  padding-bottom: 12px;
}

h1 {
  color: var(--title);
  font-family: teutonic, sans-serif;
  font-size: 2.3rem;
  line-height: 1;
  margin: 0;
  text-transform: uppercase;
}

/* Only shown where the rail is off-canvas. */
.rail-toggle {
  background: var(--background-dark);
  border: 1px solid var(--box-border);
  border-radius: 4px;
  color: var(--title);
  cursor: pointer;
  display: none;
  flex: none;
  padding: 8px 11px;
}

.rail-toggle:hover {
  border-color: var(--spooky-green);
}

.rail-scrim {
  display: none;
}

.admin-title-fade-enter-active,
.admin-title-fade-leave-active {
  transition: opacity 130ms ease-in-out;
}

.admin-title-fade-enter-from,
.admin-title-fade-leave-to {
  opacity: 0;
}

.admin-content {
  display: flex;
  flex-direction: column;
  gap: 16px;
  view-transition-name: admin-content;
}

.admin-loading {
  background: color-mix(in srgb, var(--background-dark) 42%, transparent);
  border: 1px solid color-mix(in srgb, var(--box-border) 75%, transparent);
  border-radius: 6px;
  display: flex;
  flex-direction: column;
  gap: 12px;
  min-height: 160px;
  padding: 14px;
}

.loading-header {
  align-items: center;
  display: flex;
  gap: 12px;
}

.loading-title,
.loading-count,
.loading-line {
  animation: admin-loading-pulse 1.1s ease-in-out infinite alternate;
  background: rgba(255, 255, 255, 0.08);
  border-radius: 3px;
}

.loading-title {
  flex: 1;
  height: 22px;
  max-width: 260px;
}

.loading-count {
  height: 24px;
  width: 42px;
}

.loading-line {
  height: 48px;
  width: 100%;
}

.loading-line.wide {
  height: 72px;
}

.loading-line.short {
  width: 68%;
}

.sr-only {
  height: 1px;
  margin: -1px;
  overflow: hidden;
  position: absolute;
  width: 1px;
  clip: rect(0, 0, 0, 0);
}

.admin-route-enter-active,
.admin-route-leave-active {
  transition: opacity 180ms cubic-bezier(.2, .8, .2, 1), transform 180ms cubic-bezier(.2, .8, .2, 1);
}

.admin-route-enter-from {
  opacity: 0;
  transform: translateY(8px);
}

.admin-route-leave-to {
  opacity: 0;
  transform: translateY(6px);
}

:global(::view-transition-old(root)),
:global(::view-transition-new(root)) {
  animation: none;
}

:global(::view-transition-group(admin-content)) {
  animation-duration: 220ms;
  animation-timing-function: cubic-bezier(.2, .8, .2, 1);
}

/* Held still. The rail is identical on both sides of the transition, so anything
   animating it is a flicker with nothing to show for it. */
:global(::view-transition-group(admin-rail)),
:global(::view-transition-old(admin-rail)),
:global(::view-transition-new(admin-rail)) {
  animation: none;
}

:global(::view-transition-old(admin-rail)) {
  display: none;
}

:global(::view-transition-old(admin-content)) {
  animation: admin-content-out 180ms cubic-bezier(.4, 0, 1, 1) both;
}

:global(::view-transition-new(admin-content)) {
  animation: admin-content-in 220ms cubic-bezier(.2, .8, .2, 1) both;
}

:global(::view-transition-old(admin-content)),
:global(::view-transition-new(admin-content)) {
  mix-blend-mode: normal;
}

@keyframes admin-content-out {
  from {
    opacity: 1;
    transform: translateY(0);
  }

  to {
    opacity: 0;
    transform: translateY(6px);
  }
}

@keyframes admin-content-in {
  from {
    opacity: 0;
    transform: translateY(8px);
  }

  to {
    opacity: 1;
    transform: translateY(0);
  }
}

@keyframes admin-loading-pulse {
  from {
    opacity: 0.45;
  }

  to {
    opacity: 0.9;
  }
}

/* Narrow: the rail comes off the grid and slides in over the page, which is the
   only way a 208px column and a readable page both fit. */
@media (max-width: 860px) {
  .admin-page {
    padding-block: 12px 20px;
  }

  .admin-layout {
    grid-template-columns: minmax(0, 1fr);
    width: calc(100vw - 24px);
  }

  /* Off-canvas, so it is no longer a column of the grid and the content column
     is the whole width. */
  .admin-scroll {
    scrollbar-gutter: auto;
  }

  .admin-rail {
    bottom: 0;
    border-radius: 0 6px 6px 0;
    border-left: none;
    background: var(--background-dark);
    left: 0;
    max-height: none;
    position: fixed;
    top: 0;
    transform: translateX(-100%);
    transition: transform 200ms cubic-bezier(.2, .8, .2, 1);
    width: 220px;
    z-index: 30;
    /* Off-canvas and transformed: naming it here would hand the browser a
       moving target to hold still. */
    view-transition-name: none;
  }

  .admin-rail.open {
    transform: translateX(0);
  }

  .rail-scrim {
    background: rgba(0, 0, 0, 0.55);
    display: block;
    inset: 0;
    position: fixed;
    z-index: 29;
  }

  .rail-toggle {
    display: block;
  }

  h1 {
    font-size: 1.8rem;
  }
}

@media (prefers-reduced-motion: reduce) {
  .admin-rail {
    transition: none;
  }
}
</style>
