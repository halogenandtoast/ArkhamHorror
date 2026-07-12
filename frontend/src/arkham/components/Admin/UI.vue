<script setup lang="ts">
import { computed, nextTick } from 'vue'
import { useRoute, useRouter } from 'vue-router'

const route = useRoute()
const router = useRouter()

const selected = computed(() => route.name === 'Rooms' ? 'rooms' : 'dashboard')

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
    <div class="admin-layout page-content">
      <header class="admin-header">
        <div class="admin-title-block">
          <p class="eyebrow">{{ $t('admin.title') }}</p>
          <Transition name="admin-title-fade" mode="out-in">
            <h1 :key="selected">{{ selected === 'rooms' ? $t('admin.rooms') : $t('admin.dashboard') }}</h1>
          </Transition>
        </div>

        <nav class="admin-nav" :class="selected" :aria-label="$t('admin.title')">
          <a
            class="admin-nav-link"
            :class="{ active: selected === 'dashboard' }"
            :href="router.resolve('/admin').href"
            @click.prevent="navigateTo('/admin')"
          >
            {{ $t('admin.dashboard') }}
          </a>
          <a
            class="admin-nav-link"
            :class="{ active: selected === 'rooms' }"
            :href="router.resolve('/admin/rooms').href"
            @click.prevent="navigateTo('/admin/rooms')"
          >
            {{ $t('admin.rooms') }}
          </a>
        </nav>
      </header>

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
</template>

<style scoped>
.admin-page {
  margin-block-start: 0;
  padding-block: 20px 28px;
  box-sizing: border-box;
  scrollbar-gutter: stable;
}

.admin-layout {
  width: min(1180px, calc(100vw - 32px));
  padding-top: 0;
  padding-bottom: 0;
}

.admin-header {
  display: flex;
  align-items: flex-end;
  justify-content: space-between;
  gap: 16px;
  margin-bottom: 16px;
  padding-bottom: 12px;
  border-bottom: 1px solid var(--box-border);
}

.eyebrow {
  color: var(--spooky-green);
  font-size: 0.75rem;
  font-weight: 700;
  letter-spacing: 0.14em;
  margin: 0 0 2px;
  text-transform: uppercase;
}

.admin-title-block {
  min-width: 14rem;
}

h1 {
  color: var(--title);
  font-family: teutonic, sans-serif;
  font-size: 2.3rem;
  line-height: 1;
  margin: 0;
  text-transform: uppercase;
}

.admin-title-fade-enter-active,
.admin-title-fade-leave-active {
  transition: opacity 130ms ease-in-out;
}

.admin-title-fade-enter-from,
.admin-title-fade-leave-to {
  opacity: 0;
}

.admin-nav {
  --admin-nav-gap: 4px;
  --admin-nav-padding: 4px;
  align-items: center;
  background: var(--background-dark);
  border: 1px solid var(--box-border);
  border-radius: 5px;
  display: grid;
  gap: var(--admin-nav-gap);
  grid-template-columns: repeat(2, minmax(0, 1fr));
  padding: var(--admin-nav-padding);
  position: relative;
}

.admin-nav::before {
  content: '';
  background: var(--spooky-green-dark);
  border-radius: 3px;
  bottom: var(--admin-nav-padding);
  left: var(--admin-nav-padding);
  position: absolute;
  top: var(--admin-nav-padding);
  transform: translateX(0);
  transition: transform 220ms cubic-bezier(.2, .8, .2, 1);
  width: calc((100% - (var(--admin-nav-padding) * 2) - var(--admin-nav-gap)) / 2);
  z-index: 0;
}

.admin-nav.rooms::before {
  transform: translateX(calc(100% + var(--admin-nav-gap)));
}

.admin-nav-link {
  border-radius: 3px;
  color: color-mix(in srgb, var(--spooky-green) 50%, #aaa);
  font-size: 0.85rem;
  font-weight: 700;
  padding: 8px 12px;
  position: relative;
  text-align: center;
  text-decoration: none;
  text-transform: uppercase;
  transition: color 0.15s ease;
  z-index: 1;
}

.admin-nav-link:hover,
.admin-nav-link.active {
  color: white;
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

@media (max-width: 700px) {
  .admin-page {
    padding-block: 12px 20px;
  }

  .admin-layout {
    width: calc(100vw - 24px);
  }

  .admin-header {
    align-items: stretch;
    flex-direction: column;
  }

  h1 {
    font-size: 1.8rem;
  }

  .admin-nav {
    width: fit-content;
  }
}
</style>
