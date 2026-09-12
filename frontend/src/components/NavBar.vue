<script lang="ts" setup>
import { computed, ref } from 'vue'
import { useUserStore } from '@/stores/user'
import { useRoute, useRouter } from 'vue-router'
import type { User } from '@/types'
import { OnClickOutside } from '@vueuse/components'
import { storeToRefs } from 'pinia'
import { useSettings } from '@/stores/settings'
import { isDevBuild } from '@/arkham/displayRules'

const expanded = ref(false);
const mobileOpen = ref(false);
const route = useRoute()
const router = useRouter()
const store = useUserStore()
const currentUser = computed<User | null>(() => store.currentUser)
const { customCardsEnabled } = storeToRefs(useSettings())

/* The pages that are halves of one section get a bar of their own under the main
 * one, rather than each claiming a top-level entry. Rendered here so it sits with
 * the menu it belongs to instead of inside whichever page is open. */
/* `covers` is every route the entry stands for, so a page deeper in -- one
 * published set -- still lights up the half it belongs to. `router-link-active`
 * cannot do this: it follows matched route records, and these are siblings. */
const CUSTOM_CARDS_SUBMENU = [
  { name: 'CardBuilder', label: 'customCardSets.mySets', icon: 'layer-group', covers: ['CardBuilder'] },
  {
    name: 'CardMarketplace',
    label: 'customCardSets.marketplace',
    icon: 'store',
    covers: ['CardMarketplace', 'CardMarketplaceSet'],
  },
]

/* The marketplace is its own top-level route rather than a child of the builder's,
 * so `router-link-active` does not reach it: that class follows matched route
 * records, not path prefixes. The section is named here instead, and the main
 * link is marked active from it. */
const routeName = computed(() => String(route.name ?? ''))

const inCustomCards = computed(() =>
  CUSTOM_CARDS_SUBMENU.some((entry) => entry.covers.includes(routeName.value)),
)

/* `customCardsEnabled` is a setting anyone can turn on, but the marketplace is
 * dev-only for now, so the bar needs both. */
const submenu = computed(() =>
  inCustomCards.value && customCardsEnabled.value && isDevBuild() ? CUSTOM_CARDS_SUBMENU : null,
)

async function logout() {
  await store.logout()
  router.push({ path: '/' })
}
</script>

<template>
  <header id="nav">
    <button v-if="currentUser" class="mobile-menu-btn" @click="mobileOpen = !mobileOpen">
      <font-awesome-icon icon="bars" />
    </button>

    <nav class="main-links">
      <router-link to="/" class="home-link">{{$t('nav.home')}}</router-link>
      <router-link v-if="currentUser" to="/decks" class="nav-link">{{$t('nav.myDecks')}}</router-link>
      <router-link v-if="currentUser" to="/cards" class="nav-link">{{$t('nav.cards')}}</router-link>
      <router-link v-if="currentUser" to="/achievements" class="nav-link">{{$t('nav.achievements')}}</router-link>
      <router-link
        v-if="currentUser && customCardsEnabled"
        to="/card-builder"
        class="nav-link"
        :class="{ 'router-link-active': inCustomCards }"
      >{{$t('nav.cardBuilder')}}</router-link>
      <router-link v-if="currentUser" to="/about" class="nav-link">{{$t('nav.about')}}</router-link>
      <router-link v-if="currentUser" to="/about?support" class="nav-link">{{$t('nav.support')}}</router-link>
      <router-link v-if="currentUser && currentUser.admin" to="/admin" class="nav-link">{{$t('nav.admin')}}</router-link>
    </nav>

    <OnClickOutside @trigger="expanded = false">
      <div class="user-links">
        <template v-if="currentUser">
          <button class="user-btn" :class="{ open: expanded }" @click="expanded = !expanded">
            <span>{{currentUser.username}}</span>
            <font-awesome-icon icon="angle-down" class="dropdown-icon" :class="{ open: expanded }" />
          </button>
          <div v-if="expanded" class="user-dropdown">
            <router-link @click="expanded = false" to="/settings">{{$t('settings')}}</router-link>
            <a href="#" @click="logout">Logout</a>
          </div>
        </template>
        <template v-else>
          <router-link to="/sign-in">Login</router-link>
          <router-link to="/sign-up">Register</router-link>
        </template>
      </div>
    </OnClickOutside>

    <div v-if="mobileOpen" class="mobile-menu" @click="mobileOpen = false">
      <router-link to="/decks">{{$t('nav.myDecks')}}</router-link>
      <router-link to="/cards">{{$t('nav.cards')}}</router-link>
      <router-link to="/achievements">{{$t('nav.achievements')}}</router-link>
      <router-link v-if="customCardsEnabled" to="/card-builder">{{$t('nav.cardBuilder')}}</router-link>
      <router-link to="/about">{{$t('nav.about')}}</router-link>
      <router-link to="/about?support">{{$t('nav.support')}}</router-link>
      <router-link v-if="currentUser && currentUser.admin" to="/admin">{{$t('nav.admin')}}</router-link>
    </div>
  </header>

  <nav v-if="submenu" id="subnav">
    <router-link
      v-for="entry in submenu"
      :key="entry.name"
      :to="{ name: entry.name }"
      class="sub-link"
      :class="{ on: entry.covers.includes(routeName) }"
    >
      <font-awesome-icon :icon="entry.icon" />
      {{ $t(entry.label) }}
    </router-link>
  </nav>
</template>

<style scoped>
#nav {
  background: var(--background-dark);
  border-bottom: 1px solid rgba(255,255,255,0.07);
  box-shadow: 0 1px 12px rgba(0,0,0,0.5);
  color: #f2f2f2;
  display: flex;
  align-items: stretch;
  height: var(--nav-height);
  flex-shrink: 0;
  position: relative;
  z-index: var(--z-index-100);
}

/* ── Sub menu ───────────────────────────────────────────── */

/* A quieter second bar, ink rather than the main bar's green, so it reads as
   somewhere inside the section rather than more top-level places to go. */
#subnav {
  align-items: stretch;
  background: color-mix(in srgb, var(--background-dark) 70%, black 30%);
  border-bottom: 1px solid rgba(255, 255, 255, 0.07);
  display: flex;
  flex-shrink: 0;
  padding: 0 6px;
  position: relative;
  z-index: var(--z-index-100);
}

.sub-link {
  align-items: center;
  color: #8e94a3;
  display: flex;
  font-size: 0.8rem;
  gap: 0.4rem;
  padding: 6px 12px;
  position: relative;
  text-decoration: none;
  transition: color 0.15s;
  white-space: nowrap;

  &:hover { color: #c4b5fd; }

  /* Marked from the route name rather than the link's own class, so a set's page
     still lights up Marketplace. */
  &.on {
    color: #c4b5fd;

    &::after {
      background: #c4b5fd;
      border-radius: 2px 2px 0 0;
      bottom: 0;
      content: '';
      height: 2px;
      left: 8px;
      position: absolute;
      right: 8px;
    }
  }
}

/* ── Main nav links ─────────────────────────────────────── */

.main-links {
  display: flex;
  align-items: stretch;
  flex: 1;
}

.home-link,
.nav-link {
  display: flex;
  align-items: center;
  padding: 0 14px;
  color: color-mix(in srgb, var(--spooky-green) 45%, #666);
  font-size: 0.875rem;
  font-weight: 500;
  text-decoration: none;
  position: relative;
  transition: color 0.15s;
  white-space: nowrap;

  &:hover { color: var(--spooky-green); }

  &.router-link-active {
    color: var(--spooky-green);
    &::after {
      content: '';
      position: absolute;
      bottom: 0;
      left: 8px;
      right: 8px;
      height: 2px;
      background: var(--spooky-green);
      border-radius: 2px 2px 0 0;
    }
  }
}

.home-link {
  font-weight: 700;
  color: color-mix(in srgb, var(--spooky-green) 70%, #ccc);
  font-size: 0.925rem;

  &:hover { color: color-mix(in srgb, var(--spooky-green) 85%, white); }

  &.router-link-active {
    color: color-mix(in srgb, var(--spooky-green) 85%, white);
    &::after { display: none; }
  }
}

@media (max-width: 768px) {
  .nav-link { display: none; }
}

/* ── Mobile hamburger ───────────────────────────────────── */

.mobile-menu-btn {
  display: none;
  align-items: center;
  justify-content: center;
  width: 44px;
  height: 100%;
  background: transparent;
  border: none;
  color: color-mix(in srgb, var(--spooky-green) 45%, #666);
  font-size: 1rem;
  cursor: pointer;
  transition: color 0.15s;

  &:hover { color: var(--spooky-green); }

  @media (max-width: 768px) {
    display: flex;
  }
}

.mobile-menu {
  position: absolute;
  top: 100%;
  left: 0;
  right: 0;
  background: var(--background-dark);
  border-bottom: 1px solid rgba(255,255,255,0.08);
  box-shadow: 0 8px 20px rgba(0,0,0,0.5);
  display: flex;
  flex-direction: column;

  a {
    padding: 14px 20px;
    color: color-mix(in srgb, var(--spooky-green) 45%, #666);
    font-size: 0.9rem;
    font-weight: 500;
    text-decoration: none;
    border-bottom: 1px solid rgba(255,255,255,0.05);
    transition: background 0.12s, color 0.12s;

    &:last-child { border-bottom: none; }
    &:hover { background: rgba(255,255,255,0.04); color: var(--spooky-green); }
    &.router-link-active { color: var(--spooky-green); }
  }
}

/* ── User section ──────────────────────────────────────── */

.user-links {
  display: flex;
  align-items: center;
  height: 100%;
  gap: 10px;
  padding: 0 16px;
  position: relative;

  a {
    color: color-mix(in srgb, var(--spooky-green) 45%, #666);
    font-size: 0.875rem;
    font-weight: 500;
    text-decoration: none;
    transition: color 0.15s;
    white-space: nowrap;
    &:hover { color: var(--spooky-green); }
  }

  @media (max-width: 768px) {
    padding: 0 10px;
    gap: 6px;
  }
}

.user-btn {
  display: flex;
  align-items: center;
  gap: 7px;
  background: transparent;
  border: none;
  color: #fff;
  font-size: 0.875rem;
  font-weight: 500;
  cursor: pointer;
  padding: 0;
  white-space: nowrap;
  transition: color 0.15s;

  &:hover { color: #fff; }
  &.open { color: #fff; }
}

.dropdown-icon {
  font-size: 0.75em;
  transition: transform 0.2s;
  &.open { transform: rotate(180deg); }
}

.user-dropdown {
  position: absolute;
  top: calc(100% + 6px);
  right: 0;
  min-width: 160px;
  background: var(--background-dark);
  border: 1px solid rgba(255,255,255,0.12);
  border-radius: 8px;
  box-shadow: 0 8px 24px rgba(0,0,0,0.6);
  overflow: hidden;

  a {
    display: block;
    padding: 12px 16px;
    color: color-mix(in srgb, var(--spooky-green) 45%, #666);
    font-size: 0.875rem;
    text-decoration: none;
    border-bottom: 1px solid rgba(255,255,255,0.05);
    transition: background 0.12s, color 0.12s;

    &:last-child { border-bottom: none; }
    &:hover { background: rgba(255,255,255,0.04); color: var(--spooky-green); }
  }
}
</style>
