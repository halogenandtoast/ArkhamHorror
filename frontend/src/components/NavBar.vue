<script lang="ts" setup>
import { computed, ref } from 'vue'
import { useUserStore } from '@/stores/user'
import { useRouter } from 'vue-router'
import type { User } from '@/types'
import { OnClickOutside } from '@vueuse/components'

const expanded = ref(false);
const mobileOpen = ref(false);
const router = useRouter()
const store = useUserStore()
const currentUser = computed<User | null>(() => store.currentUser)

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
      <router-link to="/about">{{$t('nav.about')}}</router-link>
      <router-link to="/about?support">{{$t('nav.support')}}</router-link>
      <router-link v-if="currentUser && currentUser.admin" to="/admin">{{$t('nav.admin')}}</router-link>
    </div>
  </header>
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
  z-index: 100;
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
