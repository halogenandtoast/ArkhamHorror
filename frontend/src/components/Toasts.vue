<script lang="ts" setup>
/*
 * Renders the queue in `@/toasts`. Mounted once, from App.vue.
 *
 * Teleported to <body> so a toast is never clipped by the board's stacking
 * contexts, and the container itself is pointer-events: none so it does not
 * swallow clicks aimed at the board behind it.
 */
import { useI18n } from 'vue-i18n'
import { toasts, dismissToast } from '@/toasts'

const { t } = useI18n()
</script>

<template>
  <Teleport to="body">
    <!-- One polite live region for the whole stack. Errors here are "deck sync
         failed", not anything safety-critical, so none of them interrupt what a
         screen reader is already saying. -->
    <div class="toasts" aria-live="polite" aria-atomic="false">
      <TransitionGroup name="toast">
        <div
          v-for="toast in toasts"
          :key="toast.id"
          class="toast"
          :class="[`toast--${toast.variant}`, toast.className]"
          @click="dismissToast(toast.id)"
        >
          <component :is="toast.component" v-if="toast.component" v-bind="toast.props" />
          <span v-else class="toast-message">{{ toast.message }}</span>

          <button
            v-if="toast.dismissible"
            class="toast-close"
            :aria-label="t('toasts.dismiss')"
            @click.stop="dismissToast(toast.id)"
          >
            <span aria-hidden="true">&times;</span>
          </button>

          <!-- The bar IS the timer: it dismisses the toast when it finishes, so
               hovering (which pauses the animation) pauses the countdown with no
               second source of truth to keep in step. A backgrounded tab does
               not run animations, so a toast raised there waits to be seen. -->
          <div
            class="toast-progress"
            :style="{ animationDuration: `${toast.timeout}ms` }"
            @animationend="dismissToast(toast.id)"
          />
        </div>
      </TransitionGroup>
    </div>
  </Teleport>
</template>

<style scoped>
.toasts {
  position: fixed;
  /* Below the nav rather than over it. */
  top: calc(var(--nav-height) + 12px);
  right: 12px;
  z-index: var(--z-toasts);
  display: flex;
  flex-direction: column;
  gap: 8px;
  width: min(360px, calc(100vw - 24px));
  pointer-events: none;
}

.toast {
  position: relative;
  overflow: hidden;
  display: flex;
  align-items: flex-start;
  gap: 10px;
  padding: 10px 12px;
  border: 1px solid var(--box-border);
  border-radius: 6px;
  background: var(--box-background);
  box-shadow: 0 6px 20px rgba(0, 0, 0, 0.45);
  color: var(--text);
  font-size: 0.85rem;
  line-height: 1.4;
  cursor: pointer;
  pointer-events: auto;
}

/* Chrome, so neutral by role: the board's magenta/yellow/cyan vocabulary stays
   reserved for the game state itself. */
.toast--success {
  --toast-accent: var(--outcome-won);
}

.toast--error {
  --toast-accent: var(--outcome-lost);
}

.toast--info {
  --toast-accent: var(--mythos);
}

/* The strip reads the variant at a glance. A component toast brings its own
   skin, so it gets no strip -- only the progress bar picks up its accent. */
.toast--success,
.toast--error,
.toast--info {
  /* Clears the strip below. */
  padding-left: 16px;
}

.toast--success::before,
.toast--error::before,
.toast--info::before {
  content: '';
  position: absolute;
  inset: 0 auto 0 0;
  width: 4px;
  background: var(--toast-accent);
}

.toast-message {
  flex: 1;
  min-width: 0;
  overflow-wrap: anywhere;
}

.toast-close {
  flex-shrink: 0;
  margin-left: auto;
  /* base.css gives every button 11px of side padding over border-box, which
     squeezes an icon-only button to nothing. */
  padding: 0;
  width: 18px;
  height: 18px;
  display: grid;
  place-items: center;
  font-size: 14px;
  line-height: 1;
  background: transparent;
  border: 0;
  border-radius: 4px;
  color: rgba(255, 255, 255, 0.55);
}

.toast-close:hover:not(:disabled) {
  background: rgba(255, 255, 255, 0.14);
  border-color: transparent;
  color: var(--text);
}

.toast-progress {
  position: absolute;
  inset: auto 0 0 0;
  height: 3px;
  transform-origin: left;
  background: var(--toast-accent, rgba(255, 255, 255, 0.5));
  animation-name: toast-progress;
  animation-timing-function: linear;
  animation-fill-mode: forwards;
}

/* Hover to read it, focus to reach the close button -- either should stop the
   clock rather than have the toast vanish mid-interaction. */
.toast:hover .toast-progress,
.toast:focus-within .toast-progress {
  animation-play-state: paused;
}

@keyframes toast-progress {
  from {
    transform: scaleX(1);
  }
  to {
    transform: scaleX(0);
  }
}

.toast-enter-active,
.toast-leave-active,
.toast-move {
  transition:
    opacity 180ms ease,
    transform 180ms ease;
}

.toast-enter-from,
.toast-leave-to {
  opacity: 0;
  transform: translateX(16px);
}

/* Taken out of the flow so the ones below slide up over it rather than jumping
   once it is gone. */
.toast-leave-active {
  position: absolute;
  right: 0;
  width: 100%;
}

/* The progress animation is exempt: it is the dismissal timer, not decoration,
   and disabling it would leave every toast on screen forever. */
@media (prefers-reduced-motion: reduce) {
  .toast-enter-from,
  .toast-leave-to {
    transform: none;
  }

  .toast-enter-active,
  .toast-leave-active,
  .toast-move {
    transition-duration: 1ms;
  }
}

@media (max-width: 800px) {
  .toasts {
    left: 12px;
    right: 12px;
    width: auto;
  }
}
</style>
