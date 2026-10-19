/*
 * The app's transient-notification queue, replacing vue-toastification.
 *
 * Only what the app actually asks for: three message variants, a toast that
 * renders a component instead of a string, a per-call timeout and stacking.
 * There is deliberately no persistent toast -- a toast dismisses itself when
 * its progress bar finishes animating (see Toasts.vue), so a toast with no
 * timeout would never leave the screen.
 *
 * The queue is module state rather than a Pinia store because toasts are
 * raised from outside setup() too -- the game's websocket handler is the main
 * one -- and a module singleton needs no component instance to reach.
 */
import { markRaw, shallowReactive, type Component } from 'vue'

export type ToastVariant = 'success' | 'error' | 'info' | 'custom'

export interface ToastOptions {
  /** How long the toast stays up, in milliseconds. */
  timeout?: number
}

export interface CustomToastOptions extends ToastOptions {
  /** Extra class on the toast root, so a component toast can reskin it. */
  className?: string
  /** Draw the close button. Off for a toast that is purely informational. */
  dismissible?: boolean
}

export interface ToastEntry {
  id: number
  variant: ToastVariant
  timeout: number
  dismissible: boolean
  message?: string
  component?: Component
  props?: Record<string, unknown>
  className?: string
}

/* vue-toastification's default, kept because several callers relied on it. */
const DEFAULT_TIMEOUT = 5000
/* Below this a toast is gone before it can be read. */
const MIN_TIMEOUT = 1000
/* A burst -- several achievements unlocking at once -- should not fill the
 * viewport. The oldest go, since the newest is what just happened. */
const MAX_TOASTS = 8

/* Shallow: an entry never changes after it is queued, and the component on it
 * must not be walked by the reactivity proxy. */
export const toasts = shallowReactive<ToastEntry[]>([])

let nextId = 0

function resolveTimeout(options?: ToastOptions): number {
  return Math.max(MIN_TIMEOUT, options?.timeout ?? DEFAULT_TIMEOUT)
}

function push(entry: Omit<ToastEntry, 'id'>): number {
  const id = ++nextId
  toasts.push({ ...entry, id })
  if (toasts.length > MAX_TOASTS) toasts.splice(0, toasts.length - MAX_TOASTS)
  return id
}

export function dismissToast(id: number): void {
  const index = toasts.findIndex((toast) => toast.id === id)
  if (index !== -1) toasts.splice(index, 1)
}

function message(variant: ToastVariant) {
  return (text: string, options?: ToastOptions) =>
    push({ variant, message: text, timeout: resolveTimeout(options), dismissible: true })
}

const api = {
  success: message('success'),
  error: message('error'),
  info: message('info'),

  /* A toast whose body is a component. It renders inside the main app, so it
   * has the app's i18n and global components available. */
  custom: (
    component: Component,
    props: Record<string, unknown> = {},
    options: CustomToastOptions = {},
  ) =>
    push({
      variant: 'custom',
      component: markRaw(component),
      props,
      timeout: resolveTimeout(options),
      className: options.className,
      dismissible: options.dismissible ?? true,
    }),

  dismiss: dismissToast,
}

export type ToastApi = typeof api

export function useToast(): ToastApi {
  return api
}
