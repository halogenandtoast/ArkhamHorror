<script lang="ts" setup>
/* What a shared prompt leaves blank, filled in.
 *
 * `Add {name} to hand` is a sentence with a hole in it, and the hole has a
 * name: the label carries `name=s:"…"` beside the key. Written out that is a
 * string with quoting rules in it, so it is edited as what it is -- a list of
 * names and values -- and each value can be a binding this step has in scope
 * rather than something typed twice.
 */
import { computed, onMounted, ref } from 'vue'
import { useI18n } from 'vue-i18n'
import {
  formatSharedLabel,
  isBindingValue,
  parseSharedLabel,
  readableParams,
  sharedTextCatalogue,
} from '@/arkham/customText'
import type { Binding } from '@/arkham/customCardBindings'

const props = defineProps<{ label: string; bindings?: Binding[] }>()
const emit = defineEmits<{ 'update:label': [value: string]; close: [] }>()

const box = ref<HTMLDialogElement | null>(null)
onMounted(() => box.value?.showModal())

const { t, te } = useI18n()

const held = computed(() => parseSharedLabel(props.label))

/* The sentence as it will read, rather than as it is written. A message with
 * branches -- "Add 1 token | Add {count} tokens" -- picks one of them by the
 * number in `count`, the same way `countVar` means it to, so the only way to
 * see which wording a value produces is to resolve it. */
const reads = computed(() => {
  const { key, params } = held.value
  return te(key) ? t(key, readableParams(params)) : ''
})

/* A binding is only known while the game is running, so the sentence above is
 * as far as the builder can read it. */
const pending = computed(() => Object.values(held.value.params).some(isBindingValue))

/* Said once, where a number is doing more than counting. */
const plural = computed(() => entry.value?.plural ?? false)

const entry = computed(() => sharedTextCatalogue().find((e) => e.key === held.value.key))

/* The blanks the prompt declares, then anything the label carries that it does
 * not -- a leftover from an earlier key is still worth seeing. */
const names = computed(() => {
  const declared = entry.value?.params ?? []
  const extra = Object.keys(held.value.params).filter((name) => !declared.includes(name))
  return [...declared, ...extra]
})

const openMenu = ref<string | null>(null)

/* Which blanks travel as numbers. A message with branches counts on one, and a
 * binding standing in for it has to arrive as a number too or the wording is
 * chosen wrong -- so a binding the editor knows holds an `Int` is written as
 * one. */
const numeric = computed(() => {
  const counts = entry.value?.plural ? ['count', 'n'] : []
  const byType = Object.entries(held.value.params)
    .filter(([, value]) => isBindingValue(value))
    .filter(([, value]) => props.bindings?.find((b) => `$${b.name}` === value)?.type === 'Int')
    .map(([name]) => name)
  return [...new Set([...counts, ...byType])]
})

function set(name: string, value: string) {
  emit(
    'update:label',
    formatSharedLabel(held.value.key, { ...held.value.params, [name]: value }, numeric.value),
  )
}

function take(name: string, bound: string) {
  openMenu.value = null
  set(name, `$${bound}`)
}
</script>

<template>
  <!-- Taken out of the field it was opened from: a dialog that stays there is
       only ever as visible as its surroundings, and a field can sit inside
       something closed. -->
  <Teleport to="body">
    <dialog ref="box" class="values" @close="emit('close')" @click.self="emit('close')">
      <section @keydown.esc="emit('close')">
        <header>
          <h2>Values</h2>
          <code class="key">${{ held.key }}</code>
        </header>

        <p v-if="entry" class="says reads">{{ reads }}</p>
        <p v-else class="says missing">Nothing in the app says this.</p>
        <p v-if="pending" class="says note">
          A value in scope is filled in while the game runs, so it stands as its own name here.
        </p>
        <p v-if="plural" class="says note">
          Written in more than one wording; the number in <code>count</code> picks which.
        </p>

        <ul v-if="names.length">
          <li v-for="name in names" :key="name">
            <code class="name">{{ name }}</code>

            <!-- One control: the value, and the names in scope that could be it. -->
            <div class="value-wrap">
              <div class="field">
                <input
                  class="value-part"
                  :value="held.params[name] ?? ''"
                  :placeholder="`what ${name} is`"
                  @input="set(name, ($event.target as HTMLInputElement).value)"
                  @keydown.stop
                />
                <button
                  type="button"
                  class="segment"
                  :class="{ on: openMenu === name }"
                  :disabled="!bindings?.length"
                  v-tooltip="bindings?.length ? 'Use a name in scope' : 'Nothing is in scope here'"
                  :aria-label="`Use a name in scope for ${name}`"
                  @click.prevent="openMenu = openMenu === name ? null : name"
                >
                  $
                </button>
              </div>

              <!-- Outside the box, which clips its own corners. -->
              <ul v-if="openMenu === name" class="menu">
                <li v-for="bound in bindings ?? []" :key="bound.name">
                  <button type="button" @click.prevent="take(name, bound.name)">
                    <code>${{ bound.name }}</code>
                    <span v-if="bound.type" class="type">:: {{ bound.type }}</span>
                  </button>
                </li>
              </ul>
            </div>
          </li>
        </ul>
        <p v-else class="says">This prompt leaves nothing blank.</p>

        <div class="actions">
          <button type="button" @click="emit('close')">Done</button>
        </div>
      </section>
    </dialog>
  </Teleport>
</template>

<style scoped lang="scss">
.values {
  background: var(--background-dark);
  border: 1px solid #f0abfc66;
  border-radius: 8px;
  color: var(--title);
  inset: 0;
  margin: auto;
  max-height: min(80vh, 36rem);
  max-width: 100%;
  padding: 0;
  width: min(34rem, 92vw);

  &::backdrop {
    background: rgba(0, 0, 0, 0.6);
  }

  > section {
    display: flex;
    flex-direction: column;
    gap: 0.6rem;
    padding: 0.9rem 1rem;
  }

  header {
    align-items: baseline;
    display: flex;
    gap: 0.6rem;
  }

  h2 {
    color: #f0abfc;
    font-family: teutonic, sans-serif;
    font-size: 1.2em;
    margin: 0;
  }

  ul {
    list-style: none;
    margin: 0;
    padding: 0;
  }

  > section > ul > li {
    align-items: center;
    display: flex;
    gap: 0.6rem;
    padding: 0.2rem 0;
  }
}

.key {
  color: #f0abfc;
  font-size: 0.75rem;
}

.says {
  font-size: 0.82rem;
  margin: 0;
  opacity: 0.8;

  &.missing {
    color: #fca5a5;
  }

  &.reads {
    color: #eee;
    opacity: 1;
  }

  &.note {
    font-size: 0.75rem;

    code {
      color: #f0abfc;
    }
  }
}

.name {
  color: #f0abfc;
  flex: none;
  font-size: 0.78rem;
  min-width: 5rem;
}

.value-wrap {
  flex: 1;
  min-width: 0;
  position: relative;
}

/* The value and the names it could be, as one control -- the same box the
   label field is built from, so a value here looks like a value there. The
   height comes from `--control-h`, which is why nothing in it carries vertical
   padding of its own: the parts stretch, and the text centres itself. */
.field {
  align-items: stretch;
  background: var(--background-dark);
  border: 1px solid var(--box-border);
  border-radius: 4px;
  display: flex;
  min-width: 0;
  overflow: hidden;

  &:focus-within {
    border-color: var(--spooky-green);
  }
}

.value-part {
  background: none;
  border: none;
  border-radius: 0;
  color: #eee;
  flex: 1 1 auto;
  font-family: monospace;
  min-height: 0;
  min-width: 0;
  padding: 0 0.6rem;

  &:focus {
    outline: none;
  }

  &::placeholder {
    color: #6b7280;
  }
}

/* A segment of the box, not a button parked next to it: square inside, divided
   by a hairline, and the builder's binding colour because that is what it
   offers. */
.segment {
  background: rgba(170, 221, 255, 0.12);
  border: none;
  border-left: 1px solid var(--box-border);
  border-radius: 0;
  color: #adf;
  flex: none;
  font-family: monospace;
  font-size: 0.82rem;
  padding: 0 0.7rem;

  &:hover:not(:disabled),
  &.on {
    background: rgba(170, 221, 255, 0.26);
  }

  &:disabled {
    background: none;
    color: #4b5563;
  }
}

.menu {
  background: #0b1220;
  border: 1px solid #374151;
  border-radius: 5px;
  box-shadow: 0 8px 20px rgba(0, 0, 0, 0.45);
  max-height: 12rem;
  overflow-y: auto;
  padding: 0.2rem;
  position: absolute;
  right: 0;
  top: 100%;
  width: max-content;
  z-index: 1;

  button {
    background: none;
    border: none;
    display: flex;
    gap: 0.5rem;
    padding: 0.25rem 0.4rem;
    text-align: left;
    width: 100%;

    &:hover {
      background: rgba(170, 221, 255, 0.12);
      border-radius: 4px;
    }
  }

  code {
    color: #5eead4;
    font-size: 0.78rem;
  }

  .type {
    color: #9ca3af;
    font-size: 0.72rem;
  }
}

/* A row of its own at the foot, where a dialog's way out belongs. */
.actions {
  display: flex;
  justify-content: flex-end;
  margin-top: 0.2rem;
}
</style>
