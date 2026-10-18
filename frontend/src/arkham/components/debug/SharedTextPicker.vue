<script lang="ts" setup>
/* The words the app already has, searched by what they say.
 *
 * A prompt written once in `label.json` is said by a hundred printed cards and
 * carries every language it has been translated into, which is reason enough
 * for a custom card to ask for one by name. There are a few hundred of them,
 * so they are searched rather than listed, and searched by the sentence --
 * nobody knows the key for "Add {name} to hand" until they have found it once.
 */
import { computed, onMounted, ref } from 'vue'
import { vFocus } from '@/arkham/components/debug/vFocus'
import { formatContent } from '@/arkham/helpers'
import { sharedTextCatalogue, type SharedText } from '@/arkham/customText'

const props = defineProps<{ chosen?: string | null }>()
const emit = defineEmits<{ pick: [key: string]; close: [] }>()

const search = ref('')

/* A real dialog rather than a div with a high number on it.
 *
 * This opens from a field that is already inside a panel inside a scroller,
 * each with a stacking context of its own; `showModal` puts it in the browser's
 * top layer, which is above every one of them whatever they are numbered, and
 * brings Esc and a backdrop with it. */
const box = ref<HTMLDialogElement | null>(null)
onMounted(() => box.value?.showModal())
const catalogue = computed<SharedText[]>(() => sharedTextCatalogue())

/* Searched the way it is read, not the way it is stored: nobody looking for
 * "curse token" types the literal braces the loader wrapped the icon in. */
const plain = (text: string) => text.replace(/\{'\{'\}/g, '{').replace(/\{'\}'\}/g, '}')

/* The braces come out of the haystack as well: "add 1 curse token" is how
 * somebody looks for "Add 1 {curse} token", and the marks around the icon are
 * not something they are going to type. */
const haystack = (text: string) => plain(text).replace(/[{}]/g, '').toLowerCase()

const matching = computed(() => {
  const needle = search.value.trim().toLowerCase()
  if (!needle) return catalogue.value.slice(0, 60)
  return catalogue.value
    .filter((e) => haystack(e.text).includes(needle) || e.key.toLowerCase().includes(needle))
    .slice(0, 60)
})

/* As the prompt reads, not as it is stored. vue-i18n's literal brace -- the
 * form the loader rewrites `{skull}` into so it survives compilation -- is
 * noise to anyone choosing a sentence, and the icons it protects are the point
 * of writing it that way. */
const asRead = (text: string) => formatContent(plain(text))

const take = (key?: string) => {
  const picked = key ?? matching.value[0]?.key
  if (picked) emit('pick', picked)
}
</script>

<template>
  <!-- Taken out of the field it was opened from: a dialog that stays there is
       only ever as visible as its surroundings, and a field can sit inside
       something closed. -->
  <Teleport to="body">
    <dialog ref="box" class="shared-picker" @close="emit('close')" @click.self="emit('close')">
      <section @keydown.esc="emit('close')">
        <header>
          <h2>Shared text</h2>
          <span class="count">{{ catalogue.length }} prompts</span>
          <span class="spacer" />
          <button type="button" class="link" @click="emit('close')">Close</button>
        </header>

        <input
          v-model="search"
          type="search"
          placeholder="Search by what it says — “add to hand”, “heal”, “choose”"
          v-focus
          @keydown.stop
          @keydown.enter.prevent="take()"
          @keydown.esc="emit('close')"
        />

        <ul>
          <li v-for="entry in matching" :key="entry.key" :class="{ on: `$${entry.key}` === chosen }">
            <button type="button" @click="take(entry.key)">
              <span class="says" v-html="asRead(entry.text)" />
              <code class="key">${{ entry.key }}</code>
              <span v-if="entry.params.length" class="blanks">
                <code v-for="blank in entry.params" :key="blank">{{ blank }}</code>
              </span>
            </button>
          </li>
          <li v-if="!matching.length" class="none">Nothing says that</li>
        </ul>
      </section>
    </dialog>
  </Teleport>
</template>

<style scoped lang="scss">
/* Sized to the window it opens over: wide enough for a sentence and its key on
   one line, short enough that the list scrolls rather than the page. */
.shared-picker {
  background: var(--background-dark);
  border: 1px solid #f0abfc66;
  border-radius: 8px;
  color: var(--title);
  height: min(80vh, 44rem);
  /* Centred in the window rather than wherever the top layer would have put
     it: `inset: 0` with auto margins is what centres a dialog in both axes. */
  inset: 0;
  margin: auto;
  max-height: 100%;
  max-width: 100%;
  padding: 0;
  width: min(46rem, 92vw);

  &::backdrop {
    background: rgba(0, 0, 0, 0.6);
  }

  > section {
    display: flex;
    flex-direction: column;
    gap: 0.6rem;
    height: 100%;
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
    min-height: 0;
    overflow-y: auto;
    padding: 0;
  }
}

.count {
  font-size: 0.75rem;
  opacity: 0.6;
}

.spacer {
  flex: 1;
}

li button {
  align-items: baseline;
  background: none;
  border: none;
  color: inherit;
  display: flex;
  gap: 0.6rem;
  padding: 0.35rem 0.45rem;
  text-align: left;
  width: 100%;

  &:hover {
    background: rgba(240, 171, 252, 0.12);
    border-radius: 4px;
  }
}

li.on button {
  background: rgba(240, 171, 252, 0.18);
  border-radius: 4px;
}

.says {
  flex: 1;
  min-width: 0;
}

.key {
  color: #f0abfc;
  flex: none;
  font-size: 0.72rem;
}

/* What the sentence leaves blank, so a prompt that needs filling in says so
   before it is picked rather than after. */
.blanks {
  display: flex;
  flex: none;
  gap: 0.25rem;

  code {
    background: rgba(240, 171, 252, 0.14);
    border-radius: 3px;
    color: #f0abfc;
    font-size: 0.68rem;
    padding: 0 0.25rem;
  }
}

.none {
  color: #9ca3af;
  font-size: 0.8rem;
  padding: 0.5rem;
}
</style>
