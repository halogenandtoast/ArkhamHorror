<script lang="ts" setup>
import { computed } from 'vue'
import { formatContent, imgsrc } from '@/arkham/helpers'
import { narrativeHost } from '@/arkham/composables/useNarrative'

/* A section is whatever the box prints under a heading: credit rows (role on the
   left, names on the right) or prose. A box may ship any number of either, in
   printed order -- Credits, Acknowledgments, Design Notes, whatever it has. */
export type AboutSection = {
  title: string
  rows: { role: string; names: string }[]
  text: string[]
  /* Design notes discuss the ending. The printed box puts them at the back, to be
     read afterwards; the log has no "afterwards", so a section that spoils stays
     collapsed until the player asks for it. */
  spoiler: boolean
}

type Props = {
  title: string
  blurb?: string
  designer?: string
  /* Content still in testing, labelled where it is being read rather than only
     where it was chosen -- a game is usually joined, not created. */
  beta?: boolean
  /* Where the work itself lives, for a player who wants the original. */
  url?: string
  sections: AboutSection[]
  /* Small print the box itself carries -- a rights notice for source material it
     adapts, say. Separate from the generic unofficial-content line. */
  notes: string[]
}

const props = defineProps<Props>()
const host = computed(() => narrativeHost(props.url))

const grunge = `url(${imgsrc('grunge.png')})`
</script>

<template>
  <div class="log-section">
    <h3 class="section-title">{{ $t('campaignLog.about.title') }}</h3>

    <!-- One page rather than a stack of boxes: this is all prose and names, so it
         reads on the parchment the rest of the log's flavor text uses. -->
    <div class="page">
      <header class="masthead">
        <h4 class="box-title">
          {{ title }}
          <span v-if="beta" class="beta-pill">{{ $t('campaignLog.about.beta') }}</span>
        </h4>
        <p v-if="designer" class="box-designer">
          {{ $t('create.designedBy', { name: designer }) }}
        </p>
        <p v-if="host" class="box-link">
          <a :href="url" target="_blank" rel="noopener noreferrer">{{ host }}</a>
        </p>
      </header>

      <p v-if="blurb" class="blurb" v-html="formatContent(blurb)"></p>

      <template v-for="(section, s) in sections" :key="s">
        <details v-if="section.spoiler" class="section spoiler">
          <summary>
            <span class="sub-title">{{ section.title }}</span>
            <span class="spoiler-hint">{{ $t('campaignLog.about.spoiler') }}</span>
          </summary>
          <p
            v-for="(paragraph, i) in section.text"
            :key="`t${i}`"
            class="prose"
            v-html="formatContent(paragraph)"
          ></p>
        </details>

        <section v-else class="section">
          <h5 class="sub-title">{{ section.title }}</h5>

          <dl v-if="section.rows.length > 0" class="credits">
            <template v-for="(row, i) in section.rows" :key="i">
              <dt v-html="formatContent(row.role)"></dt>
              <dd v-html="formatContent(row.names)"></dd>
            </template>
          </dl>

          <p
            v-for="(paragraph, i) in section.text"
            :key="`t${i}`"
            class="prose"
            v-html="formatContent(paragraph)"
          ></p>
        </section>
      </template>

      <!-- A div, not a <footer>: base.css styles the bare element as the app's own
           page footer (fixed to the viewport bottom, centred, 0.5em). -->
      <div class="fine-print">
        <p v-for="(note, i) in notes" :key="i" v-html="formatContent(note)"></p>
        <p>{{ $t('campaignLog.about.unofficial') }}</p>
      </div>
    </div>
  </div>
</template>

<style scoped>
.log-section {
  background: var(--box-background);
  border: 1px solid rgba(255, 255, 255, 0.07);
  border-radius: 8px;
  padding: 14px 16px;
}

.section-title {
  font-family: teutonic, sans-serif;
  font-size: 1.1em;
  font-weight: normal;
  color: rgba(255, 255, 255, 0.75);
  text-transform: uppercase;
  letter-spacing: 0.08em;
  margin: 0 0 10px;
  padding-bottom: 8px;
  border-bottom: 1px solid rgba(255, 255, 255, 0.07);
}

.page {
  color: var(--page-ink);
  background: linear-gradient(#DFDAD8, #c9c4c2);
  background-image: v-bind(grunge);
  background-size: cover;
  border-radius: 5px;
  padding: 20px 24px;
  letter-spacing: .02em;
  line-height: 1.6;
  --measure: 74ch;
  --page-ink: #1c1b17;
  --page-ink-2: #403d35;
  --page-ink-3: #514d44;
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
  text-rendering: optimizelegibility;
}

.masthead {
  padding-bottom: 12px;
  border-bottom: 1px solid rgba(0, 0, 0, 0.18);
}

.box-title {
  font-family: teutonic, sans-serif;
  font-weight: normal;
  font-size: 1.5em;
  margin: 0;
}

/* Darkgoldenrod, as every other beta marker in the app, but on parchment it
   needs the darker ink to stay legible. */
.beta-pill {
  font-family: sans-serif;
  font-size: 10px;
  font-weight: 700;
  letter-spacing: 0.08em;
  text-transform: uppercase;
  padding: 2px 8px;
  margin-left: 8px;
  border-radius: 999px;
  background: darkgoldenrod;
  color: white;
  white-space: nowrap;
  vertical-align: middle;
}

.box-designer {
  font-style: italic;
  font-size: 0.95em;
  color: var(--page-ink-2);
  margin: 2px 0 0;
}

/* The marker `[^n]` leaves where a footnote is referenced, and the same marker
   opening the note itself at the foot of the page. `FormattedEntry` styles these
   for flavor text; the About page renders its own prose, so it needs its own. */
:deep(.footnote-ref) {
  font-size: 0.7em;
  font-style: normal;
  font-weight: bold;
  vertical-align: super;
  line-height: 0;
  padding-left: 1px;
}

/* `reset.css` sets `* { font: inherit }`, which flattens the emphasis
   `formatContent` emits, so the markup has to ask for it back. `:deep`, because
   v-html content carries no scope attribute. */
:deep(i) {
  font-style: italic;
}

:deep(strong),
:deep(b) {
  font-weight: bold;
}

/* The host, as its own text: it says where it goes and needs no translation. */
.box-link {
  font-size: 0.9em;
  margin: 4px 0 0;
}

.box-link a {
  color: #5a4a1e;
  text-decoration: none;
  border-bottom: 1px dotted rgba(90, 74, 30, 0.5);
}

.box-link a:hover {
  color: var(--page-ink);
}

.blurb {
  margin: 14px 0 0;
  max-width: var(--measure);
  line-height: 1.65;
}

.section {
  margin-top: 18px;
}

.sub-title {
  display: block;
  font-family: teutonic, sans-serif;
  font-size: 1.05em;
  font-weight: normal;
  text-transform: uppercase;
  letter-spacing: 0.08em;
  margin: 0 0 8px;
}

/* No gap: the stripe has to run unbroken across both columns, so the gutter is
   padding on the cells instead. */
.credits {
  display: grid;
  grid-template-columns: minmax(0, 1fr) minmax(0, 1.4fr);
  margin: 0;
  border-radius: 3px;
  overflow: hidden;
}

.credits dt,
.credits dd {
  padding: 7px 10px;
}

/* Light bands, so a role and its names read as one row rather than two. */
.credits dt:nth-of-type(odd),
.credits dd:nth-of-type(odd) {
  background: rgba(0, 0, 0, 0.055);
}

.credits dt {
  font-family: teutonic, sans-serif;
  letter-spacing: 0.03em;
  line-height: 1.45;
  color: var(--page-ink-2);
}

.credits dd {
  margin: 0;
  line-height: 1.45;
}

/* The credit rows read as a list; prose after them wants air. */
.credits + .prose {
  margin-top: 12px;
}

.prose {
  margin: 0 0 12px;
  max-width: var(--measure);
  line-height: 1.65;
}

.prose:last-child {
  margin-bottom: 0;
}

.spoiler > summary {
  display: flex;
  align-items: baseline;
  gap: 8px;
  cursor: pointer;
  list-style: none;
}

.spoiler > summary::-webkit-details-marker {
  display: none;
}

/* The disclosure triangle, rotating on open. */
.spoiler > summary::before {
  content: '';
  flex: none;
  width: 0;
  height: 0;
  border-left: 5px solid currentcolor;
  border-top: 4px solid transparent;
  border-bottom: 4px solid transparent;
  transition: transform 150ms ease;
}

.spoiler[open] > summary::before {
  transform: rotate(90deg);
}

.spoiler > summary > .sub-title {
  margin: 0;
}

.spoiler-hint {
  font-size: 0.8em;
  font-style: italic;
  color: var(--page-ink-3);
}

.spoiler > summary:hover > .spoiler-hint {
  color: var(--page-ink);
}

.fine-print {
  margin-top: 18px;
  padding-top: 12px;
  border-top: 1px solid rgba(0, 0, 0, 0.18);
  max-width: var(--measure);
  font-size: 0.85em;
  line-height: 1.55;
  color: var(--page-ink-3);
}

.fine-print p {
  margin: 0 0 6px;
}

.fine-print p:last-child {
  margin-bottom: 0;
}

/* Role above names once the two columns stop fitting side by side. */
@media (max-width: 480px) {
  .page {
    padding: 16px 16px;
  }

  .credits {
    grid-template-columns: minmax(0, 1fr);
  }

  .credits dt {
    font-size: 0.85em;
    padding-bottom: 1px;
  }

  .credits dd {
    padding-top: 1px;
  }

  .sub-title {
    font-size: 1em;
  }
}
</style>
