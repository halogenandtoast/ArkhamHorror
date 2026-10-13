<script lang="ts" setup>
import { imgsrc } from '@/arkham/helpers'

/* A section is whatever the box prints under a heading: credit rows (role on the
   left, names on the right) or prose. A box may ship any number of either, in
   printed order -- Credits, Acknowledgements, Special Thanks, whatever it has. */
export type AboutSection = {
  title: string
  rows: { role: string; names: string }[]
  text: string[]
  /* Design notes discuss the ending. The printed box puts them at the back, to be
     read afterwards; the log has no "afterwards", so a section that spoils stays
     collapsed until the player asks for it. */
  spoiler: boolean
}

defineProps<{
  title: string
  blurb?: string
  designer?: string
  sections: AboutSection[]
  /* Small print the box itself carries -- a rights notice for source material it
     adapts, say. Separate from the generic unofficial-content line. */
  notes: string[]
}>()

const grunge = `url(${imgsrc('grunge.png')})`
</script>

<template>
  <div class="log-section">
    <h3 class="section-title">{{ $t('campaignLog.about.title') }}</h3>

    <div class="box intro-text">
      <h4 class="box-title">{{ title }}</h4>
      <p v-if="designer" class="box-designer">
        {{ $t('create.designedBy', { name: designer }) }}
      </p>
      <p v-if="blurb" class="box-blurb">{{ blurb }}</p>
      <p class="box-unofficial">{{ $t('campaignLog.about.unofficial') }}</p>
    </div>

    <template v-for="(section, s) in sections" :key="s">
      <details v-if="section.spoiler" class="spoiler">
        <summary>
          <span class="sub-title">{{ section.title }}</span>
          <span class="spoiler-hint">{{ $t('campaignLog.about.spoiler') }}</span>
        </summary>
        <p v-for="(paragraph, i) in section.text" :key="`t${i}`" class="prose">
          {{ paragraph }}
        </p>
      </details>

      <template v-else>
        <h4 class="sub-title">{{ section.title }}</h4>

        <dl v-if="section.rows.length > 0" class="credits">
          <template v-for="(row, i) in section.rows" :key="i">
            <dt>{{ row.role }}</dt>
            <dd>{{ row.names }}</dd>
          </template>
        </dl>

        <p v-for="(paragraph, i) in section.text" :key="`t${i}`" class="prose">
          {{ paragraph }}
        </p>
      </template>
    </template>

    <p v-for="(note, i) in notes" :key="i" class="note">{{ note }}</p>
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

.intro-text {
  color: var(--neutral-extra-dark);
  background: linear-gradient(#DFDAD8, #c9c4c2);
  background-image: v-bind(grunge);
  background-size: cover;
  border-radius: 5px;
  padding: 16px 20px;
  letter-spacing: .03em;
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
  text-rendering: optimizelegibility;
}

.box-title {
  font-family: teutonic, sans-serif;
  font-weight: normal;
  font-size: 1.3em;
  margin: 0;
}

.box-designer {
  font-style: italic;
  font-size: 0.95em;
  opacity: 0.75;
  margin: 2px 0 0;
}

.box-blurb {
  margin: 10px 0 0;
  line-height: 1.5;
  text-align: justify;
}

.box-unofficial {
  margin: 10px 0 0;
  font-size: 0.85em;
  line-height: 1.4;
  opacity: 0.7;
}

.sub-title {
  font-family: teutonic, sans-serif;
  font-size: 0.95em;
  font-weight: normal;
  color: rgba(255, 255, 255, 0.75);
  text-transform: uppercase;
  letter-spacing: 0.08em;
  margin: 16px 0 8px;
}

.credits {
  display: grid;
  grid-template-columns: minmax(0, 1fr) minmax(0, 1.4fr);
  gap: 6px 16px;
  margin: 0;
}

.credits dt {
  font-family: teutonic, sans-serif;
  color: rgba(255, 255, 255, 0.6);
  letter-spacing: 0.03em;
  line-height: 1.35;
}

.credits dd {
  margin: 0;
  color: rgba(255, 255, 255, 0.9);
  line-height: 1.35;
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
  width: 0;
  height: 0;
  border-left: 5px solid rgba(255, 255, 255, 0.5);
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
  color: rgba(255, 255, 255, 0.45);
}

.spoiler > summary:hover > .spoiler-hint {
  color: rgba(255, 255, 255, 0.7);
}

.spoiler {
  margin-top: 16px;
}

.spoiler[open] > summary {
  margin-bottom: 8px;
}

.prose {
  margin: 0 0 8px;
  line-height: 1.5;
  color: rgba(255, 255, 255, 0.85);
}

.prose:last-of-type {
  margin-bottom: 0;
}

.note {
  margin: 14px 0 0;
  font-size: 0.85em;
  line-height: 1.45;
  color: rgba(255, 255, 255, 0.55);
}

/* Role above names once the two columns stop fitting side by side. */
@media (max-width: 480px) {
  .credits {
    grid-template-columns: minmax(0, 1fr);
    gap: 2px;
  }

  .credits dt {
    font-size: 0.85em;
  }

  .credits dd {
    margin-bottom: 8px;
  }
}
</style>
