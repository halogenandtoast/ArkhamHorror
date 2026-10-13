<script lang="ts" setup>
import { imgsrc } from '@/arkham/helpers'

defineProps<{
  title: string
  blurb?: string
  designer?: string
  credits: { role: string; names: string }[]
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

    <template v-if="credits.length > 0">
      <h4 class="credits-title">{{ $t('campaignLog.about.credits') }}</h4>
      <dl class="credits">
        <template v-for="credit in credits" :key="credit.role">
          <dt>{{ credit.role }}</dt>
          <dd>{{ credit.names }}</dd>
        </template>
      </dl>
    </template>
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

.credits-title {
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
