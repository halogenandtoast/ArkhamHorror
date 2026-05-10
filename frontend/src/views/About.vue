<script setup lang="ts">
import { useRoute } from 'vue-router';
import { computed } from 'vue';
import { useI18n } from 'vue-i18n';

const route = useRoute();
const { t } = useI18n();
const isSupportPage = computed(() => route.query.support !== undefined);

</script>
<template>
  <div class="page-container">
    <div class="page-content column">
      <section class="column" :class="{ focus: isSupportPage }">
        <h2 class="title">{{ t('about.supportTitle') }}</h2>
        <div class="box column">
          <p>{{ t('about.supportBody') }}</p>

          <p><a href="https://patreon.com/HalogenAndToast?utm_medium=unknown&utm_source=join_link&utm_campaign=creatorshare_creator&utm_content=copyLink">Patreon</a></p>

          <p>{{ t('about.supportNote') }}</p>
        </div>
      </section>
      <section class="column">
        <h2 class="title">{{ t('about.copyrightTitle') }}</h2>
        <div class="box column">
          <p v-html="t('about.copyrightBody')"></p>

          <p>{{ t('about.copyrightArtwork') }}</p>
        </div>
      </section>
      <section class="column">
        <h2 class="title">{{ t('about.projectTitle') }}</h2>

        <div class="box" v-html="t('about.projectBody')"></div>
      </section>
    </div>
  </div>
</template>

<style scoped>
.focus {
  .box {
    border: 1px solid var(--select);
    a {
      color: var(--select);
    }
  }
}

@media (max-width: 768px) {
  .page-content {
    width: 100%;
    padding: 0 12px;
    box-sizing: border-box;
  }
}
</style>
