<script setup lang="ts">
import { useRoute } from 'vue-router';
import { computed } from 'vue';

const route = useRoute();
const isSupportPage = computed(() => route.query.support !== undefined);

</script>
<template>
  <div class="page-container">
    <div class="page-content column">
      <section class="column" :class="{ focus: isSupportPage }">
        <h2 class="title">Support</h2>
        <div class="box column">
          <p>It's quite expensive to run the infrastructure for this service. This project relies on contributions from the community in order to cover those costs. Please visit our Patreon if you are able to financially support this project.</p>

          <p><a href="https://www.patreon.com/HalogenAndToast">Patreon</a></p>

          <p>Please note that no features are locked behind a paywall. This project is free to use for everyone and will remain so for as long as it continues to run.</p>
        </div>
      </section>
      <section class="column">
        <h2 class="title">Copyright Disclaimer</h2>
        <div class="box column">
          <p>The information presented in this app about <a href="https://www.fantasyflightgames.com/en/products/arkham-horror-the-card-game/">Arkham Horror: The Card Game™</a>, both textual and graphical, is © Fantasy Flight Games 2024. This app is a fan project and is not produced, endorsed, or supported by, or affiliated with Fantasy Flight Games.</p>

          <p>All artwork and illustrations are the intellectual property of their respective creators. All Arkham Horror: The Card Game™ images and graphics are copyrighted by Fantasy Flight Games.</p>
        </div>
      </section>
      <section class="column">
        <h2 class="title">Project Details</h2>

        <div class="box">
          This application was created by <a href="https://github.com/halogenandtoast">halogenandtoast</a> and contributors to support the Arkham Horror: The Card Game community. The source code of this project is available at <a href="https://github.com/halogenandtoast/ArkhamHorror">Github</a>. Feedback and bug reports are welcome via Github issues or the dedicated channel on the Mythos Busters discord server.
        </div>
      </section>
    </div>
  </div>
</template>

<style lang="scss" scoped>
.focus {
  .box {
    border: 1px solid var(--select);
    a {
      color: var(--select);
    }
  }
}
</style>
