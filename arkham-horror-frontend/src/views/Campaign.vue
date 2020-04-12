<template>
  <div id="campaign" v-if="ready">
    {{campaign.difficulty}} Campaign
  </div>
</template>

<script lang="ts">
import { Component, Prop, Vue } from 'vue-property-decorator';
import api from '@/api';
import { ArkhamHorrorCampaign } from '@/arkham/types';

@Component
export default class Campaign extends Vue {
  @Prop(String) readonly campaignId!: string;

  private ready = false;
  private campaign: ArkhamHorrorCampaign | null = null;

  async mounted() {
    this.campaign = await api
      .get(`arkham/campaigns/${this.campaignId}`)
      .then((response) => Promise.resolve(response.data));
    this.ready = true;
  }
}
</script>
