<script lang="ts" setup>

import { computed } from 'vue'
import * as ArkhamGame from '@/arkham/types/Game'
import { imgsrc } from '@/arkham/helpers';
import { Game } from '@/arkham/types/Game';
import { ConcealedCard } from '@/arkham/types/Game';
import AbilityButton from '@/arkham/components/AbilityButton.vue'
import { AbilityLabel, AbilityMessage, Message, MessageType } from '@/arkham/types/Message'

const props = defineProps<{
  game: Game
  card: ConcealedCard
  playerId: string
}>()

const emit = defineEmits<{
  choose: [value: number]
}>()

const id = computed(() => props.card.id)
const choices = computed(() => ArkhamGame.choices(props.game, props.playerId))
const choose = (idx: number) => emit('choose', idx)

const imageName = computed(() => {
  if (!props.card.flipped) return 'concealed-card'

  switch (props.card.kind) {
    case "Decoy": return 'decoy'
    case "AcolyteAny": return 'acolyte-any'
    case "ApportionedKa": return 'apportioned-ka'
    case "CityOfRemnantsL": return 'city-of-remnants-l'
    case "CityOfRemnantsM": return 'city-of-remnants-m'
    case "CityOfRemnantsR": return 'city-of-remnants-r'
    case "CoterieAgentA": return 'coterie-agent-a'
    case "CoterieAgentB": return 'coterie-agent-b'
    case "CoterieAgentC": return 'coterie-agent-c'
    case "CoterieAssasinA": return 'coterie-assasin-a'
    case "CoterieAssasinB": return 'coterie-assasin-b'
    case "CoteriaEnforcerA": return 'coterie-enforcer-a'
    case "CoteriaEnforcerB": return 'coterie-enforcer-b'
    case "DecoyVoidChimeraEarsplitter": return 'decoy-void-chimera-earsplitter'
    case "DecoyVoidChimeraFellbeak": return 'decoy-void-chimera-fellbeak'
    case "DecoyVoidChimeraFellhound": return 'decoy-void-chimera-fellhound'
    case "DecoyVoidChimeraGorefeaster": return 'decoy-void-chimera-gorefeaster'
    case "DesiderioDelgadoAlvarez": return 'desiderio-delgado-alvarez'
    case "EmissaryFromYuggoth": return 'emissary-from-yuggoth'
    case "LaChicaRoja": return 'la-chica-roja'
    case "MimeticNemesis": return 'mimetic-nemesis'
    case "SinisterAspirantA": return 'sinister-aspirant-a'
    case "SinisterAspirantB": return 'sinister-aspirant-b'
    case "SinisterAspirantC": return 'sinister-aspirant-c'
    case "TheRedGlovedMan": return 'the-red-gloved-man'
    case "TzuSanNiang": return 'tzu-san-niang'
    case "VoidChimeraTrueForm": return 'void-chimera-true-form'
    case "WizardOfTheOrder": return 'wizard-of-the-order'
  }
})

const image = computed(() => {
  return imgsrc(`mini-cards/${imageName.value}.jpg`);
})

function isAbility(v: Message): v is AbilityLabel {
  if (v.tag !== MessageType.ABILITY_LABEL) {
    return false
  }

  const { source } = v.ability;

  if (source.sourceTag === 'ProxySource') {
    if ("contents" in source.source) {
      return source.source.contents === id.value
    }
  } else if (source.tag === 'ConcealedCardSource') {
    return source.contents === id.value
  }

  return false
}

const abilities = computed(() => {
  return choices
    .value
    .reduce<AbilityMessage[]>((acc, v, i) => {
      if (isAbility(v)) {
        return [...acc, { contents: v, displayAsAction: false, index: i}];
      }

      return acc;
    }, []);
})

function canInteract(c: Message): boolean {
  if (c.tag === MessageType.TARGET_LABEL && c.target.contents === id.value) {
    return true
  }
  return false
}

const cardAction = computed(() => choices.value.findIndex(canInteract))

</script>

<template>
  <div class="concealed-card">
    <img
      :src="image"
      class="concealed-card"
      :data-image="image"
      :class="{'concealed-card--can-interact': cardAction !== -1 }"
      @click="$emit('choose', cardAction)"
    />
    <AbilityButton
      v-for="ability in abilities"
      :key="ability.index"
      :ability="ability.contents"
      :data-image="image"
      :game="game"
      @click="$emit('choose', ability.index)"
      />
  </div>
</template>

<style scoped>
.concealed-card--can-interact {
  border: 1px solid var(--select);
  border-radius: 2px;
  cursor: pointer;
}
</style>
