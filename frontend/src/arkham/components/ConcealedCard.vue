<script lang="ts" setup>

import { computed, ref, onBeforeUnmount, nextTick } from 'vue'
import * as ArkhamGame from '@/arkham/types/Game'
import { imgsrc } from '@/arkham/helpers';
import { Game } from '@/arkham/types/Game';
import { ConcealedCard } from '@/arkham/types/Game';
import AbilitiesMenu from '@/arkham/components/AbilitiesMenu.vue'
import { AbilityLabel, AbilityMessage, Message, MessageType } from '@/arkham/types/Message'

const props = defineProps<{
  game: Game
  card: ConcealedCard
  playerId: string
}>()

const emit = defineEmits<{
  choose: [value: number]
}>()

const frame = ref(null)
const id = computed(() => props.card.id)
const choices = computed(() => ArkhamGame.choices(props.game, props.playerId))
const choose = (idx: number) => emit('choose', idx)

const showAbilities = ref<boolean>(false)

const abilitiesEl = ref<HTMLElement | null>(null)

async function chooseAbility(ability: number) {
  abilitiesEl.value?.blur()
  choose(ability)
}

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
  if (v.tag === MessageType.FIGHT_LABEL && v.enemyId === id.value) {
    return true
  }

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

function isCardAction(c: Message): boolean {
  if (c.tag === MessageType.TARGET_LABEL && c.target.contents === id.value) {
    return true
  }
  return false
}

const canInteract = computed(() => abilities.value.length > 0 || cardAction.value !== -1)
const cardAction = computed(() => choices.value.findIndex(isCardAction))

let clickTimeout: ReturnType<typeof setTimeout> | null = null
// clickCount is used to determine if the user clicked once or twice
let clickCount = 0

onBeforeUnmount(() => {
  if (clickTimeout) {
    clearTimeout(clickTimeout)
    clickTimeout = null
  }
  clickCount = 0
})

async function clicked() {
  clickCount++;
  if (clickTimeout) clearTimeout(clickTimeout);
  clickTimeout = setTimeout(async () => {
    // Ensure this does not conflict with the double-click zoom-in functionality (toggleZoom in Scenario.vue)
    if (clickCount === 1){
      if(cardAction.value !== -1) {
        choose(cardAction.value)
      } else if (abilities.value.length > 0) {
        showAbilities.value = !showAbilities.value
        await nextTick()
        if (showAbilities.value === true) {
          abilitiesEl.value?.focus()
        } else {
          abilitiesEl.value?.blur()
        }
      }
    }

    // Reset click count and timeout
    clickCount = 0;
    clickTimeout = null;
  }, 300);
}

</script>

<template>
  <div class="concealed-card" ref="frame">
    <img
      :src="image"
      class="concealed-card"
      :data-image="image"
      :class="{'concealed-card--can-interact': canInteract}"
      @click="clicked"
    />
    <AbilitiesMenu
      v-model="showAbilities"
      :abilities="abilities"
      :frame="frame"
      :show-move="abilities.length > 1"
      :game="game"
      position="left"
      @choose="chooseAbility"
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
