<script lang="ts" setup>
import Draggable from '@/components/Draggable.vue';
import { computed } from 'vue';
import { useDebug } from '@/arkham/debug';
import { imgsrc } from '@/arkham/helpers';
import type { Game } from '@/arkham/types/Game';
import { useMenu } from '@/composeable/menu';

const props = defineProps<{
  game: Game
  card: ConcealedCard
  playerId: string
}>()

const { addEntry } = useMenu()

const emit = defineEmits<{ close: [] }>()

addEntry({
  id: `close-debug-${props.card.id}`,
  content: "",
  shortcut: "Escape",
  action: () => emit('close')
})



const imageName = computed(() => {
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
    case "CoterieAssassinA": return 'coterie-assasin-a'
    case "CoterieAssassinB": return 'coterie-assasin-b'
    case "CoterieEnforcerA": return 'coterie-enforcer-a'
    case "CoterieEnforcerB": return 'coterie-enforcer-b'
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

const debug = useDebug()
</script>

<template>
  <Draggable>
    <template #handle><h2>Debug Concealed Card</h2></template>
    <div class="concealed-card--outer">
      <div class="concealed-card" :data-index="card.id">
        <div class="card-frame">
          <div class="card-wrapper">
            <img :src="image" class="card-no-overlay" />
          </div>
        </div>
      </div>
      <div class="buttons">
        <button @click="emit('close')">Close</button>
      </div>
    </div>
  </Draggable>
</template>

<style scoped>
.card-no-overlay {
  width: calc(var(--card-width) * 5); 
  max-width: calc(var(--card-width) * 5);
  border-radius: 15px;
  transform: rotate(0deg);
  transition: transform 0.2s linear;
}

.concealed-card {
  display: flex;
  flex-direction: column;
}

.buttons {
  display: flex;
  flex-direction: column;
  justify-content: space-around;
  flex: 1;
  gap: 5px;
}

.concealed-card--outer {
  padding: 10px;
  display: flex;
  flex-direction: row;
  align-items: center;
  gap: 10px;
}

.button{
  margin-top: 2px;
  border: 0;
  color: #fff;
  border-radius: 4px;
  border: 1px solid #ff00ff;
}

.card-frame {
  position: relative;
  display: flex;
  align-items: center;
  justify-content: center;
}
</style>
