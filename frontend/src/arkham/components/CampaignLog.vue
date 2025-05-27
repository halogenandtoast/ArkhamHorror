<script lang="ts" setup>
import * as Arkham from '@/arkham/types/Game'
import { LogContents, LogKey, formatKey, logContentsDecoder } from '@/arkham/types/Log';
import {imgsrc} from '@/arkham/helpers'
import { computed, ref, onMounted, watch } from 'vue'
import { fetchCard } from '@/arkham/api';
import type { CardDef } from '@/arkham/types/CardDef'
import { type Name, simpleName } from '@/arkham/types/Name'
import { scenarioToI18n, type Remembered } from '@/arkham/types/Scenario'
import Supplies from '@/arkham/components/Supplies.vue';
import XpBreakdown from '@/arkham/components/XpBreakdown.vue';
import InvestigatorRow from '@/arkham/components/InvestigatorRow.vue';
import { toCapitalizedWords } from '@/arkham/helpers';
import { useI18n } from 'vue-i18n';
import { Seal } from '@/arkham/types/Seal';

export interface Props {
  game: Arkham.Game
  cards: CardDef[]
  playerId: string
}

const props = defineProps<Props>()

const { t } = useI18n()
const mainLog = props.game.campaign?.log || props.game.scenario?.standaloneCampaignLog || { recorded: [], recordedSets: [], recordedCounts: [] }

const remembered = computed(() => {
  const log = props.game.scenario?.log
  if (!log) return []
  if (!props.game.scenario) return []
  const prefix = scenarioToI18n(props.game.scenario)
  return log.map((record: Remembered) => {
    if (record.tag == 'YouOweBiancaResources') {
      return `You owe Bianca resources (${record.contents})`
    }

    if (record.tag === 'RememberedName') {
      return t(`${prefix}.remembered.${record.actualTag.charAt(0).toLowerCase() + record.actualTag.slice(1)}`, { name: simpleName(record.name) })
    }

    return t(`${prefix}.remembered.${record.tag.charAt(0).toLowerCase() + record.tag.slice(1)}`)
  })
})

const otherLog = ref<LogContents | null>(null)

if (props.game.campaign?.meta?.otherCampaignAttrs?.log) {
  logContentsDecoder.decodePromise(props.game.campaign?.meta?.otherCampaignAttrs?.log).then(res => otherLog.value = res)
}

const breakdowns =
  props.game.campaign?.xpBreakdown ||
    (props.game.scenario && props.game.scenario.xpBreakdown ? [[{ "tag": "ScenarioStep", "contents": props.game.scenario.id }, props.game.scenario.xpBreakdown]] : undefined) ||
    []

const logTitle = props.game.campaign?.meta?.currentCampaignMode ?
  (props.game.campaign.meta.currentCampaignMode === 'TheDreamQuest' ? "The Dream-Quest" : "The Web of Dreams") : null


const otherLogTitle = logTitle ?
  (logTitle === 'The Dream-Quest' ? 'The Web of Dreams' : 'The Dream-Quest') : null

const logTitles = logTitle && otherLogTitle ? [logTitle, otherLogTitle].sort() : null


const campaignLog = ref(mainLog)

const recorded = computed(() => campaignLog.value.recorded.filter((r) => {
  return !["Teachings1", "Teachings2", "Teachings3"].includes(r.tag)
}).map(formatKey))
const recordedSets = computed(() => campaignLog.value.recordedSets)
const recordedCounts = computed(() => campaignLog.value.recordedCounts)
const partners = computed(() => campaignLog.value.partners)
const hasSupplies = computed(() => Object.values(props.game.investigators).some((i) => i.supplies.length > 0))

const loadedCards = ref<CardDef[]>([]);

// Function to load missing cards
async function loadMissingCards() {
  const nonCardKeys = ['theCircleUndone.key.mementosDiscovered', 'theInnsmouthConspiracy.key.memoriesRecovered', 'theInnsmouthConspiracy.key.possibleSuspects', 'theInnsmouthConspiracy.key.possibleHideouts', 'edgeOfTheEarth.key.suppliesRecovered', 'edgeOfTheEarth.key.sealsPlaced', 'edgeOfTheEarth.key.sealsRecovered'];
  const missingCardCodes = new Set();
  for (const [key, setValue] of Object.entries(recordedSets.value)) {
    if (nonCardKeys.includes(key)) continue;
    for (const val of setValue) { // Assuming setValue is an entry [setKey, setValues]
      const cardCode = val.contents; // Assuming this is how you get the card code
      if (!findCard(cardCode)) {
        missingCardCodes.add(cardCode);
      }
    }
  }

  const missingCardsPromises = Array.from(missingCardCodes).map(code => fetchCard(code.replace(/^c/, '')));
  const fetchedCards = await Promise.all(missingCardsPromises);

  loadedCards.value.push(...fetchedCards);
}

// Invoke the loading function when the component mounts or recordedSets changes
onMounted(loadMissingCards);
watch(recordedSets, loadMissingCards, { deep: true });

const findCard = (cardCode: string): CardDef | undefined => {
  return props.cards.find((c) => c.cardCode == cardCode) || loadedCards.value.find(c => c.cardCode == cardCode);
}

const displayRecordValue = (key: string, value: SomeRecordable): string => {
  if (key === 'theCircleUndone.key.mementosDiscovered') {
    const contents = value.contents || value.recordVal?.contents
    return toCapitalizedWords(contents)
  }

  if (key === 'theInnsmouthConspiracy.key.memoriesRecovered') {
    const contents = value.contents || value.recordVal?.contents
    const memory = contents.charAt(0).toLowerCase() + contents.slice(1)
    return t(`theInnsmouthConspiracy.memoriesRecovered.${memory}`)
  }

  if (key === 'theInnsmouthConspiracy.key.outForBlood') {
    const contents = value.contents || value.recordVal?.contents
    const suspect = contents.charAt(0).toLowerCase() + contents.slice(1)
    return t(`theInnsmouthConspiracy.possibleSuspects.${suspect}`, suspect)
  }

  if (key === 'theInnsmouthConspiracy.key.possibleSuspects') {
    const contents = value.contents || value.recordVal?.contents
    const suspect = contents.charAt(0).toLowerCase() + contents.slice(1)
    return t(`theInnsmouthConspiracy.possibleSuspects.${suspect}`, suspect)
  }

  if (key === 'theInnsmouthConspiracy.key.possibleHideouts') {
    const contents = value.contents || value.recordVal?.contents
    const hideout = contents.charAt(0).toLowerCase() + contents.slice(1)
    return t(`theInnsmouthConspiracy.possibleHideouts.${hideout}`, hideout)
  }

  if (key === 'edgeOfTheEarth.key.suppliesRecovered') {
    const contents = value.contents || value.recordVal?.contents
    const supply = contents.charAt(0).toLowerCase() + contents.slice(1)
    return t(`edgeOfTheEarth.suppliesRecovered.${supply}`, supply)
  }

  if (isSeal(key)) return ""

  const code = value.contents || value.recordVal?.contents
  return cardCodeToTitle(code)
}

const isSeal = (key: string): boolean => {
  return ['edgeOfTheEarth.key.sealsRecovered', 'edgeOfTheEarth.key.sealsPlaced'].includes(key)
}

const sealImage = (seal: Seal): string => {
  const revealed = seal.sealActive ? "active" : "dormant"
  switch (seal.sealKind) {
    case "SealA": return imgsrc(`seals/seal-a-${revealed}.png`)
    case "SealB": return imgsrc(`seals/seal-b-${revealed}.png`)
    case "SealC": return imgsrc(`seals/seal-c-${revealed}.png`)
    case "SealD": return imgsrc(`seals/seal-d-${revealed}.png`)
    case "SealE": return imgsrc(`seals/seal-e-${revealed}.png`)
  }
}

const cardCodeToTitle = (cardCode: string): string => {
  const card = findCard(cardCode)

  if (card) {
    return fullName(card.name)
  }

  if(cardCode == "c01121b") {
    return "The Masked Hunter"
  }

  if(cardCode == "c50026b") {
    return "Narōgath"
  }

  return "unknown"
}

const fullName = (name: Name): string => {
  const subtitle = name.subtitle
  if (subtitle) {
    return `${name.title}: ${subtitle}`
  }

  return name.title
}

const setClass = (key: string): string => {
  // split on '.' and take the last part
  return key.split('.').pop() || ''
}

const emptyLog = computed(() => {
  if ((logTitles ?? []).length > 0) return false;
  if (hasSupplies.value) return false;
  if (recorded.value.length > 0) return false;
  if (remembered.value.length > 0) return false;
  if (Object.entries(recordedSets.value).length > 0) return false;
  return true;
})

</script>

<template>
<svg class="hidden">
  <defs>
    <symbol id="icon-right-arrow" viewBox="0 0 32 32">
    <path d="M12.707 5.293l-1.414 1.414 9.293 9.293-9.293 9.293 1.414 1.414 10.707-10.707-10.707-10.707z"></path>
    </symbol>
    <symbol id="icon-health" viewBox="0 0 26 32">
      <path d="M25.731 6.63c0 0.379 0 0.758 0 1.138-0.124 0.308-0.324 0.6-0.357 0.917-0.166 1.351-0.905 1.936-2.341 1.802-0.407-0.040-0.822-0.055-1.212-0.079-0.025 0.142-0.041 0.182-0.025 0.205 0.108 0.198 0.224 0.387 0.324 0.585 0.714 1.501 1.228 3.002 0.183 4.583-0.125 0.19-0.075 0.537 0 0.782 0.39 1.367 0.523 2.742 0.166 4.117-0.407 1.564-0.39 3.121-0.083 4.693 0.083 0.435 0.158 0.877 0.166 1.312 0.025 1.699-1.013 3.010-2.764 3.311-1.942 0.332-3.793-0.008-5.578-0.853-1.419-0.672-2.855-1.407-4.374-1.786-2.083-0.521-3.752-1.407-5.063-3.074-0.739-0.948-0.872-2.197-1.926-2.994-0.531-0.403-0.955-1.177-1.054-1.833-0.357-2.489-0.208-4.97 0.722-7.356 0.174-0.458 0.664-0.798 0.872-1.256 0.191-0.403 0.216-0.893 0.232-1.343 0-0.055-0.531-0.134-0.822-0.182-0.905-0.15-1.569-0.845-1.693-1.754-0.058-0.34-0.141-0.703-0.282-1.027-0.481-1.106-0.017-2.118 1.179-2.442 0.506-0.142 1.029-0.245 1.561-0.292 1.295-0.119 2.59-0.198 3.901-0.292-0.49-1.596-0.324-2.236 0.706-2.876 1.029-0.648 1.967-0.529 3.113 0.379 0.697-0.924 1.469-1.177 2.565-0.909 0.564 0.134 1.154 0.229 1.735 0.284 0.988 0.095 1.76 0.569 1.785 1.454 0.025 0.83 0.44 1.209 1.096 1.541 0.199 0.103 0.407 0.245 0.531 0.427 0.589 0.845 1.394 1.043 2.391 0.845 0.315-0.063 0.664 0.032 0.988 0 1.66-0.182 2.806 0.442 3.362 1.975zM18.326 9.419c-0.407 0.126-0.83 0.245-1.229 0.395-0.955 0.356-1.685 1.335-1.536 2.276 0.083 0.529 0.050 0.853-0.564 0.956 1.046 1.075 2.183 1.817 3.777 2.118-0.174-0.229-0.249-0.324-0.332-0.427-0.747-0.956-0.855-2.086-0.598-3.145 0.158-0.632 0.747-1.185 0.722-1.928 0.706-0.126 1.586-0.601 1.586 0.703 0 0.032 0.108 0.063 0.166 0.087 0.149 0.585 0.216 1.209 0.465 1.762 0.589 1.288 0.581 2.173-0.456 3.066-0.581 0.498-0.415 0.964-0.208 1.351 0.299 0.553 0.166 0.948-0.1 1.422-0.1 0.174 0 0.648 0.025 0.648 0.847 0.032 0.465 0.529 0.34 0.861-0.183 0.498-0.415 0.988-0.672 1.462-0.681 1.296-0.681 1.288-1.677 0.198-0.498-0.545-1.112-1.003-1.901-0.79-0.805 0.221-1.17 0.877-1.287 1.651-0.033 0.198-0.033 0.403-0.041 0.522 0.34 0 0.614 0 0.88 0-0.158 0.174-0.332 0.514-0.465 0.498-0.772-0.087-0.631 0.308-0.606 0.782 0.017 0.34-0.116 0.766-0.349 1.019-0.149 0.166-0.797 0.229-0.872 0.118-0.515-0.782-1.046-1.58-1.361-2.449-0.374-1.027-0.531-2.133-0.78-3.2-0.083-0.008-0.158-0.024-0.241-0.032-0.274 1.177-0.54 2.354-0.797 3.461-0.266-0.032-0.83-0.008-0.855-0.119-0.199-0.743-1.229-0.664-1.436-1.557-0.207-0.869-0.075-2.37 0.913-2.671 1.17-0.363 1.378-1.478 2.133-2.157 0.091-0.079 0.025-0.356-0.025-0.521-0.282-1.011-0.589-2.023-0.847-3.034-0.050-0.19 0.149-0.474 0.066-0.601-0.282-0.403-0.647-0.751-1.129-1.272 1.237 0.229 0.938-0.632 0.971-0.94 0.141-1.478 1.229-3.066-0.282-4.362 0.423-0.158 0.896-0.332 1.361-0.498-0.83-0.988-1.585-1.928-2.324-2.829 0.465-0.237 0.838-0.427 1.212-0.616-0.050-0.111-0.1-0.221-0.158-0.332-0.398 0.15-0.888 0.229-1.187 0.49-0.191 0.166-0.216 0.648-0.116 0.924 0.191 0.561 0.531 1.083 0.78 1.628 0.1 0.205 0.124 0.442 0.183 0.672-0.216-0.008-0.432 0.024-0.639-0.024-2.117-0.458-4.217-0.261-6.309 0.15-0.266 0.055-0.473 0.411-0.706 0.632 0.299 0.229 0.573 0.545 0.913 0.672 0.307 0.111 0.689 0.024 1.121 0.024-0.498 0.3-0.93 0.553-1.353 0.806h0.017c-0.357 0.727 0.091 0.901 0.706 0.885 0.282-0.008 0.564-0.205 0.855-0.221 0.199-0.016 0.564 0.095 0.589 0.205 0.249 1.201 0.564 2.41-0.374 3.516-0.515 0.608-1.461 1.090-0.946 2.133 0.008 0.008-0.058 0.047-0.058 0.071-0.149 1.683-0.382 3.358-0.423 5.041-0.025 1.003 0.598 1.77 1.552 2.268 0.191 0.103 0.39 0.324 0.44 0.529 0.564 2.213 2.059 3.563 4.267 4.314 1.594 0.545 3.121 1.256 4.715 1.786 1.444 0.482 2.93 0.909 4.424 1.225 1.644 0.348 3.378-1.003 2.963-3.042-0.34-1.667-0.481-3.358-0.058-5.057 0.365-1.47 0.357-2.939-0.166-4.385-0.191-0.521-0.141-0.932 0.241-1.446 0.324-0.435 0.482-1.169 0.332-1.675-0.29-1.011-0.838-1.952-1.287-2.923 0.125-1.288 0.125-1.288 1.527-1.241 0.357 0.008 0.714 0.047 1.079 0.063 0.324 0.008 0.648 0 0.971 0 0-0.103 0-0.205 0-0.308-1.378-0.284-2.772-0.466-4.192-0.142-0.473 0.095-0.905 0.363-1.353 0.553zM18.974 5.919c-1.004 0.324-1.859 0.917-2.615 0.205-0.938-0.885-1.81-0.474-2.731-0.111-0.058 0.024-0.133 0.008-0.199 0.024-1.336 0.403-2.001 1.383-1.834 2.718 0.108 0.869-0.398 1.825 0.44 2.592 0.050 0.047-0.091 0.261-0.1 0.403-0.017 0.316-0.091 0.885 0.017 0.917 0.631 0.213 0.772-0.363 1.038-0.735 0.017-0.024-0.008-0.071 0.008-0.095 0.141-0.253 0.274-0.514 0.432-0.751 0.789-1.177 2.656-0.924 3.329-2.283 0.066-0.134 0.415-0.182 0.631-0.198 2.067-0.158 4.134-0.284 6.201-0.458 0.506-0.040 1.004-0.221 1.511-0.34-0.183-0.253-0.374-0.506-0.531-0.766-0.066-0.103-0.050-0.237-0.1-0.356-0.241-0.632-0.697-0.932-1.428-0.893-0.988 0.055-1.992-0.024-2.972 0.087-0.589 0.063-0.88-0.111-1.229-0.521-0.465-0.537-1.062-0.964-1.602-1.438-0.033 0.047-0.075 0.087-0.108 0.134 0.564 0.585 1.154 1.169 1.843 1.865zM13.454 1.186c-0.739-0.158-1.029 0.142-1.17 0.679-0.125 0.49-0.158 1.051-0.457 1.43-0.506 0.648-0.133 0.719 0.398 0.751 1.029 0.063 0.548-0.687 0.523-1.051-0.041-0.703-0.183-1.367 0.706-1.809zM8.033 12.272c-0.091-0.521-0.863-0.845-0.432-1.509 0.083-0.119-0.017-0.348-0.033-0.521-0.191 0.095-0.473 0.15-0.573 0.3-0.548 0.861-1.087 1.738-1.552 2.647-0.125 0.253 0.033 0.624 0.025 0.94 0 0.15-0.075 0.3-0.116 0.45-1.129-0.308-0.755 0.521-0.722 0.948 0.058 0.743 0.257 1.478 0.365 2.22 0.033 0.205-0.125 0.442-0.075 0.64 0.091 0.379 0.266 0.735 0.398 1.106 0.191-0.229 0.423-0.435 0.573-0.687 0.365-0.624 0.681-1.28 1.046-1.896 0.498-0.845 1.17-1.628 1.511-2.521 0.241-0.648 0.34-1.185 1.295-1.106-0.041-0.111-0.083-0.213-0.133-0.324-0.382 0.032-0.755 0.063-1.146 0.095-0.108-0.19-0.382-0.466-0.432-0.782zM2.505 5.793c1.121-0.442 2.333-0.529 3.329 0.016 0.913 0.498 1.984 1.138 1.793 2.584 0.398-0.363 0.78-0.758 0.706-1.051-0.166-0.64 0.033-1.485-0.896-1.817-1.212-0.427-2.449-0.506-3.71-0.3-0.448 0.071-0.88 0.198-1.32 0.292 0.033 0.095 0.066 0.182 0.1 0.277zM2.513 7.27c0.315 0.087 0.473 0.332 0.971 0.213 1.104-0.277 2.092 0.079 2.175 1.225 0.008 0.134 0.191 0.261 0.291 0.387 0.125-0.158 0.349-0.3 0.365-0.466 0.1-1.106-1.054-2.031-2.2-1.786-0.54 0.119-1.079 0.284-1.61 0.427h0.008zM16.683 7.934c0.697-0.142 1.386-0.348 2.092-0.403 1.212-0.103 2.44-0.040 3.644-0.198 0.398-0.055 0.731-0.569 1.087-0.869-0.324-0.079-0.664-0.261-0.971-0.221-1.527 0.198-3.055 0.411-4.557 0.711-0.498 0.103-0.921 0.49-1.386 0.743 0.033 0.079 0.058 0.158 0.091 0.237zM13.595 8.092c-0.066 0.229 0.041 0.514 0.075 0.766 0.473-0.292 0.955-0.577 1.411-0.893 0.075-0.055 0.108-0.284 0.050-0.356-0.515-0.656-1.536-0.672-2.225 0.103 0.531-0.134 0.863-0.19 0.689 0.379z"></path>
      </symbol>
      <symbol id="icon-sanity" viewBox="0 0 39 32">
        <path d="M22.298 30.992c-1.303-0.472-2.385-1.189-2.943-2.569-0.089-0.227-0.514-0.3-0.754-0.481-0.807-0.599-1.596-1.226-2.412-1.816-0.319-0.236-0.665-0.508-1.028-0.572-1.401-0.254-2.287-0.999-2.66-2.433-0.080-0.309-0.505-0.699-0.807-0.744-1.702-0.263-3.28-0.726-4.681-1.843-0.417-0.336-1.126-0.281-1.693-0.427-1.871-0.481-2.873-1.652-3.183-3.595-0.089-0.554-0.31-1.080-0.461-1.616 0-1.598 0-3.196 0-4.784 0.319-0.636 0.505-1.389 0.975-1.87 1.64-1.716 2.846-4.013 5.426-4.576 0.151-0.036 0.248-0.272 0.399-0.345 1.002-0.499 1.977-1.080 3.023-1.425 1.658-0.545 3.369-0.917 5.062-1.362 2.341 0 4.681 0 7.013 0 0.931 0.29 1.853 0.645 2.793 0.872 1.755 0.427 3.538 0.753 4.548 2.56 0.089 0.154 0.231 0.363 0.364 0.372 0.94 0.118 0.798 0.826 0.807 1.462-0.257-0.091-0.523-0.163-0.771-0.281-0.479-0.227-1.161-0.372-1.365-0.763-0.736-1.416-2.084-1.652-3.334-2.043-1.48-0.463-2.988-0.799-4.273-1.135-0.399 0.182-0.736 0.49-1.028 0.454-1.259-0.145-2.553 0.454-3.803-0.172-0.186-0.091-0.479 0.036-0.718 0.091-1.968 0.445-3.998 0.744-5.896 1.416-1.481 0.517-2.828 1.453-4.185 2.306-0.452 0.281-0.7 0.89-1.091 1.307-0.7 0.753-1.543 1.389-2.119 2.233-0.337 0.481-0.31 1.253-0.372 1.906-0.027 0.263 0.266 0.608 0.186 0.817-0.523 1.38-0.035 2.733-0.018 4.094 0.027 1.825 1.888 3.277 3.458 2.896 0.186-0.045 0.496 0.064 0.638 0.209 1.56 1.552 3.44 2.079 5.55 1.897 0.603-0.054 0.833 0.154 0.762 0.799-0.186 1.743 0.594 2.524 2.332 2.469 0.275-0.009 0.692 0.245 0.807 0.49 0.559 1.162 1.401 1.925 2.651 2.097 0.585 0.082 0.727 0.409 0.984 0.89 0.363 0.69 0.94 1.471 1.605 1.725 0.603 0.227 1.507-0.064 2.145-0.39 0.266-0.136 0.24-0.944 0.293-1.453 0.044-0.372-0.009-0.753-0.018-1.126 0.355 0.163 0.789 0.245 1.055 0.499 1.179 1.144 2.704 1.080 3.848 0.49 0.887-0.454 1.826-0.772 2.686-1.244 0.505-0.281 0.966-0.726 1.321-1.189 0.895-1.171 1.144-2.678 2.128-3.886 0.567-0.699 1.108-1.952 1.188-3.014 0.257-3.386 0.213-6.763-2.252-9.496-0.78-0.872-1.569-1.734-2.349-2.596 0.222-0.245 0.434-0.49 0.709-0.79 1.862 1.162 4.14 4.004 4.584 6.237 0.213 1.080 0.603 2.088 0.824 3.141 0 0.218 0 0.436 0 0.654-0.071 0.336-0.168 0.672-0.204 1.017-0.098 0.89 0.044 1.87-0.275 2.66-0.869 2.115-1.888 4.176-2.908 6.219-1.188 2.387-3.662 2.851-5.781 3.722-0.922 0.381-2.119 0.064-3.316 0.064-0.133 1.262-1.374 1.489-2.394 1.988-0.363-0.018-0.718-0.018-1.073-0.018zM26.625 6.743c-0.532 1.507-1.064 3.014-1.596 4.521 0.098 0.064 0.195 0.136 0.302 0.2 0.789-0.763 1.587-1.525 2.376-2.288 0.142 0.345 0.142 0.563 0.035 0.663-0.771 0.699-0.727 1.307 0.107 1.934 0.16 0.118 0.23 0.499 0.204 0.735-0.115 1.089-1.312 1.888-0.816 3.168 0.098 0.245-0.009 0.563-0.018 0.844 0.293-0.145 0.674-0.209 0.869-0.445 0.275-0.318 0.408-0.763 0.532-1.008 0.408 0.626 0.674 1.48 1.215 1.734 0.833 0.399 1.49 0.844 2.048 1.598 0.603 0.808 1.977 1.035 2.686 0.608 0.807-0.481 1.268-1.108 0.674-2.242-0.222 0.327-0.372 0.626-0.594 0.853-0.133 0.145-0.363 0.191-0.541 0.291-0.168-0.499-0.514-1.026-0.461-1.498 0.106-0.944 1.144-1.616 0.86-2.733-0.018-0.054 0.257-0.145 0.293-0.254 0.115-0.372 0.346-0.808 0.248-1.126-0.576-1.879-1.747-3.35-3.218-4.594-0.754-0.645-1.49-1.325-2.553-0.654-0.177-0.527 0.452-1.507-0.754-1.099 0.195-1.108-1.179-2.115-2.491-1.934-0.328 0.045-0.745-0.027-1.019-0.209-1.933-1.28-2.97-0.926-3.679 1.298-0.053 0.182-0.248 0.454-0.346 0.445-0.266-0.036-0.567-0.145-0.762-0.327-0.293-0.29-0.514-0.663-0.869-1.144 0.018 0.136 0.044-0.045-0.018-0.1-1.817-1.434-3.67-0.699-5.514-0.054-0.098 0.036-0.239 0.1-0.257 0.173-0.098 0.39-0.16 0.781-0.239 1.171 0.284 0.009 0.567 0.018 0.842 0.045 0.195 0.018 0.381 0.082 0.567 0.091 0.426 0.027 0.851 0.082 1.268 0.027 0.337-0.045 0.656-0.236 0.984-0.363-0.106 0.563-0.452 0.872-0.559 1.244-0.098 0.363 0.018 0.808 0.124 1.198 0.035 0.118 0.461 0.263 0.488 0.236 0.213-0.3 0.39-0.626 0.55-0.953 0.133-0.272 0.222-0.572 0.328-0.853 0.266 0.209 0.638 0.354 0.771 0.626 0.346 0.735 0.913 0.826 1.622 0.953 0.745 0.136 1.552 0.49 2.101 1.008 1.339 1.235 2.491 1.062 3.28-0.645 0.204-0.436 0.452-0.844 0.683-1.271 0.080 0.054 0.16 0.091 0.248 0.127zM32.387 11.237c-1.188-0.1-0.984 1.171-1.48 1.788-0.674-1.335-0.665-1.525 0.567-2.233 0.417-0.236 0.63-1.271 1.489-0.436 1.073 1.044 1.126 1.008-0.035 1.988-0.195-0.409-0.337-1.089-0.541-1.108zM32.246 17.129c0.284-0.899 0.576-1.788 0.94-2.932 0.541 1.126 0.195 2.215-0.94 2.932zM30.933 15.685c-0.133 0.109-0.496-0.073-0.754-0.118 0.106-0.227 0.151-0.536 0.328-0.663 0.39-0.3 0.842-0.499 1.268-0.744 0.098 0.1 0.195 0.209 0.293 0.309-0.372 0.409-0.709 0.862-1.135 1.216zM19.736 23.066c0.355 0.272 0.7 0.554 1.055 0.826 0.816 0.617 1.419 1.28 0.931 2.47-0.142 0.354 0.035 0.908 0.195 1.316 0.053 0.127 0.745 0.154 0.754 0.118 0.142-0.554 0.47-1.262 0.266-1.68-0.762-1.543-1.232-3.132-0.869-4.83 0.284-1.325 0.222-2.46-0.665-3.532-0.248-0.3-0.222-0.835-0.408-1.216-0.239-0.49-0.585-0.917-0.851-1.389-0.239-0.427-0.629-0.899-0.585-1.307 0.071-0.645-0.204-0.772-0.665-0.826-1.241-0.136-2.403 0.136-3.404 0.908-0.301 0.227-0.47 0.626-0.709 0.944 0.452 0.1 0.949 0.381 1.339 0.263 0.665-0.2 1.241-0.672 1.88-0.98 0.16-0.082 0.417 0.045 0.621 0.073-0.044 0.29 0.009 0.726-0.151 0.844-0.629 0.445-1.321 0.79-1.995 1.162-0.709 0.39-1.578 0.69-0.842 1.788 0.071 0.1 0.080 0.318 0.018 0.418-0.621 1.026-0.904 2.124-0.665 3.332 0.071 0.354 0.035 0.781 0.23 1.053 0.106 0.145 0.727 0.145 0.842-0.009 0.23-0.3 0.328-0.735 0.363-1.135 0.035-0.327-0.098-0.663-0.16-0.999 0.124-0.064 0.248-0.127 0.381-0.182 0.266 0.436 0.612 0.844 0.78 1.316 0.301 0.844 0.426 1.761 0.754 2.587 0.434 1.080 1.312 1.389 2.181 0.935-0.363-0.39-0.957-0.744-1.055-1.198-0.222-1.053-0.23-2.152-0.266-3.241-0.009-0.2 0.275-0.463 0.488-0.608 0.248-0.163 0.559-0.236 0.842-0.345-0.106-0.254-0.151-0.645-0.328-0.726-0.319-0.136-0.745-0.191-1.073-0.091s-0.603 0.418-0.949 0.681c-0.355-0.744-0.674-1.416-1.082-2.279 0.789 0.254 1.241 0.218 1.161-0.626-0.009-0.127 0.372-0.327 0.594-0.427 0.16-0.073 0.496 0.018 0.523-0.064 0.115-0.3 0.133-0.772 0.55-0.336 0.204 0.209 0.284 0.708 0.195 1.008-0.115 0.418-0.443 0.781-0.692 1.189 1.188 0.418 1.41 0.681 1.747 1.879 0.142 0.508 0.098 0.799-0.514 0.926-0.301 0.064-0.771 0.427-0.771 0.654 0 0.327 0.355 0.654 0.559 0.98 0.027 0.036 0.035 0.082 0.053 0.127-0.186 0-0.355 0-0.523 0 0 0.082-0.035 0.154-0.080 0.227zM12.599 8.65c0.16 0.1 0.709-0.073 0.771-0.254 0.293-0.763 0.674-0.735 1.232-0.318 0.124 0.091 0.461 0.073 0.541-0.027 0.151-0.2 0.337-0.617 0.248-0.717-0.204-0.245-0.541-0.39-0.851-0.527-0.603-0.254-1.463 0.009-1.684-0.971-0.346-1.552-1.401-1.961-2.447-0.79-0.692 0.772-1.321 1.244-2.341 1.325-0.364 0.027-0.692 0.545-1.020 0.862-0.275 0.272-0.514 0.581-0.771 0.862-0.621 0.672-1.286 1.316-1.853 2.034-0.133 0.172-0.080 0.626 0.044 0.862 0.496 0.908 1.099 1.562 2.279 1.725 1.445 0.2 1.605 0.163 1.312-1.371-0.062-0.354-0.115-0.89 0.071-1.071 0.461-0.427 1.037-0.899 1.614-0.98 1.064-0.145 1.605-0.599 1.543-1.861 0.55 0.527 0.887 0.944 1.312 1.217zM13.308 15.032c-0.142-0.145-0.266-0.554-0.177-0.654 0.426-0.508 0.851-1.162 1.419-1.389 1.862-0.744 3.821-0.971 5.798-0.581 0.514 0.1 1.037 0.39 1.463 0.726 0.895 0.699 1.702 1.516 2.607 2.197 0.293 0.218 0.895 0.39 1.117 0.227 0.275-0.2 0.399-0.754 0.426-1.162 0.053-0.899-0.098-1.716-1.241-1.807-0.151-0.009-0.31-0.182-0.417-0.309-1.108-1.407-2.642-1.906-4.273-1.689-2.438 0.336-4.858 0.862-7.261 1.416-0.922 0.209-1.303 1.226-0.949 2.079 0.098 0.227 0.319 0.409 0.39 0.635 0.364 1.198 1.153 1.716 2.544 1.498-0.55-0.436-1.028-0.781-1.445-1.189zM12.847 18.082c-0.142 0.054-0.337 0.2-0.426 0.154-0.532-0.272-1.011-0.699-1.56-0.881-1.046-0.345-1.622-1.017-1.924-2.088-0.151-0.554-0.47-1.307-0.895-1.48-1.17-0.463-2.438-0.663-3.936-1.035 0.532 0.926 0.523 0.908 1.037 0.536 0.071-0.045 0.355 0.1 0.443 0.227 0.151 0.218 0.213 0.49 0.319 0.754-0.239 0.027-0.541 0.054-0.851 0.109-0.514 0.091-0.913 0.127-0.39 0.862 0.301 0.418 0.053 1.18 0.585 1.625 0.213 0.182 0.293 0.599 0.514 0.672 0.239 0.082 0.567-0.127 0.86-0.191 0.222-0.045 0.514-0.136 0.665-0.045 0.355 0.227 0.656 0.545 0.966 0.844 1.498 1.489 3.307 1.825 5.284 1.498 0.718-0.118 1.294-0.926 1.073-1.571-0.098-0.291-0.443-0.49-0.674-0.726-0.124 0.145-0.23 0.336-0.39 0.436-0.204 0.127-0.461 0.2-0.7 0.3zM11.145 18.69c-1.41 0.254-3.059-2.006-3.848-3.795 1.303 1.289 2.607 2.569 3.848 3.795zM28.868 26.243c0.434-0.236 0.683-0.844 1.020-1.316 1.161 0.717 2.048 0.4 2.784-1.153 0.080-0.173 0-0.418 0-0.626-0.248 0.027-0.55-0.036-0.718 0.091-0.266 0.2-0.426 0.536-0.638 0.808-0.044-0.036-0.098-0.073-0.142-0.109 0.115-0.363 0.24-0.717 0.355-1.062-0.638-0.182-1.215-0.354-1.791-0.517 0.115-0.055 0.221-0.109 0.337-0.163 0.044-0.399 0.266-0.953 0.088-1.189-0.984-1.325-1.197 0.663-1.933 0.554 0.124-0.39 0.248-0.763 0.372-1.135-0.408 0.136-0.816 0.263-1.224 0.418-0.124 0.045-0.266 0.136-0.346 0.254-0.23 0.336-0.434 0.69-0.647 1.044 0.355 0.191 0.683 0.463 1.064 0.572 0.701 0.2 1.427 0.3 2.146 0.436-0.39 0.254-0.816 0.127-1.073 0.281-1.029 0.608-1.862 0.327-2.731-0.345-0.133-0.1-0.86 0.227-0.904 0.445-0.089 0.499 0.062 1.053 0.124 1.589 0.833-0.654 0.771 0.372 1.117 0.627 0.417 0.3 0.816 0.617 1.348 1.026 0.080-0.527 0.124-0.826 0.168-1.080 0.426 0.181 0.984 0.681 1.223 0.554zM12.847 18.082c0.168-0.218 0.346-0.436 0.567-0.717-0.186-0.109-0.355-0.218-0.532-0.3-1.995-0.944-2.766-2.769-2.057-4.893 0.035-0.1 0.115-0.227 0.195-0.254 1.8-0.545 3.6-1.089 5.408-1.616 0.709-0.209 1.303-0.518 1.41-1.643-2.722 0.508-5.355 0.953-7.962 1.507-0.842 0.182-0.931 1.008-0.904 1.761 0.018 0.499 0.106 0.999 0.204 1.498 0.355 1.906 1.117 3.504 3.094 4.158 0.213 0.054 0.381 0.318 0.576 0.499zM23.858 24.945c0.231-1.244 0.47-2.488 0.709-3.786 0.293 0.236 0.496 0.409 0.523 0.427 0.31-0.508 0.505-0.999 0.842-1.334 0.239-0.236 0.771-0.191 0.975-0.445 0.328-0.399 0.488-0.935 0.718-1.416-0.044-0.1-0.089-0.191-0.133-0.291-0.426 0.172-0.842 0.39-1.277 0.508-0.302 0.082-0.665 0.118-0.94 0.009-0.31-0.118-0.558-0.427-0.869-0.681 0.443-0.254 0.736-0.427 1.029-0.599 0.044-0.118 0.080-0.245 0.124-0.363-1.897 0.363-2.491-0.926-3.218-2.152-0.337-0.563-1.002-0.971-1.782-0.617 0.55 0.636 1.356 1.162 1.481 1.816 0.222 1.135 0.665 1.897 1.614 2.469 0.621 0.363 0.824 0.935 0.239 1.516-1.551 1.543-0.984 3.205-0.39 4.866 0.124 0.027 0.239 0.046 0.355 0.073zM34.533 20.442c0.035-0.163-0.612-0.49-0.975-0.699-0.842-0.481-1.711-0.935-2.562-1.398-0.709-0.381-1.401-0.781-2.136-1.117-0.133-0.064-0.39 0.145-0.594 0.236 0.089 0.172 0.133 0.409 0.275 0.508 0.426 0.318 0.904 0.563 1.321 0.89 0.23 0.182 0.479 0.445 0.567 0.717 0.576 1.888 0.567 1.888 2.367 1.707 0.142-0.018 0.284 0.018 0.346 0.027 0.824 0.2 1.268-0.227 1.392-0.872zM21.367 9.385c0.168-0.009 0.355-0.191 0.461-0.345 0.035-0.064-0.115-0.309-0.23-0.409-0.736-0.572-1.986-0.236-2.509 0.763 0.825 0.009 1.56 0.027 2.279-0.009z"></path>
      </symbol>
    </defs>
  </svg>
  <div class="content column">
    <div class="investigators-log">
      <InvestigatorRow v-for="investigator in Object.values(game.investigators)" :key="investigator.id" :investigator="investigator" :game="game" />
    </div>
    <div class="campaign-log column">
      <h1>Campaign Log: {{game.name}}</h1>
      <div v-if="emptyLog" class="box">
        No entries yet.
      </div>
      <div v-if="remembered.length > 0" class="remembered box">
        <h3 class="title">Remembered</h3>
        <ul>
          <li v-for="record in remembered" :key="record">{{record}}</li>
        </ul>
      </div>
      <div class="log-categories">
        <div v-if="logTitles" class="options">
          <template v-for="title in logTitles" :key="title">
            <input
              type="radio"
              v-model="campaignLog"
              :value="title === logTitle ? mainLog : otherLog"
              :checked="title === logTitle"
              :id="`log${title}`"
            />
            <label :for="`log${title}`">{{title}}</label>
          </template>
        </div>
        <div v-if="hasSupplies" class="supplies-container">
          <h2>Supplies</h2>
          <div class="supplies-content">
            <Supplies v-for="i in game.investigators" :key="i.id" :player="i">
              <template #heading>
                <h3>{{i.name.title}}</h3>
              </template>
            </Supplies>
          </div>
        </div>
        <div v-if="recorded.length > 0" class="box">
          Campaign Notes
          <ul>
            <li v-for="record in recorded" :key="record">{{t(record)}}</li>
            <template v-for="i in game.investigators" :key="i.id">
              <li v-for="record in i.log.recorded" :key="`${i.id}${record}`">{{fullName(i.name)}} {{t(formatKey(record))}}.</li>
            </template>
          </ul>
        </div>
        <ul>
          <li v-for="[setKey, setValues] in Object.entries(recordedSets)" :key="setKey">{{t(setKey)}}
            <ul :class="setClass(setKey)">
              <li v-if="isSeal(setKey)" v-for="setValue in setValues"><img :src="sealImage(setValue.contents)" class="seal"/></li>
              <li v-else v-for="setValue in setValues" :key="setValue" :class="{ 'crossed-out': setValue.tag === 'CrossedOut', 'circled': setValue.circled }">{{displayRecordValue(setKey, setValue)}}</li>
            </ul>
          </li>
        </ul>
        <ul>
          <li v-for="[key, value] in recordedCounts" :key="key">{{t(formatKey(key))}}: {{value}}.</li>
        </ul>
        <div v-if="Object.values(partners).length > 0" class="partners box">
          <h3 class="title">Expedition Team</h3>
          <div class="partners-content">
            <table>
              <thead>
                <tr>
                  <th></th>
                  <th>Damage</th>
                  <th>Horror</th>
                </tr>
              </thead>
              <tbody>
                <tr v-for="[cCode, partner] in Object.entries(partners)" :key="cCode" class="partner" :class="{ [partner.status]: true }">
                  <td v-if="partner.status === 'TheEntity'" class="partner-name"><span class="name"><s>{{cardCodeToTitle(cCode)}}</s></span><span class='name'>The Entity</span></td>
                  <td v-else class="partner-name"><span class="name">{{cardCodeToTitle(cCode)}}</span><span class="status-mia" v-if="partner.status === 'Mia'">MIA</span></td>
                  <td>{{partner.damage}}</td>
                  <td>{{partner.horror}}</td>
                </tr>
              </tbody>
            </table>
          </div>
        </div>
      </div>
    </div>

    <div v-for="([step, entries], idx) in breakdowns" :key="idx" class="breakdowns">
      <XpBreakdown :game="game" :step="step" :entries="entries" :playerId="playerId" />
    </div>
  </div>
</template>

<style lang="scss" scoped>
h1 {
  font-family: teutonic, sans-serif;
  margin: 0;
  padding: 0;
  color: var(--title);
  margin-bottom: 10px;
}

.campaign-log {
  width: 80%;
  margin-inline: auto;
  margin-block: 20px;
  font-size: 1.8em;
}

.breakdowns {
  width: 80%;
  margin: 0 auto;
}

.crossed-out {
  text-decoration: line-through;
}

.options {
  display: flex;
  justify-content: space-around;
}

.log-categories {
  display: flex;
  flex-direction: column;
  gap: 10px;
  margin: 0;
  padding: 0;
}

ul {
  display: flex;
  flex-direction: column;
  gap: 10px;
  margin: 0;
  padding: 0;
}

li {
  display: flex;
  flex-direction: column;
  gap: 10px;
  background: var(--box-background);
  border: 1px solid var(--box-border);
  border-radius: 5px;
  padding: 10px;
  color: var(--title);
  margin: 0;

  & ul li {
    background: rgba(255, 255, 255, 0.1);
  }
}

.box {
  & ul li {
    background: rgba(255, 255, 255, 0.1);
  }
}

.content {
  overflow: auto;
  width: 100%;
  padding-bottom: 50px;
}

.supplies-container {
  display: flex;
  flex-direction: column;
  color: var(--title);
}

.supplies-content {
  color: var(--title);
  display: flex;
  flex-direction: row;
  gap: 10px;
}

.circled {
  background: var(--rogue-dark);
}

h3.title {
  color: var(--title);
  font-family: teutonic, sans-serif;
  margin-bottom: 10px;
}

table {
  width: 100%;
  display: grid;
  grid-template-columns: repeat(3, auto);
  gap: 5px;

  thead, tbody, tr {
    display: contents;
  }
}

.partner td {
  background: var(--box-background);
  border: 1px solid var(--box-border);
  border-radius: 5px;
  padding: 10px;
  color: var(--title);
}

tr td:not(:first-child) {
  text-align: right;
}

.Eliminated td {
  text-decoration: line-through;
  background: darkred;
}

.Mia td {
  background: darkgoldenrod;
}

.partner-name {
  display: flex;
  gap: 10px;

  .name {
    flex: 1;
  }
}

.status-mia {
  background: rgba(0, 0, 0, 0.5);
  padding-inline: 5px;
  border-radius: 2px;
}

.seal {
  max-width: 45px;
}

.sealsPlaced, .sealsRecovered {
  display: flex;
  flex-direction: row;
  gap: 10px;
}

.investigators-log {
  display: flex;
  flex-direction: column;
  gap: 10px;
  place-items: center;
  margin: 20px;
}

.hidden {
  display: none;
}

</style>
