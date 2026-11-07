<script lang="ts" setup>
import { computed, onBeforeUnmount, onMounted, ref } from 'vue'
import { useI18n } from 'vue-i18n'
import type { Game } from '@/arkham/types/Game'
import type { Location } from '@/arkham/types/Location'
import { cardImage } from '@/arkham/cardImages'
import { replaceText } from '@/arkham/cardTextFormat'
import { useCardNames } from '@/arkham/composables/useCardNames'
import { ownInvestigator } from '@/arkham/ownInvestigator'
import type { MapLocationInfo } from '@/arkham/composables/useLocationInfo'
import AbilityButton from '@/arkham/components/AbilityButton.vue'

/* One location's face-up side in a corner of the map, out of the way of the figures: its numbers, text and
 * flavour, what is attached to it, where it leads, and the actions the server offers for it right now (only
 * when there are any). Every button sends the index its choice already had; looking never sends anything. */
const props = defineProps<{
  game: Game
  playerId: string
  location: Location
  info: MapLocationInfo
  /** The connected locations, by the name their face-up side shows. */
  connections: { id: string; name: string; revealed: boolean }[]
  /** Room kept free on the right for the actions panel (pixels). */
  rightGap: number
}>()
const emit = defineEmits<{ (e: 'choose', index: number): void; (e: 'inspect', id: string): void; (e: 'close'): void }>()

const { t } = useI18n()
const nameOf = useCardNames()
const card = ref<HTMLElement | null>(null)

const me = computed(() => ownInvestigator(props.game, props.playerId))
const textHtml = computed(() => props.info.face.text ? replaceText(props.info.face.text) : null)
const flavorHtml = computed(() => props.info.face.flavor ? replaceText(props.info.face.flavor) : null)

const named = (ids: string[], pool: Record<string, { cardCode: string }>) =>
  ids.flatMap((id) => pool[id] ? [{ id, name: nameOf(pool[id].cardCode) }] : [])
// Investigators and enemies stand on the map already; only what lies on the location itself is listed.
const attached = computed(() => [
  ...named(props.location.treacheries, props.game.treacheries),
  ...named(props.location.assets, props.game.assets),
  ...named(props.location.events, props.game.events),
])
const extraLabel = (x: string) => t(`locationMap.info.extra.${x.split(':')[0]}`, { value: x.split(':')[1] ?? '' })
const hasAction = computed(() => Boolean(props.info.actions.move || props.info.actions.investigate || props.info.actions.other.length || props.info.actions.selectIndex !== -1))

function choose(index: number) {
  emit('choose', index)
  emit('close')
}

const onKey = (event: KeyboardEvent) => { if (event.key === 'Escape') emit('close') }
onMounted(() => { window.addEventListener('keydown', onKey); card.value?.focus() })
onBeforeUnmount(() => window.removeEventListener('keydown', onKey))
import GameIcon from '@/arkham/components/GameIcon.vue'
</script>

<template>
  <aside ref="card" class="info-card" :class="{ unrevealed: !info.revealed }" :style="{ '--right-gap': rightGap + 'px' }"
    role="dialog" :aria-label="info.face.name" tabindex="-1" @click.stop>
    <header>
      <img class="thumb" :src="cardImage(location.cardCode, info.revealed ? '' : 'b')" alt="" loading="lazy" />
      <div class="title">
        <h3><span class="state" :title="info.revealed ? t('locationMap.marker.revealed') : t('locationMap.marker.unrevealed')">{{ info.revealed ? '●' : '?' }}</span>{{ info.face.name }}</h3>
        <p v-if="info.face.traits" class="traits">{{ info.face.traits }}</p>
      </div>
      <button class="close" type="button" :aria-label="t('locationMap.close')" @click="emit('close')">✕</button>
    </header>

    <section v-if="info.shroud || info.revealed" class="stats">
      <span v-if="info.shroud" :class="{ modified: info.shroud.current !== info.shroud.printed }"
        :title="t('locationMap.marker.shroud') + (info.shroud.current !== info.shroud.printed ? ' · ' + t('locationMap.marker.printed', { n: info.shroud.printed }) : '')">
        🔍 <b>{{ info.shroud.x ? 'X' : info.shroud.current }}</b></span>
      <span :title="t('locationMap.clues')"><GameIcon type="clue" /> <b>{{ info.clues.left }}<template v-if="info.clues.printed !== null">/{{ info.clues.printed }}</template></b></span>
      <span v-if="info.doom" class="doom" :title="t('locationMap.doom')"><GameIcon type="doom" /> <b>{{ info.doom }}</b></span>
      <span v-if="info.victory" :title="t('locationMap.info.victory')">VP <b>{{ info.victory }}</b></span>
      <span v-if="info.shroud && !info.shroud.x && me" class="compare" :title="t('locationMap.info.investigateCompare', { intellect: me.intellect, shroud: info.shroud.current })">
        <span class="intellect-icon" aria-hidden="true"></span> {{ me.intellect }} vs {{ info.shroud.current }}</span>
    </section>

    <!-- Card text comes from the bundled ArkhamDB card list, formatted the way the card overlay formats it. -->
    <p v-if="textHtml" class="text" v-html="textHtml"></p>
    <p v-if="flavorHtml" class="flavor" v-html="flavorHtml"></p>

    <ul v-if="attached.length || info.extras.length" class="attached">
      <li v-for="a in attached" :key="a.id">▣ {{ a.name }}</li>
      <li v-for="x in info.extras" :key="x">✧ {{ extraLabel(x) }}</li>
    </ul>

    <div v-if="connections.length" class="link-row" :aria-label="t('locationMap.info.connections')">
      <span class="arrow" aria-hidden="true">↔</span>
      <button v-for="c in connections" :key="c.id" type="button" class="link-chip" :class="{ hidden: !c.revealed }" @click="emit('inspect', c.id)">
        {{ c.revealed ? '●' : '?' }} {{ c.name }}</button>
    </div>

    <section v-if="hasAction" class="actions">
      <button v-if="info.actions.selectIndex !== -1" type="button" class="act primary" @click="choose(info.actions.selectIndex)">✦ {{ t('locationMap.info.chooseThis') }}</button>
      <button v-if="info.actions.move" type="button" class="act move" @click="choose(info.actions.move.index)">↔ {{ t('locationMap.marker.moveHere') }}</button>
      <button v-if="info.actions.investigate" type="button" class="act investigate" @click="choose(info.actions.investigate.index)">🔍 {{ t('locationMap.info.investigate') }}</button>
      <AbilityButton v-for="c in info.actions.other" :key="c.index" :game="game" :ability="c.contents" @choose="choose(c.index)" />
    </section>
  </aside>
</template>

<style scoped>
.info-card { position: absolute; z-index: 70; top: 12px; right: calc(var(--right-gap) + 12px); width: 290px; max-height: calc(100% - 24px); overflow-y: auto; display: grid; gap: 8px; align-content: start; padding: 10px; background: #101913f2; border: 1px solid #c6a873; border-radius: 8px; box-shadow: 0 8px 24px #000b; color: #e9e3d0; font-size: 0.8rem; }
.info-card.unrevealed { border-style: dashed; }
.info-card:focus { outline: none; }
header { display: grid; grid-template-columns: 44px 1fr auto; gap: 8px; align-items: start; }
.thumb { width: 44px; aspect-ratio: 5 / 7; object-fit: cover; border-radius: 3px; border: 1px solid #ffffff22; }
h3 { margin: 0; display: flex; gap: 6px; align-items: baseline; font: 600 1.05rem/1.15 var(--ui-serif); font-variant: small-caps; color: #fff3cf; }
.state { color: #c6a873; font-size: 0.8rem; }
.unrevealed .state { color: #d8c79f; }
.traits { margin: 2px 0 0; font-style: italic; color: #c9bf9f; }
.close { background: none; border: 0; color: #b4ad99; font-size: 1rem; cursor: pointer; padding: 2px 4px; }
.close:hover { color: #fff; }
.stats { display: flex; flex-wrap: wrap; gap: 4px 10px; padding: 5px 8px; background: #ffffff0d; border-radius: 6px; font-variant-numeric: tabular-nums; }
.stats b { color: #fff3cf; }
.stats .modified b { color: #f5b26b; }
.stats .doom b { color: #e0a595; }
.stats .compare { margin-left: auto; color: #9fd3e6; }
.text { margin: 0; line-height: 1.4; }
.flavor { margin: 0; font-style: italic; color: #c9bf9f; line-height: 1.4; }
.attached { margin: 0; padding: 0; list-style: none; display: flex; flex-wrap: wrap; gap: 4px 10px; color: #d8c79f; }
.link-row { display: flex; flex-wrap: wrap; gap: 4px; align-items: center; }
.arrow { color: #b9a678; }
.link-chip { padding: 1px 8px; border: 1px solid #c6a87388; border-radius: 10px; background: transparent; color: #e9e3d0; font: inherit; cursor: pointer; }
.link-chip.hidden { border-style: dashed; color: #c8c2b0; }
.link-chip:hover { border-color: #ffe9a0; }
.actions { display: grid; gap: 5px; }
.act { padding: 6px 10px; border: 1px solid #75836b; border-radius: 5px; background: #1d2a21; color: #e9e3d0; font: inherit; font-weight: 600; text-align: left; cursor: pointer; }
.act:hover { border-color: #e6c584; }
.act.primary { background: #c6a873; border-color: #c6a873; color: #191c16; }
.act.move { border-color: #9fc3a0; }
.act.investigate { border-color: #9fd3e6; }
button:focus-visible { outline: 2px solid #ffe9a0; outline-offset: 2px; }
@media (max-width: 800px) {
  .info-card { position: fixed; top: auto; left: 0; right: 0; bottom: 0; width: auto; max-height: 50dvh; border-radius: 12px 12px 0 0; z-index: 200; }
}
</style>
